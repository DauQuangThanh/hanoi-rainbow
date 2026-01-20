#!/usr/bin/env python3
"""
Extract structure from RPG/RPG IV/ILE source files.

This script analyzes RPG source code and extracts:
- Specification structure (H, F, D, C, P)
- Variable definitions (D-specs)
- File definitions (F-specs)
- Subroutine structure (BEGSR/ENDSR)
- Procedure definitions (P-specs)
- Program calls (CALLB/CALLP)
- Program dependencies (/COPY, /INCLUDE)

Usage:
    extract-structure.py <rpg_source_file> [--output output.json]

Example:
    extract-structure.py ORDPROC.rpgle --output structure.json
    extract-structure.py CUSTMAINT.rpg
"""

import argparse
import json
import re
from pathlib import Path
from typing import Dict, List, Any


class RPGStructureExtractor:
    """Extract structural information from RPG source programs."""
    
    def __init__(self, source_file: Path):
        self.source_file = source_file
        self.content = source_file.read_text(encoding='utf-8', errors='ignore')
        self.lines = self.content.split('\n')
        
    def extract(self) -> Dict[str, Any]:
        """Extract all structural information."""
        return {
            'program_name': self.extract_program_name(),
            'specifications': self.extract_specifications(),
            'data_structures': self.extract_data_structures(),
            'file_definitions': self.extract_file_definitions(),
            'subroutines': self.extract_subroutines(),
            'procedures': self.extract_procedures(),
            'calls': self.extract_calls(),
            'copy_members': self.extract_copy_members(),
            'statistics': self.calculate_statistics()
        }
    
    def extract_program_name(self) -> str:
        """Extract program name from file or H-spec."""
        # Try to find in H-spec comments or use filename
        for line in self.lines:
            if line.strip().startswith('H') or line.strip().startswith('h'):
                # Look for program name in comments
                match = re.search(r'(PROGRAM|PGM):\s*(\S+)', line, re.IGNORECASE)
                if match:
                    return match.group(2)
        return self.source_file.stem
    
    def extract_specifications(self) -> Dict[str, int]:
        """Find line numbers where specifications start."""
        specs = {'H': [], 'F': [], 'D': [], 'C': [], 'P': [], 'O': []}
        for i, line in enumerate(self.lines):
            if len(line) > 0:
                spec_type = line[0].upper()
                if spec_type in specs:
                    specs[spec_type].append(i + 1)
        return {k: v for k, v in specs.items() if v}
    
    def extract_data_structures(self) -> List[Dict[str, Any]]:
        """Extract D-spec data structures and standalone fields."""
        data_defs = []
        
        for i, line in enumerate(self.lines):
            if len(line) > 0 and line[0].upper() == 'D':
                # Parse D-spec format (fixed or free)
                name_match = re.match(r'^D\s+(\S+)', line, re.IGNORECASE)
                if name_match:
                    name = name_match.group(1)
                    
                    # Check for data structure
                    is_ds = 'DS' in line.upper()
                    
                    # Extract data type
                    type_match = re.search(r'\s+(CHAR|VARCHAR|INT|UNS|PACKED|ZONED|DATE|TIME|TIMESTAMP|LIKE|LIKEDS)\s*\(([^)]+)\)', line, re.IGNORECASE)
                    data_type = type_match.group(1) if type_match else None
                    length = type_match.group(2) if type_match else None
                    
                    data_defs.append({
                        'name': name,
                        'type': 'data_structure' if is_ds else 'field',
                        'data_type': data_type,
                        'length': length,
                        'line': i + 1
                    })
        
        return data_defs
    
    def extract_file_definitions(self) -> List[Dict[str, str]]:
        """Extract file definitions from F-specs."""
        files = []
        for i, line in enumerate(self.lines):
            if len(line) > 0 and line[0].upper() == 'F':
                # Parse F-spec format
                file_match = re.match(r'^F(\S+)', line, re.IGNORECASE)
                if file_match:
                    file_name = file_match.group(1)
                    
                    # Determine file type (I=Input, O=Output, U=Update, etc.)
                    file_type = 'UNKNOWN'
                    if 'DISK' in line.upper():
                        if ' O ' in line.upper() or ' OUTPUT ' in line.upper():
                            file_type = 'OUTPUT'
                        elif ' U ' in line.upper() or ' UPDATE ' in line.upper():
                            file_type = 'UPDATE'
                        else:
                            file_type = 'INPUT'
                    
                    files.append({
                        'name': file_name,
                        'type': file_type,
                        'line': i + 1
                    })
        return files
    
    def extract_subroutines(self) -> List[Dict[str, Any]]:
        """Extract subroutines (BEGSR/ENDSR blocks)."""
        subroutines = []
        current_sr = None
        
        for i, line in enumerate(self.lines):
            # Look for BEGSR
            begsr_match = re.search(r'BEGSR\s+(\S+)', line, re.IGNORECASE)
            if begsr_match:
                current_sr = {
                    'name': begsr_match.group(1),
                    'start_line': i + 1,
                    'end_line': None
                }
            
            # Look for ENDSR
            if 'ENDSR' in line.upper() and current_sr:
                current_sr['end_line'] = i + 1
                subroutines.append(current_sr)
                current_sr = None
        
        return subroutines
    
    def extract_procedures(self) -> List[Dict[str, Any]]:
        """Extract procedures (P-spec definitions)."""
        procedures = []
        current_proc = None
        
        for i, line in enumerate(self.lines):
            if len(line) > 0 and line[0].upper() == 'P':
                # Look for procedure begin/end
                proc_match = re.match(r'^P\s+(\S+)\s+(B|E)', line, re.IGNORECASE)
                if proc_match:
                    proc_name = proc_match.group(1)
                    proc_flag = proc_match.group(2).upper()
                    
                    if proc_flag == 'B':  # Begin
                        current_proc = {
                            'name': proc_name,
                            'start_line': i + 1,
                            'end_line': None
                        }
                    elif proc_flag == 'E' and current_proc:  # End
                        current_proc['end_line'] = i + 1
                        procedures.append(current_proc)
                        current_proc = None
        
        return procedures
    
    def extract_calls(self) -> List[Dict[str, Any]]:
        """Extract CALLB/CALLP statements to other programs."""
        calls = []
        for i, line in enumerate(self.lines):
            # Match CALLB or CALLP
            match = re.search(r'\s+(CALLB|CALLP)\s+[\'"]?(\S+)[\'"]?', line, re.IGNORECASE)
            if match:
                calls.append({
                    'type': match.group(1).upper(),
                    'program': match.group(2).strip('\'"'),
                    'line': i + 1
                })
        return calls
    
    def extract_copy_members(self) -> List[Dict[str, Any]]:
        """Extract /COPY and /INCLUDE statements."""
        copy_members = []
        for i, line in enumerate(self.lines):
            match = re.search(r'^\s*/(COPY|INCLUDE)\s+(\S+)', line, re.IGNORECASE)
            if match:
                copy_members.append({
                    'type': match.group(1).upper(),
                    'name': match.group(2),
                    'line': i + 1
                })
        return copy_members
    
    def extract_sql_operations(self) -> List[Dict[str, Any]]:
        """Extract embedded SQL operations."""
        sql_ops = []
        in_sql = False
        sql_text = []
        sql_start = 0
        
        for i, line in enumerate(self.lines):
            if 'EXEC SQL' in line.upper() or '/EXEC SQL' in line.upper():
                in_sql = True
                sql_start = i + 1
                sql_text = []
            
            if in_sql:
                sql_text.append(line.strip())
                
            if 'END-EXEC' in line.upper() or '/END-EXEC' in line.upper():
                if in_sql:
                    in_sql = False
                    sql_ops.append({
                        'statement': ' '.join(sql_text),
                        'line': sql_start
                    })
        
        return sql_ops
    
    def calculate_statistics(self) -> Dict[str, int]:
        """Calculate basic statistics."""
        return {
            'total_lines': len(self.lines),
            'data_definitions': len(self.extract_data_structures()),
            'file_definitions': len(self.extract_file_definitions()),
            'subroutines': len(self.extract_subroutines()),
            'procedures': len(self.extract_procedures()),
            'calls': len(self.extract_calls()),
            'copy_members': len(self.extract_copy_members()),
            'sql_operations': len(self.extract_sql_operations())
        }


def main():
    parser = argparse.ArgumentParser(description='Extract structure from legacy source files')
    parser.add_argument('source_file', type=Path, help='Path to source file')
    parser.add_argument('--output', '-o', type=Path, help='Output JSON file (default: stdout)')
    
    args = parser.parse_args()
    
    if not args.source_file.exists():
        print(f"Error: File not found: {args.source_file}")
        return 1
    
    extractor = RPGStructureExtractor(args.source_file)
    structure = extractor.extract()
    
    output_json = json.dumps(structure, indent=2)
    
    if args.output:
        args.output.write_text(output_json)
        print(f"Structure written to {args.output}")
    else:
        print(output_json)
    
    return 0


if __name__ == '__main__':
    exit(main())
