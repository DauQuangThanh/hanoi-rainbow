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
    
    def extract_divisions(self) -> Dict[str, int]:
        """Find line numbers where divisions start."""
        divisions = {}
        for i, line in enumerate(self.lines):
            for div_name in ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE']:
                if re.search(rf'{div_name}\s+DIVISION', line, re.IGNORECASE):
                    divisions[div_name] = i + 1
        return divisions
    
    def extract_working_storage(self) -> List[Dict[str, Any]]:
        """Extract Working-Storage variables."""
        variables = []
        in_working_storage = False
        
        for line in self.lines:
            if re.search(r'WORKING-STORAGE\s+SECTION', line, re.IGNORECASE):
                in_working_storage = True
                continue
            if re.search(r'(PROCEDURE\s+DIVISION|LINKAGE\s+SECTION)', line, re.IGNORECASE):
                in_working_storage = False
                
            if in_working_storage:
                match = re.match(r'\s*(\d{2})\s+(\S+)\s+(.+)', line)
                if match:
                    level, name, rest = match.groups()
                    pic_match = re.search(r'PIC\s+(\S+)', rest, re.IGNORECASE)
                    variables.append({
                        'level': int(level),
                        'name': name,
                        'picture': pic_match.group(1) if pic_match else None,
                        'line': self.lines.index(line) + 1
                    })
        
        return variables
    
    def extract_file_definitions(self) -> List[Dict[str, str]]:
        """Extract file definitions from SELECT statements."""
        files = []
        for line in self.lines:
            match = re.search(r'SELECT\s+(\S+)\s+ASSIGN', line, re.IGNORECASE)
            if match:
                files.append({
                    'name': match.group(1),
                    'line': self.lines.index(line) + 1
                })
        return files
    
    def extract_paragraphs(self) -> List[Dict[str, Any]]:
        """Extract paragraph and section names."""
        paragraphs = []
        for i, line in enumerate(self.lines):
            # Match paragraph names (word followed by period at start of line)
            match = re.match(r'^([A-Z0-9\-]+)\.\s*$', line.strip())
            if match and i > 0:
                para_name = match.group(1)
                # Skip division names
                if para_name not in ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE']:
                    paragraphs.append({
                        'name': para_name,
                        'line': i + 1,
                        'type': 'section' if 'SECTION' in para_name else 'paragraph'
                    })
        return paragraphs
    
    def extract_calls(self) -> List[Dict[str, Any]]:
        """Extract CALL statements to other programs."""
        calls = []
        for i, line in enumerate(self.lines):
            match = re.search(r'CALL\s+[\'"](\S+)[\'"]', line, re.IGNORECASE)
            if match:
                calls.append({
                    'program': match.group(1),
                    'line': i + 1
                })
        return calls
    
    def extract_copybooks(self) -> List[Dict[str, Any]]:
        """Extract COPY statements."""
        copybooks = []
        for i, line in enumerate(self.lines):
            match = re.search(r'COPY\s+(\S+)', line, re.IGNORECASE)
            if match:
                copybooks.append({
                    'name': match.group(1).rstrip('.'),
                    'line': i + 1
                })
        return copybooks
    
    def extract_sql_operations(self) -> List[Dict[str, Any]]:
        """Extract embedded SQL operations."""
        sql_ops = []
        in_sql = False
        sql_text = []
        sql_start = 0
        
        for i, line in enumerate(self.lines):
            if 'EXEC SQL' in line.upper():
                in_sql = True
                sql_start = i + 1
                sql_text = []
            
            if in_sql:
                sql_text.append(line.strip())
                
            if 'END-EXEC' in line.upper() and in_sql:
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
            'working_storage_vars': len(self.extract_working_storage()),
            'paragraphs': len(self.extract_paragraphs()),
            'calls': len(self.extract_calls()),
            'copybooks': len(self.extract_copybooks()),
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
    
    extractor = LegacyStructureExtractor(args.source_file)
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
