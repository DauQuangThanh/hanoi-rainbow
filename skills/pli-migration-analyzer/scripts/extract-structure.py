#!/usr/bin/env python3
"""
Extract structure from PL/I source files.

This script analyzes PL/I source code and extracts:
- Procedure structure
- Variable declarations (DECLARE)
- File definitions (OPEN/CLOSE)
- Subroutine structure
- Program calls (CALL)
- Program dependencies
"""

import argparse
import json
import re
from pathlib import Path
from typing import Dict, List, Any


class PLIStructureExtractor:
    """Extract structural information from PL/I source programs."""
    
    def __init__(self, source_file: Path):
        self.source_file = source_file
        self.content = source_file.read_text(encoding='utf-8', errors='ignore')
        self.lines = self.content.split('\n')
        
    def extract(self) -> Dict[str, Any]:
        """Extract all structural information."""
        return {
            'program_name': self.extract_program_name(),
            'procedures': self.extract_procedures(),
            'declarations': self.extract_declarations(),
            'file_definitions': self.extract_file_definitions(),
            'calls': self.extract_calls(),
            'includes': self.extract_includes(),
            'on_conditions': self.extract_on_conditions(),
            'sql_operations': self.extract_sql_operations(),
            'statistics': self.calculate_statistics()
        }
    
    def extract_program_name(self) -> str:
        """Extract program name from file or PROCEDURE OPTIONS(MAIN)."""
        for line in self.lines:
            # Look for main procedure definition
            match = re.search(r'(\w+):\s*PROC(?:EDURE)?\s+OPTIONS\s*\(\s*MAIN\s*\)', line, re.IGNORECASE)
            if match:
                return match.group(1)
        return self.source_file.stem
    
    def extract_procedures(self) -> List[Dict[str, Any]]:
        """Extract procedure definitions."""
        procedures = []
        current_proc = None
        
        for i, line in enumerate(self.lines):
            # Look for PROC or PROCEDURE statements
            proc_match = re.search(r'(\w+):\s*PROC(?:EDURE)?', line, re.IGNORECASE)
            if proc_match:
                if current_proc:
                    procedures.append(current_proc)
                current_proc = {
                    'name': proc_match.group(1),
                    'start_line': i + 1,
                    'end_line': None,
                    'is_main': 'OPTIONS' in line.upper() and 'MAIN' in line.upper(),
                    'returns': None
                }
                # Check for RETURNS clause
                returns_match = re.search(r'RETURNS\s*\(\s*([^)]+)\s*\)', line, re.IGNORECASE)
                if returns_match:
                    current_proc['returns'] = returns_match.group(1).strip()
            
            # Look for END statement
            if current_proc and re.search(rf'\bEND\s+{re.escape(current_proc["name"])}\b', line, re.IGNORECASE):
                current_proc['end_line'] = i + 1
                procedures.append(current_proc)
                current_proc = None
        
        if current_proc:
            current_proc['end_line'] = len(self.lines)
            procedures.append(current_proc)
        
        return procedures
    
    def extract_declarations(self) -> List[Dict[str, Any]]:
        """Extract DECLARE statements."""
        declarations = []
        
        for i, line in enumerate(self.lines):
            # Look for DCL or DECLARE statements
            if re.search(r'\b(DCL|DECLARE)\b', line, re.IGNORECASE):
                # Extract variable names and types
                # Match patterns like: DCL var_name type(size)
                var_matches = re.finditer(r'(\w+)\s+(FIXED\s+(?:DECIMAL|BINARY)|CHARACTER|CHAR|BIT|POINTER|ENTRY|FILE)(?:\s*\(([^)]+)\))?', 
                                         line, re.IGNORECASE)
                for match in var_matches:
                    var_name = match.group(1)
                    var_type = match.group(2)
                    var_attrs = match.group(3) if match.group(3) else None
                    
                    declarations.append({
                        'name': var_name,
                        'type': var_type.upper(),
                        'attributes': var_attrs,
                        'line': i + 1
                    })
        
        return declarations
    
    def extract_file_definitions(self) -> List[Dict[str, str]]:
        """Extract file definitions from FILE attribute and OPEN statements."""
        files = []
        file_set = set()
        
        for i, line in enumerate(self.lines):
            # Look for FILE attribute in DECLARE
            file_match = re.search(r'FILE\s+(\w+)', line, re.IGNORECASE)
            if file_match and file_match.group(1) not in file_set:
                file_name = file_match.group(1)
                file_set.add(file_name)
                files.append({
                    'name': file_name,
                    'line': i + 1,
                    'type': 'declared'
                })
            
            # Look for OPEN FILE statements
            open_match = re.search(r'OPEN\s+FILE\s*\(\s*(\w+)\s*\)', line, re.IGNORECASE)
            if open_match and open_match.group(1) not in file_set:
                file_name = open_match.group(1)
                file_set.add(file_name)
                files.append({
                    'name': file_name,
                    'line': i + 1,
                    'type': 'opened'
                })
        
        return files
    
    def extract_calls(self) -> List[Dict[str, Any]]:
        """Extract CALL statements to other programs."""
        calls = []
        for i, line in enumerate(self.lines):
            # Match CALL statements
            match = re.search(r'CALL\s+(\w+)', line, re.IGNORECASE)
            if match:
                calls.append({
                    'program': match.group(1),
                    'line': i + 1
                })
        return calls
    
    def extract_includes(self) -> List[Dict[str, Any]]:
        """Extract %INCLUDE directives."""
        includes = []
        for i, line in enumerate(self.lines):
            match = re.search(r'%INCLUDE\s+([^\s;]+)', line, re.IGNORECASE)
            if match:
                includes.append({
                    'name': match.group(1).strip('\'"'),
                    'line': i + 1
                })
        return includes
    
    def extract_on_conditions(self) -> List[Dict[str, Any]]:
        """Extract ON condition handlers."""
        on_conditions = []
        for i, line in enumerate(self.lines):
            # Match ON ENDFILE, ON ERROR, etc.
            match = re.search(r'ON\s+(ENDFILE|ERROR|CONVERSION|OVERFLOW|UNDERFLOW|ZERODIVIDE|FIXEDOVERFLOW|SIZE|SUBSCRIPTRANGE)', 
                            line, re.IGNORECASE)
            if match:
                on_conditions.append({
                    'condition': match.group(1).upper(),
                    'line': i + 1
                })
        return on_conditions
    
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
                
            if 'END-EXEC' in line.upper() or ';' in line:
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
            'procedures': len(self.extract_procedures()),
            'declarations': len(self.extract_declarations()),
            'file_definitions': len(self.extract_file_definitions()),
            'calls': len(self.extract_calls()),
            'includes': len(self.extract_includes()),
            'on_conditions': len(self.extract_on_conditions()),
            'sql_operations': len(self.extract_sql_operations())
        }


def main():
    parser = argparse.ArgumentParser(description='Extract structure from PL/I source files')
    parser.add_argument('source_file', type=Path, help='Path to PL/I source file')
    parser.add_argument('--output', '-o', type=Path, help='Output JSON file (default: stdout)')
    
    args = parser.parse_args()
    
    if not args.source_file.exists():
        print(f"Error: File not found: {args.source_file}")
        return 1
    
    extractor = PLIStructureExtractor(args.source_file)
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
