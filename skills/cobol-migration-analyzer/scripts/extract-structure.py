#!/usr/bin/env python3
"""
Extract structure from COBOL source files.

This script analyzes COBOL source code and extracts:
- Division structure
- Working-Storage variables
- File definitions
- Paragraph/Section structure
- CALL statements
- Program dependencies
"""

import argparse
import json
import re
from pathlib import Path
from typing import Dict, List, Any


class COBOLStructureExtractor:
    """Extract structural information from COBOL programs."""
    
    def __init__(self, source_file: Path):
        self.source_file = source_file
        self.content = source_file.read_text(encoding='utf-8', errors='ignore')
        self.lines = self.content.split('\n')
        
    def extract(self) -> Dict[str, Any]:
        """Extract all structural information."""
        return {
            'program_name': self.extract_program_name(),
            'divisions': self.extract_divisions(),
            'working_storage': self.extract_working_storage(),
            'file_definitions': self.extract_file_definitions(),
            'paragraphs': self.extract_paragraphs(),
            'calls': self.extract_calls(),
            'copybooks': self.extract_copybooks(),
            'sql_operations': self.extract_sql_operations(),
            'statistics': self.calculate_statistics()
        }
    
    def extract_program_name(self) -> str:
        """Extract program name from PROGRAM-ID."""
        for line in self.lines:
            match = re.search(r'PROGRAM-ID\.\s+(\S+)', line, re.IGNORECASE)
            if match:
                return match.group(1).rstrip('.')
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
    parser = argparse.ArgumentParser(description='Extract structure from COBOL source files')
    parser.add_argument('cobol_file', type=Path, help='Path to COBOL source file')
    parser.add_argument('--output', '-o', type=Path, help='Output JSON file (default: stdout)')
    
    args = parser.parse_args()
    
    if not args.cobol_file.exists():
        print(f"Error: File not found: {args.cobol_file}")
        return 1
    
    extractor = COBOLStructureExtractor(args.cobol_file)
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
