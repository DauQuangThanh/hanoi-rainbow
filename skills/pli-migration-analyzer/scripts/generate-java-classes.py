#!/usr/bin/env python3
"""
Generate Java POJO classes from legacy data structures.

This script parses legacy data structure definitions and generates corresponding
Java classes with appropriate data types, getters, setters, and validation.
"""

import argparse
import re
from pathlib import Path
from typing import List, Dict, Optional, Tuple


class FieldDefinition:
    """Represents a PL/I field definition."""
    
    def __init__(self, level: int, name: str, picture: Optional[str], occurs: Optional[int] = None):
        self.level = level
        self.name = name
        self.picture = picture
        self.occurs = occurs
        self.children: List[FieldDefinition] = []
    
    def is_group(self) -> bool:
        """Check if this is a group item (no picture clause)."""
        return self.picture is None
    
    def to_java_type(self) -> str:
        """Convert PL/I picture to Java type."""
        if self.picture is None:
            # Group item - will be a nested class
            return self.to_java_class_name()
        
        pic = self.picture.upper()
        
        # Numeric types
        if re.match(r'^S?9+V?9*$', pic.replace('(', '').replace(')', '')):
            if 'V' in pic or 'COMP-3' in pic:
                return 'BigDecimal'
            digit_count = pic.count('9')
            if digit_count <= 9:
                return 'int' if pic.startswith('S') or pic.startswith('9') else 'long'
            elif digit_count <= 18:
                return 'long'
            else:
                return 'BigInteger'
        
        # Alphanumeric
        if pic.startswith('X'):
            return 'String'
        
        # Default to String for unknown patterns
        return 'String'
    
    def to_java_field_name(self) -> str:
        """Convert PL/I name to Java field name (camelCase)."""
        parts = self.name.lower().replace('_', '-').split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])
    
    def to_java_class_name(self) -> str:
        """Convert PL/I name to Java class name (PascalCase)."""
        parts = self.name.lower().replace('_', '-').split('-')
        return ''.join(p.capitalize() for p in parts)


class PLIStructureParser:
    """Parse PL/I data structures and extract field definitions."""
    
    def __init__(self, pli_file: Path):
        self.pli_file = pli_file
        self.content = pli_file.read_text(encoding='utf-8', errors='ignore')
        self.lines = self.content.split('\n')
        
    def parse(self) -> FieldDefinition:
        """Parse PL/I DECLARE statements and return root field definition."""
        fields = []
        
        for line in self.lines:
            # Match PL/I DECLARE statements with level numbers (for structures)
            # Pattern: DCL 01 name ...
            level_match = re.match(r'\s*(?:DCL|DECLARE)\s+(\d{2})\s+(\w+)(?:\s+(FIXED\s+(?:DECIMAL|BINARY)|CHARACTER|CHAR|BIT))?(?:\s*\(([^)]+)\))?', line, re.IGNORECASE)
            if level_match:
                level = int(level_match.group(1))
                name = level_match.group(2)
                data_type = level_match.group(3) if level_match.group(3) else None
                attributes = level_match.group(4) if level_match.group(4) else None
                
                # Convert PL/I type to picture-like format for compatibility
                picture = self._pli_type_to_picture(data_type, attributes) if data_type else None
                
                field = FieldDefinition(level, name, picture, None)
                fields.append(field)
        
        # Build hierarchy
        return self._build_hierarchy(fields)
    
    def _pli_type_to_picture(self, data_type: str, attributes: str) -> str:
        """Convert PL/I data type to picture format."""
        if not data_type:
            return None
        
        data_type_upper = data_type.upper()
        if 'FIXED DECIMAL' in data_type_upper or 'FIXED DEC' in data_type_upper:
            # FIXED DECIMAL(p,q) -> 9(p)V9(q) equivalent
            return f"9({attributes})" if attributes else "9(15)"
        elif 'FIXED BINARY' in data_type_upper or 'FIXED BIN' in data_type_upper:
            return "9(9)"  # Integer equivalent
        elif 'CHARACTER' in data_type_upper or 'CHAR' in data_type_upper:
            return f"X({attributes})" if attributes else "X(1)"
        elif 'BIT' in data_type_upper:
            return "9(1)"  # Boolean equivalent
        return None
    
    def _build_hierarchy(self, fields: List[FieldDefinition]) -> FieldDefinition:
        """Build hierarchical structure from flat field list."""
        if not fields:
            return FieldDefinition(1, 'Root', None)
        
        root = fields[0]
        stack = [root]
        
        for field in fields[1:]:
            # Pop stack until we find the parent level
            while stack and stack[-1].level >= field.level:
                stack.pop()
            
            if stack:
                stack[-1].children.append(field)
            
            stack.append(field)
        
        return root


class JavaClassGenerator:
    """Generate Java class from PL/I data structure."""
    
    def __init__(self, root_field: FieldDefinition, package_name: str = 'com.example.model'):
        self.root = root_field
        self.package_name = package_name
        self.imports = set()
        
    def generate(self) -> str:
        """Generate complete Java class."""
        self._collect_imports(self.root)
        
        lines = []
        lines.append(f'package {self.package_name};')
        lines.append('')
        
        # Add imports
        if self.imports:
            for imp in sorted(self.imports):
                lines.append(f'import {imp};')
            lines.append('')
        
        # Generate class
        lines.append('/**')
        lines.append(f' * Generated from PL/I data structure: {self.root.name}')
        lines.append(' * Auto-generated - do not modify directly')
        lines.append(' */')
        lines.extend(self._generate_class(self.root, 0))
        
        return '\n'.join(lines)
    
    def _collect_imports(self, field: FieldDefinition):
        """Collect required imports."""
        java_type = field.to_java_type()
        
        if java_type == 'BigDecimal':
            self.imports.add('java.math.BigDecimal')
        elif java_type == 'BigInteger':
            self.imports.add('java.math.BigInteger')
        elif field.occurs:
            self.imports.add('java.util.List')
            self.imports.add('java.util.ArrayList')
        
        for child in field.children:
            self._collect_imports(child)
    
    def _generate_class(self, field: FieldDefinition, indent_level: int) -> List[str]:
        """Generate class definition recursively."""
        lines = []
        indent = '    ' * indent_level
        
        class_name = field.to_java_class_name()
        
        lines.append(f'{indent}public class {class_name} {{')
        
        # Generate fields
        for child in field.children:
            field_indent = '    ' * (indent_level + 1)
            java_type = child.to_java_type()
            field_name = child.to_java_field_name()
            
            if child.occurs:
                lines.append(f'{field_indent}private List<{java_type}> {field_name} = new ArrayList<>();')
            else:
                lines.append(f'{field_indent}private {java_type} {field_name};')
        
        if field.children:
            lines.append('')
        
        # Generate getters and setters
        for child in field.children:
            field_indent = '    ' * (indent_level + 1)
            java_type = child.to_java_type()
            field_name = child.to_java_field_name()
            method_suffix = field_name[0].upper() + field_name[1:]
            
            if child.occurs:
                return_type = f'List<{java_type}>'
                lines.append(f'{field_indent}public {return_type} get{method_suffix}() {{')
                lines.append(f'{field_indent}    return {field_name};')
                lines.append(f'{field_indent}}}')
                lines.append('')
                lines.append(f'{field_indent}public void set{method_suffix}({return_type} {field_name}) {{')
                lines.append(f'{field_indent}    this.{field_name} = {field_name};')
                lines.append(f'{field_indent}}}')
            else:
                lines.append(f'{field_indent}public {java_type} get{method_suffix}() {{')
                lines.append(f'{field_indent}    return {field_name};')
                lines.append(f'{field_indent}}}')
                lines.append('')
                lines.append(f'{field_indent}public void set{method_suffix}({java_type} {field_name}) {{')
                lines.append(f'{field_indent}    this.{field_name} = {field_name};')
                lines.append(f'{field_indent}}}')
            lines.append('')
        
        # Generate nested classes for group items
        for child in field.children:
            if child.is_group() and child.children:
                lines.extend(self._generate_class(child, indent_level + 1))
                lines.append('')
        
        lines.append(f'{indent}}}')
        
        return lines


def main():
    parser = argparse.ArgumentParser(description='Generate Java classes from PL/I data structures')
    parser.add_argument('pli_file', type=Path, help='Path to PL/I source file with DECLARE statements')
    parser.add_argument('--package', '-p', default='com.example.model', help='Java package name')
    parser.add_argument('--output-dir', '-o', type=Path, help='Output directory for Java files')
    
    args = parser.parse_args()
    
    if not args.pli_file.exists():
        print(f"Error: PL/I file not found: {args.pli_file}")
        return 1
    
    # Parse PL/I structure
    parser_obj = PLIStructureParser(args.pli_file)
    root_field = parser_obj.parse()
    
    # Generate Java class
    generator = JavaClassGenerator(root_field, args.package)
    java_code = generator.generate()
    
    # Write output
    if args.output_dir:
        args.output_dir.mkdir(parents=True, exist_ok=True)
        # Convert package name to path (e.g., com.example.model -> com/example/model)
        package_parts = args.package.split('.')
        package_path = args.output_dir.joinpath(*package_parts)
        package_path.mkdir(parents=True, exist_ok=True)
        
        output_file = package_path / f'{root_field.to_java_class_name()}.java'
        output_file.write_text(java_code)
        print(f"Generated: {output_file}")
    else:
        print(java_code)
    
    return 0


if __name__ == '__main__':
    exit(main())
