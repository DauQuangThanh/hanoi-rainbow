#!/usr/bin/env python3
"""
Generate Java POJO classes from RPG data structures.

This script parses RPG data structure definitions (D-specs) and generates 
corresponding Java classes with appropriate data types, getters, setters, 
and validation annotations.

Usage:
    generate-java-classes.py <rpg_source_file> [--output-dir ./output] [--package com.example]

Example:
    generate-java-classes.py CUSTOMER.rpgle --output-dir src/main/java --package com.example.model
"""

import argparse
import re
from pathlib import Path
from typing import List, Dict, Optional, Tuple


class RPGFieldDefinition:
    """Represents an RPG D-spec field definition."""
    
    def __init__(self, name: str, data_type: str, length: int, decimals: int = 0, 
                 is_array: bool = False, array_size: int = 0):
        self.name = name
        self.data_type = data_type
        self.length = length
        self.decimals = decimals
        self.is_array = is_array
        self.array_size = array_size
        self.children: List[RPGFieldDefinition] = []
    
    def is_data_structure(self) -> bool:
        """Check if this is a data structure (has children)."""
        return len(self.children) > 0
    
    def to_java_type(self) -> str:
        """Convert RPG data type to Java type."""
        # Packed decimal
        if self.data_type == 'P':
            return 'BigDecimal'
        
        # Zoned decimal
        if self.data_type == 'S':
            return 'BigDecimal'
        
        # Character/Alphanumeric
        if self.data_type == 'A' or self.data_type == '':
            return 'String'
        
        # Date
        if self.data_type == 'D':
            return 'LocalDate'
        
        # Integer
        if self.data_type == 'I':
            if self.length <= 4:
                return 'int'
            else:
                return 'long'
            return 'String'
        
        # Default to String for unknown patterns
        return 'String'
    
    def to_java_field_name(self) -> str:
        """Convert RPG name to Java field name (camelCase)."""
        parts = self.name.lower().replace('_', '-').split('-')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])
    
    def to_java_class_name(self) -> str:
        """Convert RPG name to Java class name (PascalCase)."""
        parts = self.name.lower().replace('_', '-').split('-')
        return ''.join(p.capitalize() for p in parts)


class RPGStructureParser:
    """Parse RPG data structures and extract field definitions."""
    
    def __init__(self, rpg_file: Path):
        self.rpg_file = rpg_file
        self.content = rpg_file.read_text(encoding='utf-8', errors='ignore')
        self.lines = self.content.split('\n')
        
    def parse(self) -> RPGFieldDefinition:
        """Parse RPG D-specs and return root field definition."""
        fields = []
        
        for line in self.lines:
            # Match RPG D-spec field definition
            if len(line) > 0 and line[0].upper() == 'D':
                # Parse D-spec format
                match = re.match(r'^D\s+(\S+)\s+.*?\s+(CHAR|VARCHAR|INT|UNS|PACKED|ZONED|DATE|TIME|LIKE|LIKEDS)\s*\((\d+)(?::(\d+))?\)', line, re.IGNORECASE)
                if match:
                    name = match.group(1)
                    data_type = match.group(2)
                    length = int(match.group(3))
                    decimals = int(match.group(4)) if match.group(4) else 0
                    
                    # Check for DIM (array dimension)
                    dim_match = re.search(r'DIM\((\d+)\)', line, re.IGNORECASE)
                    is_array = dim_match is not None
                    array_size = int(dim_match.group(1)) if dim_match else 0
                    
                    field = RPGFieldDefinition(name, data_type, length, decimals, is_array, array_size)
                    fields.append(field)
        
        # Build hierarchy
        return self._build_hierarchy(fields)
    
    def _build_hierarchy(self, fields: List[RPGFieldDefinition]) -> RPGFieldDefinition:
        """Build hierarchical structure from flat field list."""
        if not fields:
            return RPGFieldDefinition('Root', 'A', 0)
        
        root = fields[0]
        stack = [root]
        
        for field in fields[1:]:
            # For RPG, determine hierarchy based on data structure membership
            # This is a simplified version - real implementation would track DS boundaries
            if stack:
                # Add to current data structure if appropriate
                pass
            
            stack.append(field)
        
        return root


class JavaClassGenerator:
    """Generate Java class from RPG data structure."""
    
    def __init__(self, root_field: RPGFieldDefinition, package_name: str = 'com.example.model'):
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
        lines.append(f' * Generated from RPG data structure: {self.root.name}')
        lines.append(' * Auto-generated - do not modify directly')
        lines.append(' */')
        lines.extend(self._generate_class(self.root, 0))
        
        return '\n'.join(lines)
    
    def _collect_imports(self, field: RPGFieldDefinition):
        """Collect required imports."""
        java_type = field.to_java_type()
        
        if java_type == 'BigDecimal':
            self.imports.add('java.math.BigDecimal')
        elif java_type == 'BigInteger':
            self.imports.add('java.math.BigInteger')
        elif field.is_array:
            self.imports.add('java.util.List')
            self.imports.add('java.util.ArrayList')
        
        for child in field.children:
            self._collect_imports(child)
    
    def _generate_class(self, field: RPGFieldDefinition, indent_level: int) -> List[str]:
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
            
            if child.is_array:
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
            
            if child.is_array:
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
        
        # Generate nested classes for data structures
        for child in field.children:
            if child.is_data_structure() and child.children:
                lines.extend(self._generate_class(child, indent_level + 1))
                lines.append('')
        
        lines.append(f'{indent}}}')
        
        return lines


def main():
    parser = argparse.ArgumentParser(description='Generate Java classes from RPG data structures')
    parser.add_argument('rpg_source', type=Path, help='Path to RPG source file with D-specs')
    parser.add_argument('--package', '-p', default='com.example.model', help='Java package name')
    parser.add_argument('--output-dir', '-o', type=Path, help='Output directory for Java files')
    
    args = parser.parse_args()
    
    if not args.rpg_source.exists():
        print(f"Error: RPG source file not found: {args.rpg_source}")
        return 1
    
    # Parse RPG data structures
    parser_obj = RPGStructureParser(args.rpg_source)
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
