#!/usr/bin/env python3
"""
Estimate migration complexity for COBOL programs.

Calculates a complexity score based on various factors:
- Lines of code
- Number of paragraphs
- Control flow complexity
- External dependencies
- File operations
- SQL operations
"""

import argparse
import json
import re
from pathlib import Path
from typing import Dict, Any


class ComplexityEstimator:
    """Estimate migration complexity for COBOL programs."""
    
    # Complexity weights
    WEIGHTS = {
        'loc_per_point': 100,      # Lines per complexity point
        'paragraph_weight': 2,      # Each paragraph adds complexity
        'call_weight': 5,           # Each external call
        'file_weight': 3,           # Each file operation
        'sql_weight': 4,            # Each SQL statement
        'goto_weight': 10,          # GO TO statements
        'alter_weight': 15,         # ALTER statements (very complex)
        'perform_varying_weight': 3, # Complex loops
    }
    
    def __init__(self, cobol_file: Path):
        self.cobol_file = cobol_file
        self.content = cobol_file.read_text(encoding='utf-8', errors='ignore')
        self.lines = [line for line in self.content.split('\n') if line.strip() and not line.strip().startswith('*')]
        
    def estimate(self) -> Dict[str, Any]:
        """Calculate complexity estimate."""
        metrics = self._collect_metrics()
        score = self._calculate_score(metrics)
        
        return {
            'program': self.cobol_file.name,
            'metrics': metrics,
            'complexity_score': score,
            'complexity_level': self._get_complexity_level(score),
            'estimated_effort_days': self._estimate_effort(score),
            'risk_factors': self._identify_risks(metrics)
        }
    
    def _collect_metrics(self) -> Dict[str, int]:
        """Collect various code metrics."""
        return {
            'total_lines': len(self.lines),
            'paragraphs': self._count_paragraphs(),
            'calls': self._count_pattern(r'CALL\s+'),
            'files': self._count_pattern(r'SELECT\s+'),
            'sql_statements': self._count_pattern(r'EXEC\s+SQL'),
            'goto_statements': self._count_pattern(r'GO\s+TO\s+'),
            'alter_statements': self._count_pattern(r'ALTER\s+'),
            'perform_varying': self._count_pattern(r'PERFORM\s+.*VARYING'),
            'copybooks': self._count_pattern(r'COPY\s+'),
            'if_statements': self._count_pattern(r'IF\s+'),
            'evaluate_statements': self._count_pattern(r'EVALUATE\s+'),
        }
    
    def _count_paragraphs(self) -> int:
        """Count paragraph definitions."""
        count = 0
        for line in self.lines:
            # Paragraph name followed by period at start of line
            if re.match(r'^[A-Z0-9\-]+\.\s*$', line.strip()):
                count += 1
        return count
    
    def _count_pattern(self, pattern: str) -> int:
        """Count occurrences of a regex pattern."""
        count = 0
        for line in self.lines:
            if re.search(pattern, line, re.IGNORECASE):
                count += 1
        return count
    
    def _calculate_score(self, metrics: Dict[str, int]) -> int:
        """Calculate overall complexity score."""
        score = 0
        
        # Base score from lines of code
        score += metrics['total_lines'] / self.WEIGHTS['loc_per_point']
        
        # Add weighted factors
        score += metrics['paragraphs'] * self.WEIGHTS['paragraph_weight']
        score += metrics['calls'] * self.WEIGHTS['call_weight']
        score += metrics['files'] * self.WEIGHTS['file_weight']
        score += metrics['sql_statements'] * self.WEIGHTS['sql_weight']
        score += metrics['goto_statements'] * self.WEIGHTS['goto_weight']
        score += metrics['alter_statements'] * self.WEIGHTS['alter_weight']
        score += metrics['perform_varying'] * self.WEIGHTS['perform_varying_weight']
        
        return int(score)
    
    def _get_complexity_level(self, score: int) -> str:
        """Categorize complexity level."""
        if score < 20:
            return 'Simple'
        elif score < 50:
            return 'Moderate'
        elif score < 100:
            return 'Complex'
        else:
            return 'Very Complex'
    
    def _estimate_effort(self, score: int) -> float:
        """Estimate effort in person-days."""
        # Rough estimate: 1 point = 0.5 days
        # This should be calibrated based on team experience
        base_days = score * 0.5
        
        # Add overhead for testing and documentation (30%)
        return round(base_days * 1.3, 1)
    
    def _identify_risks(self, metrics: Dict[str, int]) -> list:
        """Identify risk factors."""
        risks = []
        
        if metrics['goto_statements'] > 0:
            risks.append({
                'type': 'control_flow',
                'description': f"Contains {metrics['goto_statements']} GO TO statements - requires refactoring",
                'severity': 'high'
            })
        
        if metrics['alter_statements'] > 0:
            risks.append({
                'type': 'control_flow',
                'description': f"Contains {metrics['alter_statements']} ALTER statements - complex refactoring needed",
                'severity': 'critical'
            })
        
        if metrics['sql_statements'] > 10:
            risks.append({
                'type': 'database',
                'description': f"High number of SQL operations ({metrics['sql_statements']}) - requires careful transaction design",
                'severity': 'medium'
            })
        
        if metrics['calls'] > 5:
            risks.append({
                'type': 'dependencies',
                'description': f"Calls {metrics['calls']} external programs - coordinate migration with dependencies",
                'severity': 'medium'
            })
        
        if metrics['total_lines'] > 1000:
            risks.append({
                'type': 'size',
                'description': f"Large program ({metrics['total_lines']} lines) - consider splitting into multiple services",
                'severity': 'medium'
            })
        
        if metrics['files'] > 5:
            risks.append({
                'type': 'io',
                'description': f"Accesses {metrics['files']} files - design appropriate data access layer",
                'severity': 'low'
            })
        
        return risks


def main():
    parser = argparse.ArgumentParser(description='Estimate COBOL program migration complexity')
    parser.add_argument('cobol_file', type=Path, help='Path to COBOL source file')
    parser.add_argument('--detailed', action='store_true', help='Show detailed metrics')
    parser.add_argument('--json', action='store_true', help='Output as JSON')
    
    args = parser.parse_args()
    
    if not args.cobol_file.exists():
        print(f"Error: File not found: {args.cobol_file}")
        return 1
    
    estimator = ComplexityEstimator(args.cobol_file)
    result = estimator.estimate()
    
    if args.json:
        print(json.dumps(result, indent=2))
    else:
        print(f"\nComplexity Analysis: {result['program']}")
        print("=" * 60)
        print(f"Complexity Level: {result['complexity_level']}")
        print(f"Complexity Score: {result['complexity_score']}")
        print(f"Estimated Effort: {result['estimated_effort_days']} person-days")
        
        if args.detailed:
            print("\nDetailed Metrics:")
            print("-" * 60)
            for key, value in result['metrics'].items():
                print(f"  {key.replace('_', ' ').title()}: {value}")
        
        if result['risk_factors']:
            print("\nRisk Factors:")
            print("-" * 60)
            for risk in result['risk_factors']:
                severity_marker = {
                    'critical': '🔴',
                    'high': '🟠',
                    'medium': '🟡',
                    'low': '🟢'
                }.get(risk['severity'], '⚪')
                print(f"  {severity_marker} [{risk['severity'].upper()}] {risk['description']}")
        else:
            print("\n✅ No significant risk factors identified")
        
        print()
    
    return 0


if __name__ == '__main__':
    exit(main())
