#!/bin/bash

# setup-design-e2e-test.sh
# Sets up the design-e2e-test command workflow for Spec-Driven Development
# This script creates the E2E test plan document from template and prepares the environment

set -euo pipefail

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source common utilities
source "$SCRIPT_DIR/common.sh"

# Main setup function
setup_design_e2e_test() {
    local repo_root
    repo_root=$(get_repo_root)
    
    # Navigate to repository root
    cd "$repo_root" || exit 1
    
    # Define paths
    local docs_dir="$repo_root/docs"
    local e2e_test_file="$docs_dir/e2e-test-plan.md"
    local template_file="$repo_root/.rainbow/templates/templates-for-commands/e2e-test-template.md"
    local ground_rules_file="$repo_root/memory/ground-rules.md"
    local architecture_file="$docs_dir/architecture.md"
    local specs_dir="$repo_root/specs"
    
    # Ensure required directories exist
    mkdir -p "$docs_dir"
    
    # Check for required files
    if [ ! -f "$template_file" ]; then
        print_error "E2E test template not found at: $template_file"
        exit 1
    fi
    
    if [ ! -f "$ground_rules_file" ]; then
        print_error "Ground-rules file not found at: $ground_rules_file"
        print_info "Please run the regulate command first."
        exit 1
    fi
    
    if [ ! -f "$architecture_file" ]; then
        print_error "Architecture file not found at: $architecture_file"
        print_info "Please run the architect command first."
        exit 1
    fi
    
    # Create E2E test document if it doesn't exist
    if [ ! -f "$e2e_test_file" ]; then
        print_info "Creating E2E test plan document from template..."
        cp "$template_file" "$e2e_test_file"
        print_success "Created: $e2e_test_file"
    else
        print_warning "E2E test plan document already exists: $e2e_test_file"
        print_info "Will update with latest context."
    fi
    
    # Count feature specifications
    local feature_count=0
    if [ -d "$specs_dir" ]; then
        feature_count=$(find "$specs_dir" -maxdepth 2 -name "spec.md" | wc -l | tr -d ' ')
    fi
    
    # Extract critical integration points from architecture
    local integration_info=""
    if [ -f "$architecture_file" ]; then
        print_info "Analyzing architecture for integration points..."
        integration_info=$(extract_integration_points "$architecture_file")
    fi
    
    # Detect AI agent
    local detected_agent
    detected_agent=$(detect_ai_agent "$repo_root")
    
    # Generate JSON output for AI agents
    generate_json_output "$e2e_test_file" "$feature_count" "$detected_agent" "$integration_info"
    
    # Print human-readable summary
    print_summary "$e2e_test_file" "$feature_count" "$integration_info"
}

# Extract integration points from architecture document
extract_integration_points() {
    local arch_file="$1"
    local integration_points=""
    
    # Look for common integration keywords in architecture
    if grep -qi "external.*system\|api.*integration\|microservice\|third.*party" "$arch_file"; then
        integration_points="External integrations detected. "
    fi
    
    if grep -qi "database\|postgresql\|mongodb\|mysql" "$arch_file"; then
        integration_points="${integration_points}Database layer present. "
    fi
    
    if grep -qi "frontend\|react\|vue\|angular" "$arch_file"; then
        integration_points="${integration_points}Frontend UI layer present. "
    fi
    
    if grep -qi "mobile\|ios\|android\|react native\|flutter" "$arch_file"; then
        integration_points="${integration_points}Mobile app layer present. "
    fi
    
    if [ -z "$integration_points" ]; then
        integration_points="Integration points need manual identification from architecture"
    fi
    
    echo "$integration_points"
}

# Generate JSON output for AI agent consumption
generate_json_output() {
    local e2e_test_file="$1"
    local feature_count="$2"
    local ai_agent="$3"
    local integration_info="$4"
    
    cat <<EOF
{
  "command": "design-e2e-test",
  "status": "ready",
  "e2e_test_document": "$e2e_test_file",
  "feature_count": $feature_count,
  "detected_ai_agent": "$ai_agent",
  "integration_analysis": "$integration_info",
  "prerequisites": {
    "architecture_doc": "docs/architecture.md",
    "ground_rules": "memory/ground-rules.md",
    "feature_specs": "specs/*/spec.md"
  },
  "next_steps": [
    "Review architecture.md to identify system components and integration points",
    "Extract critical user journeys from feature specifications",
    "Design test scenarios covering end-to-end workflows",
    "Define test data management and environment strategy",
    "Select appropriate testing framework and tools",
    "Create detailed test scenarios with expected results",
    "Plan test execution schedule and CI/CD integration",
    "Document test reporting and metrics strategy"
  ],
  "workflow": {
    "previous_command": "architect",
    "current_command": "design-e2e-test",
    "next_command": "standardize",
    "document_type": "product-level",
    "document_location": "docs/"
  },
  "output_files": [
    "docs/e2e-test-plan.md",
    "docs/e2e-test-scenarios.md",
    "docs/test-data-guide.md",
    "docs/e2e-test-setup.md"
  ]
}
EOF
}

# Print human-readable summary
print_summary() {
    local e2e_test_file="$1"
    local feature_count="$2"
    local integration_info="$3"
    
    echo ""
    print_success "=== Design E2E Test Setup Complete ==="
    echo ""
    print_info "E2E Test Document: $e2e_test_file"
    print_info "Feature Specifications: $feature_count"
    print_info "Integration Analysis: $integration_info"
    echo ""
    print_info "Prerequisites Met:"
    echo "  ✓ Architecture document available"
    echo "  ✓ Ground-rules constraints loaded"
    echo "  ✓ Feature specifications available"
    echo ""
    print_info "Next Steps:"
    echo "  1. Review architecture to identify system integration points"
    echo "  2. Extract critical user journeys from feature specs"
    echo "  3. Design comprehensive E2E test scenarios"
    echo "  4. Define test data management strategy"
    echo "  5. Select testing framework (Playwright, Cypress, etc.)"
    echo "  6. Plan test execution and CI/CD integration"
    echo "  7. Set up test reporting and monitoring"
    echo ""
    print_info "To generate E2E test plan with AI assistance, run:"
    echo "  design-e2e-test"
    echo ""
}

# Execute main setup
setup_design_e2e_test
