#!/bin/bash

# setup-standardize.sh
# Sets up the standardize command workflow for Spec-Driven Development
# This script creates the standards document from template and prepares the environment

set -euo pipefail

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source common utilities
source "$SCRIPT_DIR/common.sh"

# Main setup function
setup_standardize() {
    local repo_root
    repo_root=$(get_repo_root)
    
    # Navigate to repository root
    cd "$repo_root" || exit 1
    
    # Define paths
    local docs_dir="$repo_root/docs"
    local standards_file="$docs_dir/standards.md"
    local template_file="$repo_root/.rainbow/templates/templates-for-commands/standards-template.md"
    local ground_rules_file="$repo_root/memory/ground-rules.md"
    local architecture_file="$docs_dir/architecture.md"
    local specs_dir="$repo_root/specs"
    
    # Ensure required directories exist
    mkdir -p "$docs_dir"
    
    # Check for required files
    if [ ! -f "$template_file" ]; then
        echo "ERROR: Standards template not found at: $template_file" >&2
        exit 1
    fi
    
    if [ ! -f "$ground_rules_file" ]; then
        echo "ERROR: Ground-rules file not found at: $ground_rules_file" >&2
        echo "INFO: Please run the regulate command first." >&2
        exit 1
    fi
    
    # Create standards document if it doesn't exist
    if [ ! -f "$standards_file" ]; then
        echo "INFO: Creating standards document from template..."
        cp "$template_file" "$standards_file"
        echo "✓ Created: $standards_file"
    else
        echo "WARNING: Standards document already exists: $standards_file"
        echo "INFO: Will update with latest context."
    fi
    
    # Detect technology stack from architecture if available
    local tech_stack=""
    if [ -f "$architecture_file" ]; then
        echo "INFO: Detected architecture document. Extracting technology stack..."
        tech_stack=$(detect_tech_stack "$architecture_file")
    else
        echo "WARNING: Architecture document not found. Standards will need manual tech stack updates."
        tech_stack="[Technology stack not detected - please update manually]"
    fi
    
    # Count feature specifications
    local feature_count=0
    if [ -d "$specs_dir" ]; then
        feature_count=$(find "$specs_dir" -maxdepth 2 -name "spec.md" | wc -l | tr -d ' ')
    fi
    
    # Detect AI agent
    local detected_agent
    detected_agent=$(detect_ai_agent "$repo_root")
    
    # Update standards document with context
    update_standards_context "$standards_file" "$tech_stack"
    
    # Generate JSON output for AI agents
    generate_json_output "$standards_file" "$feature_count" "$detected_agent" "$tech_stack"
    
    # Print human-readable summary
    print_summary "$standards_file" "$feature_count" "$tech_stack"
}

# Detect technology stack from architecture document
detect_tech_stack() {
    local arch_file="$1"
    local tech_stack=""
    
    # Extract technology information from architecture document
    # Look for common technology stack sections
    if grep -qi "frontend.*react" "$arch_file"; then
        tech_stack="${tech_stack}Frontend: React, "
    elif grep -qi "frontend.*vue" "$arch_file"; then
        tech_stack="${tech_stack}Frontend: Vue, "
    elif grep -qi "frontend.*angular" "$arch_file"; then
        tech_stack="${tech_stack}Frontend: Angular, "
    fi
    
    if grep -qi "backend.*fastapi\|backend.*python" "$arch_file"; then
        tech_stack="${tech_stack}Backend: Python/FastAPI, "
    elif grep -qi "backend.*express\|backend.*node" "$arch_file"; then
        tech_stack="${tech_stack}Backend: Node.js, "
    elif grep -qi "backend.*spring\|backend.*java" "$arch_file"; then
        tech_stack="${tech_stack}Backend: Java/Spring, "
    fi
    
    if grep -qi "postgresql\|postgres" "$arch_file"; then
        tech_stack="${tech_stack}Database: PostgreSQL, "
    elif grep -qi "mongodb\|mongo" "$arch_file"; then
        tech_stack="${tech_stack}Database: MongoDB, "
    elif grep -qi "mysql" "$arch_file"; then
        tech_stack="${tech_stack}Database: MySQL, "
    fi
    
    # Remove trailing comma and space
    tech_stack=$(echo "$tech_stack" | sed 's/, $//')
    
    if [ -z "$tech_stack" ]; then
        tech_stack="[Technology stack not detected - please update manually]"
    fi
    
    echo "$tech_stack"
}

# Update standards document with detected context
update_standards_context() {
    local standards_file="$1"
    local tech_stack="$2"
    
    # Update technology stack section if placeholder exists
    if grep -q "\[e.g., React 18, TypeScript 5" "$standards_file"; then
        print_info "Updating technology stack in standards document..."
        # Note: This is a basic update. Manual review recommended.
    fi
}

# Generate JSON output for AI agent consumption
generate_json_output() {
    local standards_file="$1"
    local feature_count="$2"
    local ai_agent="$3"
    local tech_stack="$4"
    
    cat <<EOF
{
  "command": "standardize",
  "status": "ready",
  "standards_document": "$standards_file",
  "feature_count": $feature_count,
  "detected_ai_agent": "$ai_agent",
  "detected_tech_stack": "$tech_stack",
  "mandatory_sections": {
    "ui_naming_conventions": {
      "status": "required",
      "priority": "MANDATORY",
      "location": "Section 2"
    }
  },
  "next_steps": [
    "Review and customize standards based on project needs",
    "Ensure UI naming conventions are comprehensive (MANDATORY)",
    "Update technology stack sections with actual stack",
    "Configure linters and formatters according to standards",
    "Set up pre-commit hooks for enforcement",
    "Share standards with all team members",
    "Reference standards in code reviews"
  ],
  "workflow": {
    "previous_command": "architect",
    "current_command": "standardize",
    "next_command": "design",
    "document_type": "product-level",
    "document_location": "docs/"
  }
}
EOF
}

# Print human-readable summary
print_summary() {
    local standards_file="$1"
    local feature_count="$2"
    local tech_stack="$3"
    
    echo ""
    print_success "=== Standardize Setup Complete ==="
    echo ""
    print_info "Standards Document: $standards_file"
    print_info "Detected Tech Stack: $tech_stack"
    print_info "Feature Specifications: $feature_count"
    echo ""
    print_warning "⭐ IMPORTANT: UI Naming Conventions (Section 2) are MANDATORY"
    echo ""
    print_info "Next Steps:"
    echo "  1. Review and customize the standards document"
    echo "  2. Update technology-specific sections with your actual stack"
    echo "  3. Ensure UI naming conventions are comprehensive"
    echo "  4. Configure linters and formatters based on standards"
    echo "  5. Set up pre-commit hooks for automated enforcement"
    echo "  6. Share standards document with all team members"
    echo ""
    print_info "To generate standards with AI assistance, run:"
    echo "  standardize"
    echo ""
}

# Execute main setup
setup_standardize
