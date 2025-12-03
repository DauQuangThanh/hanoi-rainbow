#!/bin/bash

# setup-architect.sh - Setup script for /rainbow.architect command
# Creates architecture documentation structure and prepares for architecture design workflow

set -e

# Source common functions
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/common.sh"

# Configuration
DOCS_DIR="docs"
ARCH_DOC="$DOCS_DIR/architecture.md"
ARCH_TEMPLATE=".rainbow/templates/templates-for-commands/arch-template.md"
ADR_DIR="$DOCS_DIR/adr"
SPECS_DIR="specs"

# Parse arguments
JSON_OUTPUT=false
PRODUCT_NAME=""
while [[ $# -gt 0 ]]; do
    case $1 in
        --json)
            JSON_OUTPUT=true
            shift
            ;;
        --product)
            PRODUCT_NAME="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
done

# Verify we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "ERROR: Not in a git repository. Please run this from the repository root." >&2
    exit 1
fi

REPO_ROOT=$(git rev-parse --show-toplevel)
cd "$REPO_ROOT"

# Verify required files exist
if [ ! -f "$ARCH_TEMPLATE" ]; then
    echo "ERROR: Architecture template not found: $ARCH_TEMPLATE" >&2
    exit 1
fi

if [ ! -f "memory/ground-rules.md" ]; then
    echo "ERROR: Ground rules file not found: memory/ground-rules.md. Run /rainbow.regulate first." >&2
    exit 1
fi

# Get product name from user input or git repo
if [ -z "$PRODUCT_NAME" ]; then
    PRODUCT_NAME=$(basename "$REPO_ROOT")
fi

# Create docs directory structure
echo "INFO: Creating architecture documentation structure..."
mkdir -p "$DOCS_DIR"
mkdir -p "$ADR_DIR"

# Copy architecture template if it doesn't exist
if [ ! -f "$ARCH_DOC" ]; then
    echo "INFO: Creating architecture document from template..."
    cp "$ARCH_TEMPLATE" "$ARCH_DOC"
    
    # Replace placeholder with product name
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        sed -i '' "s/\[PRODUCT\/PROJECT NAME\]/$PRODUCT_NAME/g" "$ARCH_DOC"
        sed -i '' "s/\[DATE\]/$(date +%Y-%m-%d)/g" "$ARCH_DOC"
    else
        # Linux
        sed -i "s/\[PRODUCT\/PROJECT NAME\]/$PRODUCT_NAME/g" "$ARCH_DOC"
        sed -i "s/\[DATE\]/$(date +%Y-%m-%d)/g" "$ARCH_DOC"
    fi
    
    echo "✓ Created: $ARCH_DOC"
else
    echo "INFO: Architecture document already exists: $ARCH_DOC"
fi

# Count existing feature specs
SPEC_COUNT=0
if [ -d "$SPECS_DIR" ]; then
    SPEC_COUNT=$(find "$SPECS_DIR" -name "spec.md" 2>/dev/null | wc -l | tr -d ' ')
fi

# Get list of feature spec paths
FEATURE_SPECS=()
if [ -d "$SPECS_DIR" ]; then
    while IFS= read -r spec_file; do
        FEATURE_SPECS+=("$spec_file")
    done < <(find "$SPECS_DIR" -name "spec.md" 2>/dev/null)
fi

# Detect AI agent
AGENT_TYPE=""
if [ -d ".github/agents" ]; then
    AGENT_TYPE="copilot"
elif [ -d ".claude/commands" ]; then
    AGENT_TYPE="claude"
elif [ -d ".cursor/commands" ]; then
    AGENT_TYPE="cursor-agent"
elif [ -d ".windsurf/workflows" ]; then
    AGENT_TYPE="windsurf"
elif [ -d ".gemini/commands" ]; then
    AGENT_TYPE="gemini"
elif [ -d ".qwen/commands" ]; then
    AGENT_TYPE="qwen"
elif [ -d ".opencode/command" ]; then
    AGENT_TYPE="opencode"
elif [ -d ".codex/commands" ]; then
    AGENT_TYPE="codex"
elif [ -d ".kilocode/rules" ]; then
    AGENT_TYPE="kilocode"
elif [ -d ".augment/rules" ]; then
    AGENT_TYPE="auggie"
elif [ -d ".roo/rules" ]; then
    AGENT_TYPE="roo"
elif [ -d ".codebuddy/commands" ]; then
    AGENT_TYPE="codebuddy"
elif [ -d ".amazonq/prompts" ]; then
    AGENT_TYPE="q"
elif [ -d ".agents/commands" ]; then
    AGENT_TYPE="amp"
elif [ -d ".shai/commands" ]; then
    AGENT_TYPE="shai"
elif [ -d ".bob/commands" ]; then
    AGENT_TYPE="bob"
fi

# Output results
if [ "$JSON_OUTPUT" = true ]; then
    # JSON output for agent consumption
    echo "{"
    echo "  \"ARCH_DOC\": \"$ARCH_DOC\","
    echo "  \"DOCS_DIR\": \"$DOCS_DIR\","
    echo "  \"ADR_DIR\": \"$ADR_DIR\","
    echo "  \"SPECS_DIR\": \"$SPECS_DIR\","
    echo "  \"SPEC_COUNT\": $SPEC_COUNT,"
    echo "  \"FEATURE_SPECS\": ["
    for i in "${!FEATURE_SPECS[@]}"; do
        if [ $i -eq $((${#FEATURE_SPECS[@]} - 1)) ]; then
            echo "    \"${FEATURE_SPECS[$i]}\""
        else
            echo "    \"${FEATURE_SPECS[$i]}\","
        fi
    done
    echo "  ],"
    echo "  \"PRODUCT_NAME\": \"$PRODUCT_NAME\","
    echo "  \"GROUND_RULES\": \"memory/ground-rules.md\","
    echo "  \"AGENT_TYPE\": \"$AGENT_TYPE\","
    echo "  \"REPO_ROOT\": \"$REPO_ROOT\""
    echo "}"
else
    # Human-readable output
    echo ""
    info "Architecture Documentation Setup Complete"
    echo ""
    echo "📋 Configuration:"
    echo "   Product Name:       $PRODUCT_NAME"
    echo "   Architecture Doc:   $ARCH_DOC"
    echo "   Documentation Dir:  $DOCS_DIR"
    echo "   ADR Directory:      $ADR_DIR"
    echo "   Feature Specs:      $SPEC_COUNT found in $SPECS_DIR/"
    echo "   AI Agent Detected:  ${AGENT_TYPE:-none}"
    echo ""
    echo "📝 Next Steps:"
    echo "   1. Review and complete $ARCH_DOC"
    echo "   2. Create C4 diagrams using Mermaid"
    echo "   3. Document architecture decisions in $ADR_DIR/"
    echo "   4. Run feature implementation plans with /rainbow.design"
    echo ""
fi

exit 0
