#!/usr/bin/env bash

set -e

# Parse command line arguments
JSON_MODE=false
ARGS=()

for arg in "$@"; do
    case "$arg" in
        --json)
            JSON_MODE=true
            ;;
        --help|-h)
            echo "Usage: $0 [--json]"
            echo "  --json    Output results in JSON format"
            echo "  --help    Show this help message"
            exit 0
            ;;
        *)
            ARGS+=("$arg")
            ;;
    esac
done

# Get script directory and load common functions
SCRIPT_DIR="$(CDPATH="" cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/common.sh"

# Get all paths and variables from common functions
eval $(get_feature_paths)

# Check if we're on a proper feature branch (only for git repos)
check_feature_branch "$CURRENT_BRANCH" "$HAS_GIT" || exit 1

# Ensure the feature directory exists
mkdir -p "$FEATURE_DIR"

# Set the business logic map file path
BUSINESS_LOGIC_MAP="$FEATURE_DIR/business-logic-map.md"

# Copy template if it exists
TEMPLATE="$REPO_ROOT/.rainbow/templates/templates-for-agents/business-logic-extraction-template.md"
if [[ -f "$TEMPLATE" ]]; then
    cp "$TEMPLATE" "$BUSINESS_LOGIC_MAP"
    echo "Copied business logic extraction template to $BUSINESS_LOGIC_MAP"
else
    echo "Warning: Business logic extraction template not found at $TEMPLATE"
    touch "$BUSINESS_LOGIC_MAP"
fi

# Output results
if $JSON_MODE; then
    printf '{"FEATURE_SPEC":"%s","BUSINESS_LOGIC_MAP":"%s","SPECS_DIR":"%s","BRANCH":"%s","HAS_GIT":"%s"}\n' \
        "$FEATURE_SPEC" "$BUSINESS_LOGIC_MAP" "$FEATURE_DIR" "$CURRENT_BRANCH" "$HAS_GIT"
else
    echo "FEATURE_SPEC: $FEATURE_SPEC"
    echo "BUSINESS_LOGIC_MAP: $BUSINESS_LOGIC_MAP"
    echo "SPECS_DIR: $FEATURE_DIR"
    echo "BRANCH: $CURRENT_BRANCH"
    echo "HAS_GIT: $HAS_GIT"
fi
