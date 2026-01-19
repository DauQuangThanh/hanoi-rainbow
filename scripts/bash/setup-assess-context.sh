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

# Get repository root
REPO_ROOT=$(get_repo_root)

# Ensure the docs directory exists
DOCS_DIR="$REPO_ROOT/docs"
mkdir -p "$DOCS_DIR"

# Set the context assessment file path (project-level, not feature-specific)
CONTEXT_ASSESSMENT="$DOCS_DIR/context-assessment.md"

# Copy template if it exists
TEMPLATE="$RAINBOW_DIR/templates/templates-for-commands/context-assessment-template.md"
if [[ -f "$TEMPLATE" ]]; then
    cp "$TEMPLATE" "$CONTEXT_ASSESSMENT"
    echo "Copied context assessment template to $CONTEXT_ASSESSMENT"
else
    echo "Warning: Context assessment template not found at $TEMPLATE"
    # Create a basic assessment file if template doesn't exist
    touch "$CONTEXT_ASSESSMENT"
fi

# Check if we're in a git repo
HAS_GIT="false"
if [[ -d "$REPO_ROOT/.git" ]]; then
    HAS_GIT="true"
fi

# Output results
if $JSON_MODE; then
    printf '{"CONTEXT_ASSESSMENT":"%s","DOCS_DIR":"%s","REPO_ROOT":"%s","HAS_GIT":"%s"}\n' \
        "$CONTEXT_ASSESSMENT" "$DOCS_DIR" "$REPO_ROOT" "$HAS_GIT"
else
    echo "CONTEXT_ASSESSMENT: $CONTEXT_ASSESSMENT"
    echo "DOCS_DIR: $DOCS_DIR"
    echo "REPO_ROOT: $REPO_ROOT"
    echo "HAS_GIT: $HAS_GIT"
fi
