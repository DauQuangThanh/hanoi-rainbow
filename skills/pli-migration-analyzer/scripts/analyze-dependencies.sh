#!/bin/bash
#
# Analyze dependencies between PL/I programs, include files, and external procedures.
# This script scans a directory of PL/I programs and generates a dependency graph.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE_DIR="${1:-.}"

# Output file
OUTPUT_FILE="dependencies.json"

echo "Analyzing dependencies in: $SOURCE_DIR"
echo "Output will be written to: $OUTPUT_FILE"

# Initialize JSON structure
cat > "$OUTPUT_FILE" << 'EOF'
{
  "programs": [],
  "dependencies": [],
  "include_files": [],
  "files": []
}
EOF

# Function to extract program name from PL/I file
extract_program_name() {
    local file="$1"
    # PL/I programs use filename as program name
    basename "$file" | sed -E 's/\.(pli|PLI|pl1)$//'
}

# Function to extract CALL statements
extract_calls() {
    local file="$1"
    grep -iE "CALL +[A-Za-z0-9_]+" "$file" | grep -oE "CALL +[A-Za-z0-9_]+" | awk '{print $2}' || true
}

# Function to extract %INCLUDE statements
extract_copies() {
    local file="$1"
    grep -iE "%INCLUDE" "$file" | grep -oE "%INCLUDE +[A-Za-z0-9_/.]+" | awk '{print $2}' | tr -d "';" || true
}

# Function to extract file names from OPEN/CLOSE statements
extract_files() {
    local file="$1"
    grep -iE "(OPEN|CLOSE) +FILE\([A-Za-z0-9_]+\)" "$file" | grep -oE "FILE\([A-Za-z0-9_]+\)" | tr -d 'FILE()' || true
}

# Find all PL/I files
echo "Scanning for PL/I files..."
PLI_FILES=$(find "$SOURCE_DIR" -type f \( -name "*.pli" -o -name "*.PLI" -o -name "*.pl1" \) 2>/dev/null || true)

if [ -z "$PLI_FILES" ]; then
    echo "No PL/I files found in $SOURCE_DIR"
    exit 1
fi

# Create temporary files for collecting data
PROGRAMS_TEMP=$(mktemp)
DEPS_TEMP=$(mktemp)
INCLUDES_TEMP=$(mktemp)
FILES_TEMP=$(mktemp)

# Cleanup on exit
trap "rm -f $PROGRAMS_TEMP $DEPS_TEMP $INCLUDES_TEMP $FILES_TEMP" EXIT

# Process each PL/I file
echo "$PLI_FILES" | while read -r pli_file; do
    [ -z "$pli_file" ] && continue
    
    echo "Processing: $pli_file"
    
    program_name=$(extract_program_name "$pli_file")
    
    # Add program to list
    echo "{\"name\": \"$program_name\", \"file\": \"$pli_file\"}" >> "$PROGRAMS_TEMP"
    
    # Extract calls
    extract_calls "$pli_file" | while read -r called_program; do
        [ -z "$called_program" ] && continue
        echo "{\"from\": \"$program_name\", \"to\": \"$called_program\", \"type\": \"call\"}" >> "$DEPS_TEMP"
    done
    
    # Extract %INCLUDE files
    extract_copies "$pli_file" | while read -r include_file; do
        [ -z "$include_file" ] && continue
        echo "{\"name\": \"$include_file\", \"used_by\": \"$program_name\"}" >> "$INCLUDES_TEMP"
        echo "{\"from\": \"$program_name\", \"to\": \"$include_file\", \"type\": \"include\"}" >> "$DEPS_TEMP"
    done
    
    # Extract file references
    extract_files "$pli_file" | while read -r file_ref; do
        [ -z "$file_ref" ] && continue
        echo "{\"name\": \"$file_ref\", \"used_by\": \"$program_name\"}" >> "$FILES_TEMP"
        echo "{\"from\": \"$program_name\", \"to\": \"$file_ref\", \"type\": \"file\"}" >> "$DEPS_TEMP"
    done
done

# Build final JSON using jq if available, otherwise use basic concatenation
if command -v jq &> /dev/null; then
    echo "Building dependency graph with jq..."
    
    PROGRAMS_JSON=$(cat "$PROGRAMS_TEMP" | jq -s '.' 2>/dev/null || echo "[]")
    DEPS_JSON=$(cat "$DEPS_TEMP" | jq -s '.' 2>/dev/null || echo "[]")
    INCLUDES_JSON=$(cat "$INCLUDES_TEMP" | jq -s 'unique' 2>/dev/null || echo "[]")
    FILES_JSON=$(cat "$FILES_TEMP" | jq -s 'unique' 2>/dev/null || echo "[]")
    
    jq -n \
        --argjson programs "$PROGRAMS_JSON" \
        --argjson deps "$DEPS_JSON" \
        --argjson includes "$INCLUDES_JSON" \
        --argjson files "$FILES_JSON" \
        '{
            programs: $programs,
            dependencies: $deps,
            include_files: $includes,
            files: $files,
            summary: {
                total_programs: ($programs | length),
                total_dependencies: ($deps | length),
                total_includes: ($includes | length),
                total_files: ($files | length)
            }
        }' > "$OUTPUT_FILE"
else
    echo "jq not found, generating basic JSON..."
    # Fallback without jq (less elegant but works)
    echo "{" > "$OUTPUT_FILE"
    echo "  \"programs\": [" >> "$OUTPUT_FILE"
    cat "$PROGRAMS_TEMP" | sed '$ ! s/$/,/' >> "$OUTPUT_FILE"
    echo "  ]," >> "$OUTPUT_FILE"
    echo "  \"dependencies\": [" >> "$OUTPUT_FILE"
    cat "$DEPS_TEMP" | sed '$ ! s/$/,/' >> "$OUTPUT_FILE"
    echo "  ]," >> "$OUTPUT_FILE"
    echo "  \"include_files\": [" >> "$OUTPUT_FILE"
    cat "$INCLUDES_TEMP" | sed '$ ! s/$/,/' >> "$OUTPUT_FILE"
    echo "  ]," >> "$OUTPUT_FILE"
    echo "  \"files\": [" >> "$OUTPUT_FILE"
    cat "$FILES_TEMP" | sed '$ ! s/$/,/' >> "$OUTPUT_FILE"
    echo "  ]" >> "$OUTPUT_FILE"
    echo "}" >> "$OUTPUT_FILE"
fi

echo ""
echo "Analysis complete! Results written to: $OUTPUT_FILE"
echo ""
echo "Summary:"
echo "  Programs: $(grep -c "\"name\"" "$PROGRAMS_TEMP" || echo 0)"
echo "  Dependencies: $(wc -l < "$DEPS_TEMP" | xargs)"
echo "  Include files: $(sort -u "$INCLUDES_TEMP" | wc -l | xargs)"
echo "  Files: $(sort -u "$FILES_TEMP" | wc -l | xargs)"
