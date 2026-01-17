#!/bin/bash
#
# Analyze dependencies between RPG programs, service programs, and files.
# This script scans a directory of RPG programs and generates a dependency graph.

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
  "copybooks": [],
  "files": []
}
EOF

# Function to extract program name from COBOL file
extract_program_name() {
    local file="$1"
    grep -i "PROGRAM-ID" "$file" | head -1 | sed -E 's/.*PROGRAM-ID[. ]+([A-Za-z0-9-]+).*/\1/' || basename "$file" .cbl
}

# Function to extract CALL statements
extract_calls() {
    local file="$1"
    grep -i "CALL" "$file" | grep -oE "'[A-Z0-9-]+'" | tr -d "'" || true
}

# Function to extract COPY statements
extract_copies() {
    local file="$1"
    grep -i "COPY" "$file" | grep -oE 'COPY +[A-Z0-9-]+' | awk '{print $2}' | tr -d '.' || true
}

# Function to extract SELECT/ASSIGN file names
extract_files() {
    local file="$1"
    grep -i "SELECT" "$file" | grep -oE 'SELECT +[A-Z0-9-]+' | awk '{print $2}' || true
}

# Find all COBOL files
echo "Scanning for COBOL files..."
COBOL_FILES=$(find "$SOURCE_DIR" -type f \( -name "*.cbl" -o -name "*.CBL" -o -name "*.cob" -o -name "*.COB" \) 2>/dev/null || true)

if [ -z "$COBOL_FILES" ]; then
    echo "No COBOL files found in $SOURCE_DIR"
    exit 1
fi

# Create temporary files for collecting data
PROGRAMS_TEMP=$(mktemp)
DEPS_TEMP=$(mktemp)
COPYBOOKS_TEMP=$(mktemp)
FILES_TEMP=$(mktemp)

# Cleanup on exit
trap "rm -f $PROGRAMS_TEMP $DEPS_TEMP $COPYBOOKS_TEMP $FILES_TEMP" EXIT

# Process each COBOL file
echo "$COBOL_FILES" | while read -r cobol_file; do
    [ -z "$cobol_file" ] && continue
    
    echo "Processing: $cobol_file"
    
    program_name=$(extract_program_name "$cobol_file")
    
    # Add program to list
    echo "{\"name\": \"$program_name\", \"file\": \"$cobol_file\"}" >> "$PROGRAMS_TEMP"
    
    # Extract calls
    extract_calls "$cobol_file" | while read -r called_program; do
        [ -z "$called_program" ] && continue
        echo "{\"from\": \"$program_name\", \"to\": \"$called_program\", \"type\": \"call\"}" >> "$DEPS_TEMP"
    done
    
    # Extract copybooks
    extract_copies "$cobol_file" | while read -r copybook; do
        [ -z "$copybook" ] && continue
        echo "{\"name\": \"$copybook\", \"used_by\": \"$program_name\"}" >> "$COPYBOOKS_TEMP"
        echo "{\"from\": \"$program_name\", \"to\": \"$copybook\", \"type\": \"copy\"}" >> "$DEPS_TEMP"
    done
    
    # Extract file references
    extract_files "$cobol_file" | while read -r file_ref; do
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
    COPYBOOKS_JSON=$(cat "$COPYBOOKS_TEMP" | jq -s 'unique' 2>/dev/null || echo "[]")
    FILES_JSON=$(cat "$FILES_TEMP" | jq -s 'unique' 2>/dev/null || echo "[]")
    
    jq -n \
        --argjson programs "$PROGRAMS_JSON" \
        --argjson deps "$DEPS_JSON" \
        --argjson copybooks "$COPYBOOKS_JSON" \
        --argjson files "$FILES_JSON" \
        '{
            programs: $programs,
            dependencies: $deps,
            copybooks: $copybooks,
            files: $files,
            summary: {
                total_programs: ($programs | length),
                total_dependencies: ($deps | length),
                total_copybooks: ($copybooks | length),
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
    echo "  \"copybooks\": [" >> "$OUTPUT_FILE"
    cat "$COPYBOOKS_TEMP" | sed '$ ! s/$/,/' >> "$OUTPUT_FILE"
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
echo "  Copybooks: $(sort -u "$COPYBOOKS_TEMP" | wc -l | xargs)"
echo "  Files: $(sort -u "$FILES_TEMP" | wc -l | xargs)"
