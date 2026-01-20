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
  "copy_members": [],
  "files": []
}
EOF

# Function to extract program name from RPG file
extract_program_name() {
    local file="$1"
    # Try to find NOMAIN or program name in H-spec comments
    # Otherwise use filename
    basename "$file" | sed 's/\.[^.]*$//'
}

# Function to extract CALLB/CALLP statements
extract_calls() {
    local file="$1"
    grep -iE '\s+(CALLB|CALLP)\s+' "$file" | grep -oE "CALL[BP]\s+'?[A-Za-z0-9_-]+'?" | sed -E "s/CALL[BP]\s+'?([A-Za-z0-9_-]+)'?/\1/" || true
}

# Function to extract /COPY and /INCLUDE statements
extract_copies() {
    local file="$1"
    grep -iE '^\s*/COPY\s+|^\s*/INCLUDE\s+' "$file" | sed -E 's|.*/(COPY|INCLUDE)\s+([A-Za-z0-9/_-]+).*|\2|' || true
}

# Function to extract F-spec file names
extract_files() {
    local file="$1"
    grep -iE '^\s*F[A-Z0-9]+' "$file" | awk '{print $1}' | sed 's/^F//' || true
}

# Find all RPG files
echo "Scanning for RPG source files..."
RPG_FILES=$(find "$SOURCE_DIR" -type f \( -name "*.rpg" -o -name "*.RPG" -o -name "*.rpgle" -o -name "*.RPGLE" -o -name "*.sqlrpgle" -o -name "*.SQLRPGLE" \) 2>/dev/null || true)

if [ -z "$RPG_FILES" ]; then
    echo "No RPG files found in $SOURCE_DIR"
    exit 1
fi

# Create temporary files for collecting data
PROGRAMS_TEMP=$(mktemp)
DEPS_TEMP=$(mktemp)
COPY_MEMBERS_TEMP=$(mktemp)
FILES_TEMP=$(mktemp)

# Cleanup on exit
trap "rm -f $PROGRAMS_TEMP $DEPS_TEMP $COPY_MEMBERS_TEMP $FILES_TEMP" EXIT

# Process each RPG file
echo "$RPG_FILES" | while read -r rpg_file; do
    [ -z "$rpg_file" ] && continue
    
    echo "Processing: $rpg_file"
    
    program_name=$(extract_program_name "$rpg_file")
    
    # Add program to list
    echo "{\"name\": \"$program_name\", \"file\": \"$rpg_file\"}" >> "$PROGRAMS_TEMP"
    
    # Extract calls (CALLB/CALLP)
    extract_calls "$rpg_file" | while read -r called_program; do
        [ -z "$called_program" ] && continue
        echo "{\"from\": \"$program_name\", \"to\": \"$called_program\", \"type\": \"call\"}" >> "$DEPS_TEMP"
    done
    
    # Extract /COPY and /INCLUDE members
    extract_copies "$rpg_file" | while read -r copy_member; do
        [ -z "$copy_member" ] && continue
        echo "{\"name\": \"$copy_member\", \"used_by\": \"$program_name\"}" >> "$COPY_MEMBERS_TEMP"
        echo "{\"from\": \"$program_name\", \"to\": \"$copy_member\", \"type\": \"copy\"}" >> "$DEPS_TEMP"
    done
    
    # Extract F-spec file references
    extract_files "$rpg_file" | while read -r file_ref; do
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
    COPY_MEMBERS_JSON=$(cat "$COPY_MEMBERS_TEMP" | jq -s 'unique' 2>/dev/null || echo "[]")
    FILES_JSON=$(cat "$FILES_TEMP" | jq -s 'unique' 2>/dev/null || echo "[]")
    
    jq -n \
        --argjson programs "$PROGRAMS_JSON" \
        --argjson deps "$DEPS_JSON" \
        --argjson copy_members "$COPY_MEMBERS_JSON" \
        --argjson files "$FILES_JSON" \
        '{
            programs: $programs,
            dependencies: $deps,
            copy_members: $copy_members,
            files: $files,
            summary: {
                total_programs: ($programs | length),
                total_dependencies: ($deps | length),
                total_copy_members: ($copy_members | length),
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
    echo "  \"copy_members\": [" >> "$OUTPUT_FILE"
    cat "$COPY_MEMBERS_TEMP" | sed '$ ! s/$/,/' >> "$OUTPUT_FILE"
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
echo "  /COPY Members: $(sort -u "$COPY_MEMBERS_TEMP" | wc -l | xargs)"
echo "  Files: $(sort -u "$FILES_TEMP" | wc -l | xargs)"
