#!/bin/bash

# Create Component Script
# Generates a new Vue component with TypeScript and TailwindCSS boilerplate

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if component name is provided
if [ -z "$1" ]; then
    echo -e "${RED}Error: Component name is required${NC}"
    echo "Usage: ./create-component.sh ComponentName [type]"
    echo "Types: ui (default), layout, section"
    exit 1
fi

COMPONENT_NAME=$1
TYPE=${2:-ui}

# Validate component name (PascalCase)
if ! [[ $COMPONENT_NAME =~ ^[A-Z][a-zA-Z0-9]*$ ]]; then
    echo -e "${RED}Error: Component name must be in PascalCase (e.g., MyButton)${NC}"
    exit 1
fi

# Determine directory based on type
case $TYPE in
    ui)
        DIR="src/components/ui"
        ;;
    layout)
        DIR="src/components/layout"
        ;;
    section)
        DIR="src/components/sections"
        ;;
    *)
        echo -e "${RED}Error: Invalid type. Must be: ui, layout, or section${NC}"
        exit 1
        ;;
esac

# Create directory if it doesn't exist
mkdir -p "$DIR"

FILE_PATH="$DIR/${COMPONENT_NAME}.vue"

# Check if component already exists
if [ -f "$FILE_PATH" ]; then
    echo -e "${YELLOW}Warning: Component ${COMPONENT_NAME} already exists${NC}"
    read -p "Overwrite? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Create component file
cat > "$FILE_PATH" << 'EOF'
<script setup lang="ts">
import { ref, computed } from 'vue'

interface Props {
  // Add your props here
}

const props = withDefaults(defineProps<Props>(), {
  // Add default values here
})

const emit = defineEmits<{
  // Add your events here
  // example: [event: MouseEvent]
}>()

// Component logic here
</script>

<template>
  <div class="COMPONENT_CONTAINER">
    <!-- Add your template here -->
    <slot />
  </div>
</template>

<style scoped>
/* Add component-specific styles if needed */
/* Prefer TailwindCSS utility classes */
</style>
EOF

# Replace placeholder
sed -i '' "s/COMPONENT_CONTAINER/$(echo $COMPONENT_NAME | sed 's/\([A-Z]\)/-\L\1/g' | sed 's/^-//')/g" "$FILE_PATH"

echo -e "${GREEN}✓ Component created successfully!${NC}"
echo -e "  Location: ${FILE_PATH}"
echo ""
echo -e "${YELLOW}Next steps:${NC}"
echo "  1. Open ${FILE_PATH}"
echo "  2. Define your props interface"
echo "  3. Add component logic"
echo "  4. Build your template with TailwindCSS"
echo ""
echo -e "${YELLOW}Usage example:${NC}"
echo "  <script setup lang=\"ts\">"
echo "  import ${COMPONENT_NAME} from '@/components/${TYPE}/${COMPONENT_NAME}.vue'"
echo "  </script>"
echo ""
echo "  <template>"
echo "    <${COMPONENT_NAME} />"
echo "  </template>"
