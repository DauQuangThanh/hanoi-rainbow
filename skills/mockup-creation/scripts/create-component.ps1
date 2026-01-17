# Create Component Script (PowerShell)
# Generates a new Vue component with TypeScript and TailwindCSS boilerplate
# Platform: Windows (PowerShell 5.1+), optional: Linux/macOS (PowerShell Core 7+)

[CmdletBinding()]
param(
    [Parameter(Mandatory=$true, Position=0)]
    [string]$ComponentName,
    
    [Parameter(Mandatory=$false, Position=1)]
    [ValidateSet('ui', 'layout', 'section')]
    [string]$Type = 'ui'
)

# Set error action preference
$ErrorActionPreference = "Stop"

# Function for colored output
function Write-ColorOutput {
    param(
        [string]$Message,
        [string]$Color = "White"
    )
    Write-Host $Message -ForegroundColor $Color
}

try {
    # Validate component name (PascalCase)
    if ($ComponentName -notmatch '^[A-Z][a-zA-Z0-9]*$') {
        Write-ColorOutput "Error: Component name must be in PascalCase (e.g., MyButton)" "Red"
        exit 1
    }

    # Determine directory based on type
    $dir = switch ($Type) {
        'ui'      { 'src\components\ui' }
        'layout'  { 'src\components\layout' }
        'section' { 'src\components\sections' }
    }

    # Create directory if it doesn't exist
    if (-not (Test-Path $dir)) {
        New-Item -ItemType Directory -Path $dir -Force | Out-Null
    }

    $filePath = Join-Path $dir "$ComponentName.vue"

    # Check if component already exists
    if (Test-Path $filePath) {
        Write-ColorOutput "Warning: Component $ComponentName already exists" "Yellow"
        $response = Read-Host "Overwrite? (y/n)"
        if ($response -ne 'y') {
            exit 1
        }
    }

    # Convert PascalCase to kebab-case for CSS class
    $kebabCase = $ComponentName -creplace '([A-Z])', '-$1' -replace '^-', '' | ForEach-Object { $_.ToLower() }

    # Create component file
    $componentContent = @"
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
  <div class="$kebabCase">
    <!-- Add your template here -->
    <slot />
  </div>
</template>

<style scoped>
/* Add component-specific styles if needed */
/* Prefer TailwindCSS utility classes */
</style>
"@

    Set-Content -Path $filePath -Value $componentContent -Encoding UTF8

    Write-ColorOutput "✓ Component created successfully!" "Green"
    Write-Host "  Location: $filePath"
    Write-Host ""
    Write-ColorOutput "Next steps:" "Yellow"
    Write-Host "  1. Open $filePath"
    Write-Host "  2. Define your props interface"
    Write-Host "  3. Add component logic"
    Write-Host "  4. Build your template with TailwindCSS"
    Write-Host ""
    Write-ColorOutput "Usage example:" "Yellow"
    Write-Host "  <script setup lang=`"ts`">"
    Write-Host "  import $ComponentName from '@/components/$Type/$ComponentName.vue'"
    Write-Host "  </script>"
    Write-Host ""
    Write-Host "  <template>"
    Write-Host "    <$ComponentName />"
    Write-Host "  </template>"
}
catch {
    Write-ColorOutput "Error: $_" "Red"
    exit 1
}
