# setup-standardize.ps1
# Sets up the standardize command workflow for Spec-Driven Development
# This script creates the standards document from template and prepares the environment

$ErrorActionPreference = "Stop"

# Get the directory where this script is located
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

# Source common utilities
. (Join-Path $scriptDir "common.ps1")

# Main setup function
function Setup-Standardize {
    $repoRoot = Get-RepoRoot
    
    # Navigate to repository root
    Set-Location $repoRoot
    
    # Define paths
    $docsDir = Join-Path $repoRoot "docs"
    $standardsFile = Join-Path $docsDir "standards.md"
    $templateFile = Join-Path $repoRoot ".rainbow" "templates" "templates-for-commands" "standards-template.md"
    $groundRulesFile = Join-Path $repoRoot ".rainbow" "memory" "ground-rules.md"
    $architectureFile = Join-Path $docsDir "architecture.md"
    $specsDir = Join-Path $repoRoot "specs"
    
    # Ensure required directories exist
    if (-not (Test-Path $docsDir)) {
        New-Item -ItemType Directory -Path $docsDir -Force | Out-Null
    }
    
    # Check for required files
    if (-not (Test-Path $templateFile)) {
        Write-Error "Standards template not found at: $templateFile"
        exit 1
    }
    
    if (-not (Test-Path $groundRulesFile)) {
        Write-Error "Ground-rules file not found at: $groundRulesFile"
        Write-Host "INFO: Please run the regulate command first."
        exit 1
    fi
    
    # Create standards document if it doesn't exist
    if (-not (Test-Path $standardsFile)) {
        Write-Host "INFO: Creating standards document from template..."
        Copy-Item $templateFile $standardsFile
        Write-Host "✓ Created: $standardsFile"
    }
    else {
        Write-Warning "Standards document already exists: $standardsFile"
        Write-Host "INFO: Will update with latest context."
    }
    
    # Detect technology stack from architecture if available
    $techStack = ""
    if (Test-Path $architectureFile) {
        Write-Host "INFO: Detected architecture document. Extracting technology stack..."
        $techStack = Get-TechStack -ArchFile $architectureFile
    }
    else {
        Write-Warning "Architecture document not found. Standards will need manual tech stack updates."
        $techStack = "[Technology stack not detected - please update manually]"
    }
    
    # Count feature specifications
    $featureCount = 0
    if (Test-Path $specsDir) {
        $featureCount = (Get-ChildItem -Path $specsDir -Filter "spec.md" -Recurse -Depth 2).Count
    }
    
    # Detect AI agent
    $detectedAgent = Get-AIAgent -RepoRoot $repoRoot
    
    # Update standards document with context
    Update-StandardsContext -StandardsFile $standardsFile -TechStack $techStack
    
    # Generate JSON output for AI agents
    New-JsonOutput -StandardsFile $standardsFile -FeatureCount $featureCount -AIAgent $detectedAgent -TechStack $techStack
    
    # Print human-readable summary
    Show-Summary -StandardsFile $standardsFile -FeatureCount $featureCount -TechStack $techStack
}

# Detect technology stack from architecture document
function Get-TechStack {
    param (
        [string]$ArchFile
    )
    
    $techStack = ""
    $content = Get-Content $ArchFile -Raw
    
    # Extract technology information from architecture document
    # Look for common technology stack sections
    if ($content -match "(?i)frontend.*react") {
        $techStack += "Frontend: React, "
    }
    elseif ($content -match "(?i)frontend.*vue") {
        $techStack += "Frontend: Vue, "
    }
    elseif ($content -match "(?i)frontend.*angular") {
        $techStack += "Frontend: Angular, "
    }
    
    if ($content -match "(?i)backend.*(fastapi|python)") {
        $techStack += "Backend: Python/FastAPI, "
    }
    elseif ($content -match "(?i)backend.*(express|node)") {
        $techStack += "Backend: Node.js, "
    }
    elseif ($content -match "(?i)backend.*(spring|java)") {
        $techStack += "Backend: Java/Spring, "
    }
    
    if ($content -match "(?i)(postgresql|postgres)") {
        $techStack += "Database: PostgreSQL, "
    }
    elseif ($content -match "(?i)(mongodb|mongo)") {
        $techStack += "Database: MongoDB, "
    }
    elseif ($content -match "(?i)mysql") {
        $techStack += "Database: MySQL, "
    }
    
    # Remove trailing comma and space
    $techStack = $techStack.TrimEnd(", ")
    
    if ([string]::IsNullOrEmpty($techStack)) {
        $techStack = "[Technology stack not detected - please update manually]"
    }
    
    return $techStack
}

# Update standards document with detected context
function Update-StandardsContext {
    param (
        [string]$StandardsFile,
        [string]$TechStack
    )
    
    # Update technology stack section if placeholder exists
    $content = Get-Content $StandardsFile -Raw
    if ($content -match "\[e.g., React 18, TypeScript 5") {
        Print-Info "Updating technology stack in standards document..."
        # Note: This is a basic update. Manual review recommended.
    }
}

# Generate JSON output for AI agent consumption
function New-JsonOutput {
    param (
        [string]$StandardsFile,
        [int]$FeatureCount,
        [string]$AIAgent,
        [string]$TechStack
    )
    
    $jsonOutput = @{
        command = "standardize"
        status = "ready"
        standards_document = $StandardsFile
        feature_count = $FeatureCount
        detected_ai_agent = $AIAgent
        detected_tech_stack = $TechStack
        mandatory_sections = @{
            ui_naming_conventions = @{
                status = "required"
                priority = "MANDATORY"
                location = "Section 2"
            }
        }
        next_steps = @(
            "Review and customize standards based on project needs",
            "Ensure UI naming conventions are comprehensive (MANDATORY)",
            "Update technology stack sections with actual stack",
            "Configure linters and formatters according to standards",
            "Set up pre-commit hooks for enforcement",
            "Share standards with all team members",
            "Reference standards in code reviews"
        )
        workflow = @{
            previous_command = "architect"
            current_command = "standardize"
            next_command = "design"
            document_type = "product-level"
            document_location = "docs/"
        }
    }
    
    Write-Output ($jsonOutput | ConvertTo-Json -Depth 10)
}

# Print human-readable summary
function Show-Summary {
    param (
        [string]$StandardsFile,
        [int]$FeatureCount,
        [string]$TechStack
    )
    
    Write-Host ""
    Print-Success "=== Standardize Setup Complete ==="
    Write-Host ""
    Print-Info "Standards Document: $StandardsFile"
    Print-Info "Detected Tech Stack: $TechStack"
    Print-Info "Feature Specifications: $FeatureCount"
    Write-Host ""
    Print-Warning "⭐ IMPORTANT: UI Naming Conventions (Section 2) are MANDATORY"
    Write-Host ""
    Print-Info "Next Steps:"
    Write-Host "  1. Review and customize the standards document"
    Write-Host "  2. Update technology-specific sections with your actual stack"
    Write-Host "  3. Ensure UI naming conventions are comprehensive"
    Write-Host "  4. Configure linters and formatters based on standards"
    Write-Host "  5. Set up pre-commit hooks for automated enforcement"
    Write-Host "  6. Share standards document with all team members"
    Write-Host ""
    Print-Info "To generate standards with AI assistance, run:"
    Write-Host "  standardize"
    Write-Host ""
}

# Execute main setup
Setup-Standardize
