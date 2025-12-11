# setup-design-e2e-test.ps1
# Sets up the design-e2e-test command workflow for Spec-Driven Development
# This script creates the E2E test plan document from template and prepares the environment

$ErrorActionPreference = "Stop"

# Get the directory where this script is located
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

# Source common utilities
. (Join-Path $scriptDir "common.ps1")

# Main setup function
function Setup-DesignE2ETest {
    $repoRoot = Get-RepoRoot
    
    # Navigate to repository root
    Set-Location $repoRoot
    
    # Define paths
    $docsDir = Join-Path $repoRoot "docs"
    $e2eTestFile = Join-Path $docsDir "e2e-test-plan.md"
    $templateFile = Join-Path $repoRoot ".rainbow" "templates" "templates-for-commands" "e2e-test-template.md"
    $groundRulesFile = Join-Path $repoRoot ".rainbow" "memory" "ground-rules.md"
    $architectureFile = Join-Path $docsDir "architecture.md"
    $specsDir = Join-Path $repoRoot "specs"
    
    # Ensure required directories exist
    if (-not (Test-Path $docsDir)) {
        New-Item -ItemType Directory -Path $docsDir -Force | Out-Null
    }
    
    # Check for required files
    if (-not (Test-Path $templateFile)) {
        Write-Error "E2E test template not found at: $templateFile"
        exit 1
    }
    
    if (-not (Test-Path $groundRulesFile)) {
        Write-Error "Ground-rules file not found at: $groundRulesFile"
        Write-Host "INFO: Please run the regulate command first."
        exit 1
    }
    
    if (-not (Test-Path $architectureFile)) {
        Print-Error "Architecture file not found at: $architectureFile"
        Print-Info "Please run the architect command first."
        exit 1
    }
    
    # Create E2E test document if it doesn't exist
    if (-not (Test-Path $e2eTestFile)) {
        Print-Info "Creating E2E test plan document from template..."
        Copy-Item $templateFile $e2eTestFile
        Print-Success "Created: $e2eTestFile"
    }
    else {
        Print-Warning "E2E test plan document already exists: $e2eTestFile"
        Print-Info "Will update with latest context."
    }
    
    # Count feature specifications
    $featureCount = 0
    if (Test-Path $specsDir) {
        $featureCount = (Get-ChildItem -Path $specsDir -Filter "spec.md" -Recurse -Depth 2).Count
    }
    
    # Extract critical integration points from architecture
    $integrationInfo = ""
    if (Test-Path $architectureFile) {
        Print-Info "Analyzing architecture for integration points..."
        $integrationInfo = Get-IntegrationPoints -ArchFile $architectureFile
    }
    
    # Detect AI agent
    $detectedAgent = Detect-AIAgent -RepoRoot $repoRoot
    
    # Generate JSON output for AI agents
    New-JsonOutput -E2ETestFile $e2eTestFile -FeatureCount $featureCount -AIAgent $detectedAgent -IntegrationInfo $integrationInfo
    
    # Print human-readable summary
    Show-Summary -E2ETestFile $e2eTestFile -FeatureCount $featureCount -IntegrationInfo $integrationInfo
}

# Extract integration points from architecture document
function Get-IntegrationPoints {
    param (
        [string]$ArchFile
    )
    
    $integrationPoints = ""
    $content = Get-Content $ArchFile -Raw
    
    # Look for common integration keywords in architecture
    if ($content -match "(?i)(external.*system|api.*integration|microservice|third.*party)") {
        $integrationPoints += "External integrations detected. "
    }
    
    if ($content -match "(?i)(database|postgresql|mongodb|mysql)") {
        $integrationPoints += "Database layer present. "
    }
    
    if ($content -match "(?i)(frontend|react|vue|angular)") {
        $integrationPoints += "Frontend UI layer present. "
    }
    
    if ($content -match "(?i)(mobile|ios|android|react native|flutter)") {
        $integrationPoints += "Mobile app layer present. "
    }
    
    if ([string]::IsNullOrEmpty($integrationPoints)) {
        $integrationPoints = "Integration points need manual identification from architecture"
    }
    
    return $integrationPoints.TrimEnd()
}

# Generate JSON output for AI agent consumption
function New-JsonOutput {
    param (
        [string]$E2ETestFile,
        [int]$FeatureCount,
        [string]$AIAgent,
        [string]$IntegrationInfo
    )
    
    $jsonOutput = @{
        command = "design-e2e-test"
        status = "ready"
        e2e_test_document = $E2ETestFile
        feature_count = $FeatureCount
        detected_ai_agent = $AIAgent
        integration_analysis = $IntegrationInfo
        prerequisites = @{
            architecture_doc = "docs/architecture.md"
            ground_rules = "memory/ground-rules.md"
            feature_specs = "specs/*/spec.md"
        }
        next_steps = @(
            "Review architecture.md to identify system components and integration points",
            "Extract critical user journeys from feature specifications",
            "Design test scenarios covering end-to-end workflows",
            "Define test data management and environment strategy",
            "Select appropriate testing framework and tools",
            "Create detailed test scenarios with expected results",
            "Plan test execution schedule and CI/CD integration",
            "Document test reporting and metrics strategy"
        )
        workflow = @{
            previous_command = "architect"
            current_command = "design-e2e-test"
            next_command = "standardize"
            document_type = "product-level"
            document_location = "docs/"
        }
        output_files = @(
            "docs/e2e-test-plan.md",
            "docs/e2e-test-scenarios.md",
            "docs/test-data-guide.md",
            "docs/e2e-test-setup.md"
        )
    }
    
    Write-Output ($jsonOutput | ConvertTo-Json -Depth 10)
}

# Print human-readable summary
function Show-Summary {
    param (
        [string]$E2ETestFile,
        [int]$FeatureCount,
        [string]$IntegrationInfo
    )
    
    Write-Host ""
    Print-Success "=== Design E2E Test Setup Complete ==="
    Write-Host ""
    Print-Info "E2E Test Document: $E2ETestFile"
    Print-Info "Feature Specifications: $FeatureCount"
    Print-Info "Integration Analysis: $IntegrationInfo"
    Write-Host ""
    Print-Info "Prerequisites Met:"
    Write-Host "  ✓ Architecture document available"
    Write-Host "  ✓ Ground-rules constraints loaded"
    Write-Host "  ✓ Feature specifications available"
    Write-Host ""
    Print-Info "Next Steps:"
    Write-Host "  1. Review architecture to identify system integration points"
    Write-Host "  2. Extract critical user journeys from feature specs"
    Write-Host "  3. Design comprehensive E2E test scenarios"
    Write-Host "  4. Define test data management strategy"
    Write-Host "  5. Select testing framework (Playwright, Cypress, etc.)"
    Write-Host "  6. Plan test execution and CI/CD integration"
    Write-Host "  7. Set up test reporting and monitoring"
    Write-Host ""
    Print-Info "To generate E2E test plan with AI assistance, run:"
    Write-Host "  design-e2e-test"
    Write-Host ""
}

# Execute main setup
Setup-DesignE2ETest
