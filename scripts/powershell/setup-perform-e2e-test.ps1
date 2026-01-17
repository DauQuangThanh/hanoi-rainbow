# setup-perform-e2e-test.ps1
# Sets up the perform-e2e-test command workflow for Spec-Driven Development
# This script prepares the test execution environment and validates prerequisites

param(
    [string]$TestMode = "smoke"
)

$ErrorActionPreference = 'Stop'

# Get the directory where this script is located
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

# Source common utilities
. (Join-Path $ScriptDir 'common.ps1')

function Detect-TestFramework {
    param([string]$PlanFile)
    
    $framework = "Unknown"
    
    if (Test-Path $PlanFile) {
        $content = Get-Content $PlanFile -Raw
        
        if ($content -match 'playwright') {
            $framework = "Playwright"
        }
        elseif ($content -match 'cypress') {
            $framework = "Cypress"
        }
        elseif ($content -match 'selenium') {
            $framework = "Selenium"
        }
        elseif ($content -match 'appium') {
            $framework = "Appium"
        }
    }
    
    return $framework
}

function Generate-JsonOutput {
    param(
        [string]$E2ETestPlan,
        [string]$TestResultsDir,
        [string]$Timestamp,
        [string]$TestMode,
        [int]$TotalScenarios,
        [string]$TestFramework,
        [bool]$FrameworkInstalled,
        [string]$AIAgent
    )
    
    $resultFile = Join-Path $TestResultsDir "e2e-test-result_$Timestamp.md"
    
    $output = @{
        command = "perform-e2e-test"
        status = "ready"
        timestamp = $Timestamp
        test_mode = $TestMode
        e2e_test_plan = $E2ETestPlan
        test_results_dir = $TestResultsDir
        result_file = $resultFile
        total_scenarios = $TotalScenarios
        detected_test_framework = $TestFramework
        framework_installed = $FrameworkInstalled
        detected_ai_agent = $AIAgent
        execution_modes = @{
            smoke = "P0 critical scenarios only (~10-15 min)"
            regression = "P0 + P1 scenarios (~1 hour)"
            full = "All scenarios P0-P3 (~3-4 hours)"
        }
        prerequisites = @{
            e2e_test_plan = $E2ETestPlan
            test_framework = $TestFramework
            framework_installed = $FrameworkInstalled
        }
        next_steps = @(
            "Parse E2E test plan to extract test scenarios",
            "Generate test scripts from scenarios (if not exists)",
            "Set up test environment and test data",
            "Execute $TestMode test suite",
            "Capture test results, screenshots, and logs",
            "Generate detailed test result report: $resultFile",
            "Analyze failures and provide recommendations",
            "Clean up test environment"
        )
        output_files = @{
            main_report = $resultFile
            screenshots_dir = Join-Path $TestResultsDir "screenshots"
            videos_dir = Join-Path $TestResultsDir "videos"
            logs_dir = Join-Path $TestResultsDir "logs"
            json_results = Join-Path $TestResultsDir "results.json"
            junit_xml = Join-Path $TestResultsDir "junit.xml"
        }
        test_suite_breakdown = @{
            smoke = @{
                priority = "P0"
                estimated_duration = "10-15 minutes"
                use_case = "Quick validation, every commit"
            }
            regression = @{
                priority = "P0 + P1"
                estimated_duration = "1 hour"
                use_case = "Daily/nightly builds"
            }
            full = @{
                priority = "P0 + P1 + P2 + P3"
                estimated_duration = "3-4 hours"
                use_case = "Weekly, pre-release"
            }
        }
    }
    
    return $output | ConvertTo-Json -Depth 10
}

function Print-Summary {
    param(
        [string]$E2ETestPlan,
        [string]$TestResultsDir,
        [string]$Timestamp,
        [string]$TestMode,
        [int]$TotalScenarios,
        [string]$TestFramework,
        [bool]$FrameworkInstalled
    )
    
    Write-Host ""
    Write-Success "=== Perform E2E Test Setup Complete ==="
    Write-Host ""
    Write-Info "Test Run Timestamp: $Timestamp"
    Write-Info "Test Mode: $TestMode"
    Write-Info "E2E Test Plan: $E2ETestPlan"
    Write-Info "Test Results Directory: $TestResultsDir"
    Write-Info "Total Scenarios in Plan: $TotalScenarios"
    Write-Info "Detected Test Framework: $TestFramework"
    
    if ($FrameworkInstalled) {
        Write-Success "✓ Test Framework Installed"
    }
    else {
        Write-Warning "⚠ Test Framework Not Detected - May need installation"
    }
    
    Write-Host ""
    Write-Info "Test Suite Modes:"
    Write-Host "  • smoke:      P0 critical scenarios only (~10-15 min)"
    Write-Host "  • regression: P0 + P1 scenarios (~1 hour)"
    Write-Host "  • full:       All scenarios P0-P3 (~3-4 hours)"
    Write-Host ""
    Write-Info "Result File Will Be:"
    Write-Host "  $TestResultsDir\e2e-test-result_$Timestamp.md"
    Write-Host ""
    Write-Info "Next Steps:"
    Write-Host "  1. Parse E2E test plan to extract scenarios"
    Write-Host "  2. Generate test scripts from scenarios (if needed)"
    Write-Host "  3. Set up test environment and seed test data"
    Write-Host "  4. Execute $TestMode test suite"
    Write-Host "  5. Capture results, screenshots, and logs"
    Write-Host "  6. Generate detailed test result report"
    Write-Host "  7. Analyze failures and provide recommendations"
    Write-Host "  8. Clean up test environment"
    Write-Host ""
    
    if (-not $FrameworkInstalled) {
        Write-Warning "NOTE: Test framework may need to be installed before execution"
        Write-Host "  Install commands (if needed):"
        Write-Host "    npm install -D @playwright/test"
        Write-Host "    npm install -D cypress"
        Write-Host ""
    }
    
    Write-Info "To execute E2E tests with AI assistance, run:"
    Write-Host "  perform-e2e-test $TestMode"
    Write-Host ""
}

# Main setup function
function Setup-PerformE2ETest {
    param([string]$TestMode)
    
    # Find repository root
    $repoRoot = Get-RepoRoot
    
    # Navigate to repository root
    Set-Location $repoRoot
    
    # Define paths
    $docsDir = Join-Path $repoRoot 'docs'
    $e2eTestPlan = Join-Path $docsDir 'e2e-test-plan.md'
    $testResultsDir = Join-Path $repoRoot 'test-results'
    $testsE2EDir = Join-Path $repoRoot 'tests\e2e'
    $groundRulesFile = Join-Path $repoRoot '.rainbow\memory\ground-rules.md'
    $architectureFile = Join-Path $docsDir 'architecture.md'
    
    # Generate timestamp for this test run
    $timestamp = Get-Date -Format 'yyyyMMdd_HHmmss'
    
    # Check for required files
    if (-not (Test-Path $e2eTestPlan)) {
        Write-Error "E2E test plan not found at: $e2eTestPlan"
        Write-Host "INFO: Please run the design-e2e-test command first."
        exit 1
    }
    
    if (-not (Test-Path $groundRulesFile)) {
        Write-Warning "Ground-rules file not found at: $groundRulesFile"
    }
    
    if (-not (Test-Path $architectureFile)) {
        Write-Warning "Architecture file not found at: $architectureFile"
    }
    
    # Create test results directory structure
    Write-Info "Setting up test results directory..."
    $subDirs = @('screenshots', 'videos', 'logs', 'archive')
    foreach ($subDir in $subDirs) {
        $dirPath = Join-Path $testResultsDir $subDir
        if (-not (Test-Path $dirPath)) {
            New-Item -ItemType Directory -Path $dirPath -Force | Out-Null
        }
    }
    
    # Create tests/e2e directory structure if it doesn't exist
    if (-not (Test-Path $testsE2EDir)) {
        Write-Info "Creating E2E test directory structure..."
        $testSubDirs = @('scenarios', 'pages', 'helpers', 'fixtures', 'config')
        foreach ($subDir in $testSubDirs) {
            $dirPath = Join-Path $testsE2EDir $subDir
            New-Item -ItemType Directory -Path $dirPath -Force | Out-Null
        }
        
        # Create basic README
        $readmeContent = @"
# End-to-End Tests

This directory contains end-to-end test scripts generated from the E2E test plan.

## Structure

- ``scenarios/`` - Test scenario implementations
- ``pages/`` - Page Object Models
- ``helpers/`` - Test utility functions
- ``fixtures/`` - Test data fixtures
- ``config/`` - Test configuration files

## Running Tests

See ``docs/e2e-test-plan.md`` for test execution instructions.

## Generated by

Hanoi Rainbow ``perform-e2e-test`` command
"@
        $readmePath = Join-Path $testsE2EDir 'README.md'
        Set-Content -Path $readmePath -Value $readmeContent
        Write-Success "Created: $testsE2EDir"
    }
    
    # Validate test mode
    $validModes = @('smoke', 'regression', 'full')
    if ($TestMode -notin $validModes) {
        $TestMode = 'smoke'
    }
    
    # Count test scenarios in the plan
    $totalScenarios = 0
    if (Test-Path $e2eTestPlan) {
        $scenarios = Select-String -Path $e2eTestPlan -Pattern '^#### Scenario' -AllMatches
        $totalScenarios = $scenarios.Matches.Count
    }
    
    # Detect test framework from plan
    $testFramework = Detect-TestFramework -PlanFile $e2eTestPlan
    
    # Detect AI agents (supports multiple agents)
    $detectedAgent = Detect-AllAIAgents -RepoRoot $repoRoot
    
    # Check if test framework is installed
    $frameworkInstalled = $false
    $npxAvailable = Get-Command npx -ErrorAction SilentlyContinue
    
    if ($npxAvailable) {
        switch ($testFramework) {
            'Playwright' {
                try {
                    npx playwright --version 2>&1 | Out-Null
                    $frameworkInstalled = $true
                }
                catch {
                    $frameworkInstalled = $false
                }
            }
            'Cypress' {
                try {
                    npx cypress --version 2>&1 | Out-Null
                    $frameworkInstalled = $true
                }
                catch {
                    $frameworkInstalled = $false
                }
            }
        }
    }
    
    # Generate JSON output for AI agents
    $jsonOutput = Generate-JsonOutput -E2ETestPlan $e2eTestPlan -TestResultsDir $testResultsDir `
        -Timestamp $timestamp -TestMode $TestMode -TotalScenarios $totalScenarios `
        -TestFramework $testFramework -FrameworkInstalled $frameworkInstalled -AIAgent $detectedAgent
    
    Write-Output $jsonOutput
    
    # Print human-readable summary
    Print-Summary -E2ETestPlan $e2eTestPlan -TestResultsDir $testResultsDir -Timestamp $timestamp `
        -TestMode $TestMode -TotalScenarios $totalScenarios -TestFramework $testFramework `
        -FrameworkInstalled $frameworkInstalled
}

# Execute main setup with test mode parameter
Setup-PerformE2ETest -TestMode $TestMode
