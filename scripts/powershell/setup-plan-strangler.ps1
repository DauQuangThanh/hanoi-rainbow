#!/usr/bin/env pwsh
# Setup strangler pattern implementation plan for a feature

[CmdletBinding()]
param(
    [switch]$Json,
    [switch]$Help
)

$ErrorActionPreference = 'Stop'

# Show help if requested
if ($Help) {
    Write-Output "Usage: ./setup-plan-strangler.ps1 [-Json] [-Help]"
    Write-Output "  -Json     Output results in JSON format"
    Write-Output "  -Help     Show this help message"
    exit 0
}

# Load common functions
. "$PSScriptRoot/common.ps1"

# Get all paths and variables from common functions
$paths = Get-FeaturePathsEnv

# Check if we're on a proper feature branch (only for git repos)
if (-not (Test-FeatureBranch -Branch $paths.CURRENT_BRANCH -HasGit $paths.HAS_GIT)) {
    exit 1
}

# Ensure the feature directory exists
New-Item -ItemType Directory -Path $paths.FEATURE_DIR -Force | Out-Null

# Set the strangler plan file path
$stranglerPlan = Join-Path $paths.FEATURE_DIR 'strangler-plan.md'

# Copy template if it exists
$template = Join-Path $paths.REPO_ROOT '.rainbow/templates/templates-for-agents/strangler-pattern-implementation-template.md'
if (Test-Path $template) {
    Copy-Item $template $stranglerPlan -Force
    Write-Output "Copied strangler pattern implementation template to $stranglerPlan"
} else {
    Write-Warning "Strangler pattern implementation template not found at $template"
    New-Item -ItemType File -Path $stranglerPlan -Force | Out-Null
}

# Output results
if ($Json) {
    $result = [PSCustomObject]@{
        FEATURE_SPEC = $paths.FEATURE_SPEC
        STRANGLER_PLAN = $stranglerPlan
        SPECS_DIR = $paths.FEATURE_DIR
        BRANCH = $paths.CURRENT_BRANCH
        HAS_GIT = $paths.HAS_GIT
    }
    $result | ConvertTo-Json -Compress
} else {
    Write-Output "FEATURE_SPEC: $($paths.FEATURE_SPEC)"
    Write-Output "STRANGLER_PLAN: $stranglerPlan"
    Write-Output "SPECS_DIR: $($paths.FEATURE_DIR)"
    Write-Output "BRANCH: $($paths.CURRENT_BRANCH)"
    Write-Output "HAS_GIT: $($paths.HAS_GIT)"
}
