#!/usr/bin/env pwsh
# Setup business logic mapping for a feature

[CmdletBinding()]
param(
    [switch]$Json,
    [switch]$Help
)

$ErrorActionPreference = 'Stop'

# Show help if requested
if ($Help) {
    Write-Output "Usage: ./setup-map-business-logic.ps1 [-Json] [-Help]"
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

# Set the business logic map file path
$businessLogicMap = Join-Path $paths.FEATURE_DIR 'business-logic-map.md'

# Copy template if it exists
$template = Join-Path $paths.REPO_ROOT '.rainbow/templates/templates-for-agents/business-logic-extraction-template.md'
if (Test-Path $template) {
    Copy-Item $template $businessLogicMap -Force
    Write-Output "Copied business logic extraction template to $businessLogicMap"
} else {
    Write-Warning "Business logic extraction template not found at $template"
    New-Item -ItemType File -Path $businessLogicMap -Force | Out-Null
}

# Output results
if ($Json) {
    $result = [PSCustomObject]@{
        FEATURE_SPEC = $paths.FEATURE_SPEC
        BUSINESS_LOGIC_MAP = $businessLogicMap
        SPECS_DIR = $paths.FEATURE_DIR
        BRANCH = $paths.CURRENT_BRANCH
        HAS_GIT = $paths.HAS_GIT
    }
    $result | ConvertTo-Json -Compress
} else {
    Write-Output "FEATURE_SPEC: $($paths.FEATURE_SPEC)"
    Write-Output "BUSINESS_LOGIC_MAP: $businessLogicMap"
    Write-Output "SPECS_DIR: $($paths.FEATURE_DIR)"
    Write-Output "BRANCH: $($paths.CURRENT_BRANCH)"
    Write-Output "HAS_GIT: $($paths.HAS_GIT)"
}
