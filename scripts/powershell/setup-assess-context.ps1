#!/usr/bin/env pwsh
# Setup context assessment for brownfield project

[CmdletBinding()]
param(
    [switch]$Json,
    [switch]$Help
)

$ErrorActionPreference = 'Stop'

# Show help if requested
if ($Help) {
    Write-Output "Usage: ./setup-assess-context.ps1 [-Json] [-Help]"
    Write-Output "  -Json     Output results in JSON format"
    Write-Output "  -Help     Show this help message"
    exit 0
}

# Load common functions
. "$PSScriptRoot/common.ps1"

# Get repository root
$repoRoot = Get-RepoRoot

# Ensure the docs directory exists
$docsDir = Join-Path $repoRoot 'docs'
New-Item -ItemType Directory -Path $docsDir -Force | Out-Null

# Set the context assessment file path (project-level, not feature-specific)
$contextAssessment = Join-Path $docsDir 'context-assessment.md'

# Copy template if it exists
$template = Join-Path $rainbowDir 'templates/templates-for-commands/context-assessment-template.md'
if (Test-Path $template) {
    Copy-Item $template $contextAssessment -Force
    Write-Output "Copied context assessment template to $contextAssessment"
} else {
    Write-Warning "Context assessment template not found at $template"
    New-Item -ItemType File -Path $contextAssessment -Force | Out-Null
}

# Check if we're in a git repo
$hasGit = 'false'
if (Test-Path (Join-Path $repoRoot '.git')) {
    $hasGit = 'true'
}

# Output results
if ($Json) {
    $result = [PSCustomObject]@{
        CONTEXT_ASSESSMENT = $contextAssessment
        DOCS_DIR = $docsDir
        REPO_ROOT = $repoRoot
        HAS_GIT = $hasGit
    }
    $result | ConvertTo-Json -Compress
} else {
    Write-Output "CONTEXT_ASSESSMENT: $contextAssessment"
    Write-Output "DOCS_DIR: $docsDir"
    Write-Output "REPO_ROOT: $repoRoot"
    Write-Output "HAS_GIT: $hasGit"
}
