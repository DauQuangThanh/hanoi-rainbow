#!/usr/bin/env pwsh
#
# Analyze dependencies between COBOL programs, copybooks, and files.
# This script scans a directory of COBOL programs and generates a dependency graph.
# PowerShell version for Windows/Cross-platform support

param(
    [string]$SourceDir = "."
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$OutputFile = "dependencies.json"

Write-Host "Analyzing dependencies in: $SourceDir"
Write-Host "Output will be written to: $OutputFile"

# Initialize JSON structure
$initialJson = @{
    programs = @()
    dependencies = @()
    copybooks = @()
    files = @()
} | ConvertTo-Json -Depth 3

Set-Content -Path $OutputFile -Value $initialJson -Encoding UTF8

# Function to extract program name from COBOL file
function Get-ProgramName {
    param([string]$FilePath)
    
    $content = Get-Content $FilePath -ErrorAction SilentlyContinue
    if ($content) {
        foreach ($line in $content) {
            if ($line -match 'PROGRAM-ID[\.\s]+([A-Za-z0-9-]+)') {
                return $matches[1]
            }
        }
    }
    return (Get-Item $FilePath).BaseName
}

# Function to extract CALL statements
function Get-CallStatements {
    param([string]$FilePath)
    
    $calls = @()
    $content = Get-Content $FilePath -ErrorAction SilentlyContinue
    if ($content) {
        foreach ($line in $content) {
            if ($line -match 'CALL.*[''"]([A-Z0-9-]+)[''"]') {
                $calls += $matches[1]
            }
        }
    }
    return $calls
}

# Function to extract COPY statements
function Get-CopyStatements {
    param([string]$FilePath)
    
    $copies = @()
    $content = Get-Content $FilePath -ErrorAction SilentlyContinue
    if ($content) {
        foreach ($line in $content) {
            if ($line -match 'COPY\s+([A-Z0-9-]+)') {
                $copies += $matches[1] -replace '\.$', ''
            }
        }
    }
    return $copies
}

# Function to extract SELECT/ASSIGN file names
function Get-FileReferences {
    param([string]$FilePath)
    
    $fileRefs = @()
    $content = Get-Content $FilePath -ErrorAction SilentlyContinue
    if ($content) {
        foreach ($line in $content) {
            if ($line -match 'SELECT\s+([A-Z0-9-]+)') {
                $fileRefs += $matches[1]
            }
        }
    }
    return $fileRefs
}

# Find all COBOL files
Write-Host "Scanning for COBOL files..."
$cobolFiles = Get-ChildItem -Path $SourceDir -Recurse -Include "*.cbl","*.CBL","*.cob","*.COB" -File -ErrorAction SilentlyContinue

if (-not $cobolFiles -or $cobolFiles.Count -eq 0) {
    Write-Host "No COBOL files found in $SourceDir"
    exit 1
}

Write-Host "Found $($cobolFiles.Count) COBOL files"

# Collections for data
$programs = @()
$dependencies = @()
$copybooks = @()
$files = @()

# Process each COBOL file
foreach ($cobolFile in $cobolFiles) {
    Write-Host "Processing: $($cobolFile.FullName)"
    
    $programName = Get-ProgramName -FilePath $cobolFile.FullName
    
    # Add program to list
    $programs += @{
        name = $programName
        file = $cobolFile.FullName
    }
    
    # Extract calls
    $calls = Get-CallStatements -FilePath $cobolFile.FullName
    foreach ($calledProgram in $calls) {
        if ($calledProgram) {
            $dependencies += @{
                from = $programName
                to = $calledProgram
                type = "call"
            }
        }
    }
    
    # Extract copybooks
    $copies = Get-CopyStatements -FilePath $cobolFile.FullName
    foreach ($copybook in $copies) {
        if ($copybook) {
            $copybooks += @{
                name = $copybook
                used_by = $programName
            }
            $dependencies += @{
                from = $programName
                to = $copybook
                type = "copy"
            }
        }
    }
    
    # Extract file references
    $fileRefs = Get-FileReferences -FilePath $cobolFile.FullName
    foreach ($fileRef in $fileRefs) {
        if ($fileRef) {
            $files += @{
                name = $fileRef
                used_by = $programName
            }
            $dependencies += @{
                from = $programName
                to = $fileRef
                type = "file"
            }
        }
    }
}

# Build final JSON with summary
$uniqueCopybooks = $copybooks | Select-Object -Property name -Unique
$uniqueFiles = $files | Select-Object -Property name -Unique

$result = @{
    programs = $programs
    dependencies = $dependencies
    copybooks = $uniqueCopybooks
    files = $uniqueFiles
    summary = @{
        total_programs = $programs.Count
        total_dependencies = $dependencies.Count
        total_copybooks = $uniqueCopybooks.Count
        total_files = $uniqueFiles.Count
    }
}

# Write to file
$result | ConvertTo-Json -Depth 10 | Set-Content -Path $OutputFile -Encoding UTF8

Write-Host ""
Write-Host "Analysis complete! Results written to: $OutputFile"
Write-Host ""
Write-Host "Summary:"
Write-Host "  Programs: $($programs.Count)"
Write-Host "  Dependencies: $($dependencies.Count)"
Write-Host "  Copybooks: $($uniqueCopybooks.Count)"
Write-Host "  Files: $($uniqueFiles.Count)"
