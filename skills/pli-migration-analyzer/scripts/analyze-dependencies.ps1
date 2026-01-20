#!/usr/bin/env pwsh
#
# Analyze dependencies between PL/I programs, include files, and external procedures.
# This script scans a directory of PL/I programs and generates a dependency graph.
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
    include_files = @()
    files = @()
} | ConvertTo-Json -Depth 3

Set-Content -Path $OutputFile -Value $initialJson -Encoding UTF8

# Function to extract program name from PL/I file
function Get-ProgramName {
    param([string]$FilePath)
    # PL/I programs use filename as program name
    return (Get-Item $FilePath).BaseName
}

# Function to extract CALL statements
function Get-CallStatements {
    param([string]$FilePath)
    
    $calls = @()
    $content = Get-Content $FilePath -ErrorAction SilentlyContinue
    if ($content) {
        foreach ($line in $content) {
            if ($line -match 'CALL\s+([A-Za-z0-9_]+)') {
                $calls += $matches[1]
            }
        }
    }
    return $calls
}

# Function to extract %INCLUDE statements
function Get-CopyStatements {
    param([string]$FilePath)
    
    $copies = @()
    $content = Get-Content $FilePath -ErrorAction SilentlyContinue
    if ($content) {
        foreach ($line in $content) {
            if ($line -match '%INCLUDE\s+([A-Za-z0-9_/.]+)') {
                $copies += $matches[1] -replace "[';\"]",''
            }
        }
    }
    return $copies
}

# Function to extract file names from OPEN/CLOSE statements
function Get-FileReferences {
    param([string]$FilePath)
    
    $fileRefs = @()
    $content = Get-Content $FilePath -ErrorAction SilentlyContinue
    if ($content) {
        foreach ($line in $content) {
            if ($line -match 'FILE\(([A-Za-z0-9_]+)\)') {
                $fileRefs += $matches[1]
            }
        }
    }
    return $fileRefs
}

# Find all PL/I files
Write-Host "Scanning for PL/I files..."
$pliFiles = Get-ChildItem -Path $SourceDir -Recurse -Include "*.pli","*.PLI","*.pl1" -File -ErrorAction SilentlyContinue

if (-not $pliFiles -or $pliFiles.Count -eq 0) {
    Write-Host "No PL/I files found in $SourceDir"
    exit 1
}

Write-Host "Found $($pliFiles.Count) PL/I files"

# Collections for data
$programs = @()
$dependencies = @()
$includeFiles = @()
$files = @()

# Process each PL/I file
foreach ($pliFile in $pliFiles) {
    Write-Host "Processing: $($pliFile.FullName)"
    
    $programName = Get-ProgramName -FilePath $pliFile.FullName
    
    # Add program to list
    $programs += @{
        name = $programName
        file = $pliFile.FullName
    }
    
    # Extract calls
    $calls = Get-CallStatements -FilePath $pliFile.FullName
    foreach ($calledProgram in $calls) {
        if ($calledProgram) {
            $dependencies += @{
                from = $programName
                to = $calledProgram
                type = "call"
            }
        }
    }
    
    # Extract %INCLUDE files
    $copies = Get-CopyStatements -FilePath $pliFile.FullName
    foreach ($includeFile in $copies) {
        if ($includeFile) {
            $includeFiles += @{
                name = $includeFile
                used_by = $programName
            }
            $dependencies += @{
                from = $programName
                to = $includeFile
                type = "include"
            }
        }
    }
    
    # Extract file references
    $fileRefs = Get-FileReferences -FilePath $pliFile.FullName
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
$uniqueIncludes = $includeFiles | Select-Object -Property name -Unique
$uniqueFiles = $files | Select-Object -Property name -Unique

$result = @{
    programs = $programs
    dependencies = $dependencies
    include_files = $uniqueIncludes
    files = $uniqueFiles
    summary = @{
        total_programs = $programs.Count
        total_dependencies = $dependencies.Count
        total_includes = $uniqueIncludes.Count
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
Write-Host "  Include files: $($uniqueIncludes.Count)"
Write-Host "  Files: $($uniqueFiles.Count)"
