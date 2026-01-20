#!/usr/bin/env pwsh
#
# Analyze dependencies between RPG programs, service programs, and files.
# This script scans a directory of RPG programs and generates a dependency graph.
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
    copy_members = @()
    files = @()
} | ConvertTo-Json -Depth 3

Set-Content -Path $OutputFile -Value $initialJson -Encoding UTF8

# Function to extract program name from RPG file
function Get-ProgramName {
    param([string]$FilePath)
    # Use filename as program name for RPG
    return (Get-Item $FilePath).BaseName
}

# Function to extract CALLB/CALLP statements
function Get-CallStatements {
    param([string]$FilePath)
    
    $calls = @()
    $content = Get-Content $FilePath -ErrorAction SilentlyContinue
    if ($content) {
        foreach ($line in $content) {
            if ($line -match '\s+(CALLB|CALLP)\s+''?([A-Za-z0-9_-]+)''?') {
                $calls += $matches[2]
            }
        }
    }
    return $calls
}

# Function to extract /COPY and /INCLUDE statements
function Get-CopyStatements {
    param([string]$FilePath)
    
    $copies = @()
    $content = Get-Content $FilePath -ErrorAction SilentlyContinue
    if ($content) {
        foreach ($line in $content) {
            if ($line -match '^\s*/(COPY|INCLUDE)\s+([A-Za-z0-9/_-]+)') {
                $copies += $matches[2]
            }
        }
    }
    return $copies
}

# Function to extract F-spec file names
function Get-FileReferences {
    param([string]$FilePath)
    
    $fileRefs = @()
    $content = Get-Content $FilePath -ErrorAction SilentlyContinue
    if ($content) {
        foreach ($line in $content) {
            if ($line -match '^\s*F([A-Z0-9]+)') {
                $fileRefs += $matches[1]
            }
        }
    }
    return $fileRefs
}

# Find all RPG files
Write-Host "Scanning for RPG source files..."
$rpgFiles = Get-ChildItem -Path $SourceDir -Recurse -Include "*.rpg","*.RPG","*.rpgle","*.RPGLE","*.sqlrpgle","*.SQLRPGLE" -File -ErrorAction SilentlyContinue

if (-not $rpgFiles -or $rpgFiles.Count -eq 0) {
    Write-Host "No RPG files found in $SourceDir"
    exit 1
}

Write-Host "Found $($rpgFiles.Count) RPG files"

# Collections for data
$programs = @()
$dependencies = @()
$copyMembers = @()
$files = @()

# Process each RPG file
foreach ($rpgFile in $rpgFiles) {
    Write-Host "Processing: $($rpgFile.FullName)"
    
    $programName = Get-ProgramName -FilePath $rpgFile.FullName
    
    # Add program to list
    $programs += @{
        name = $programName
        file = $rpgFile.FullName
    }
    
    # Extract calls (CALLB/CALLP)
    $calls = Get-CallStatements -FilePath $rpgFile.FullName
    foreach ($calledProgram in $calls) {
        if ($calledProgram) {
            $dependencies += @{
                from = $programName
                to = $calledProgram
                type = "call"
            }
        }
    }
    
    # Extract /COPY and /INCLUDE members
    $copies = Get-CopyStatements -FilePath $rpgFile.FullName
    foreach ($copyMember in $copies) {
        if ($copyMember) {
            $copyMembers += @{
                name = $copyMember
                used_by = $programName
            }
            $dependencies += @{
                from = $programName
                to = $copyMember
                type = "copy"
            }
        }
    }
    
    # Extract F-spec file references
    $fileRefs = Get-FileReferences -FilePath $rpgFile.FullName
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
$uniqueCopyMembers = $copyMembers | Select-Object -Property name -Unique
$uniqueFiles = $files | Select-Object -Property name -Unique

$result = @{
    programs = $programs
    dependencies = $dependencies
    copy_members = $uniqueCopyMembers
    files = $uniqueFiles
    summary = @{
        total_programs = $programs.Count
        total_dependencies = $dependencies.Count
        total_copy_members = $uniqueCopyMembers.Count
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
Write-Host "  /COPY Members: $($uniqueCopyMembers.Count)"
Write-Host "  Files: $($uniqueFiles.Count)"
