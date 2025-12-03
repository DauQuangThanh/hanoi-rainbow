# setup-architect.ps1 - Setup script for /rainbow.architect command
# Creates architecture documentation structure and prepares for architecture design workflow

param(
    [switch]$Json,
    [string]$Product = ""
)

# Import common functions
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
. "$scriptDir\common.ps1"

# Configuration
$docsDir = "docs"
$archDoc = Join-Path $docsDir "architecture.md"
$archTemplate = ".rainbow\templates\templates-for-commands\arch-template.md"
$adrDir = Join-Path $docsDir "adr"
$specsDir = "specs"

# Verify we're in a git repository
try {
    $null = git rev-parse --git-dir 2>&1
} catch {
    Write-Error "Not in a git repository. Please run this from the repository root."
    exit 1
}

$repoRoot = git rev-parse --show-toplevel
Set-Location $repoRoot

# Verify required files exist
if (-not (Test-Path $archTemplate)) {
    Write-Error "Architecture template not found: $archTemplate"
    exit 1
}

if (-not (Test-Path ".rainbow\memory\ground-rules.md")) {
    Write-Error "Ground rules file not found: .rainbow\memory\ground-rules.md. Run /rainbow.regulate first."
    exit 1
}

# Get product name from user input or git repo
if ([string]::IsNullOrEmpty($Product)) {
    $Product = Split-Path -Leaf $repoRoot
}

# Create docs directory structure
Write-Host "INFO: Creating architecture documentation structure..."
New-Item -ItemType Directory -Path $docsDir -Force | Out-Null
New-Item -ItemType Directory -Path $adrDir -Force | Out-Null

# Copy architecture template if it doesn't exist
if (-not (Test-Path $archDoc)) {
    Write-Host "INFO: Creating architecture document from template..."
    Copy-Item $archTemplate $archDoc
    
    # Replace placeholder with product name and date
    $content = Get-Content $archDoc -Raw
    $content = $content -replace '\[PRODUCT/PROJECT NAME\]', $Product
    $content = $content -replace '\[DATE\]', (Get-Date -Format "yyyy-MM-dd")
    Set-Content -Path $archDoc -Value $content
    
    Write-Host "✓ Created: $archDoc"
} else {
    Write-Host "INFO: Architecture document already exists: $archDoc"
}

# Count existing feature specs
$specCount = 0
$featureSpecs = @()
if (Test-Path $specsDir) {
    $featureSpecs = Get-ChildItem -Path $specsDir -Filter "spec.md" -Recurse -File | ForEach-Object { $_.FullName -replace [regex]::Escape($repoRoot + '\'), '' -replace '\\', '/' }
    $specCount = $featureSpecs.Count
}

# Detect AI agent
$agentType = ""
if (Test-Path ".github\agents") {
    $agentType = "copilot"
} elseif (Test-Path ".claude\commands") {
    $agentType = "claude"
} elseif (Test-Path ".cursor\commands") {
    $agentType = "cursor-agent"
} elseif (Test-Path ".windsurf\workflows") {
    $agentType = "windsurf"
} elseif (Test-Path ".gemini\commands") {
    $agentType = "gemini"
} elseif (Test-Path ".qwen\commands") {
    $agentType = "qwen"
} elseif (Test-Path ".opencode\command") {
    $agentType = "opencode"
} elseif (Test-Path ".codex\commands") {
    $agentType = "codex"
} elseif (Test-Path ".kilocode\rules") {
    $agentType = "kilocode"
} elseif (Test-Path ".augment\rules") {
    $agentType = "auggie"
} elseif (Test-Path ".roo\rules") {
    $agentType = "roo"
} elseif (Test-Path ".codebuddy\commands") {
    $agentType = "codebuddy"
} elseif (Test-Path ".amazonq\prompts") {
    $agentType = "q"
} elseif (Test-Path ".agents\commands") {
    $agentType = "amp"
} elseif (Test-Path ".shai\commands") {
    $agentType = "shai"
} elseif (Test-Path ".bob\commands") {
    $agentType = "bob"
}

# Output results
if ($Json) {
    # JSON output for agent consumption
    $featureSpecsJson = $featureSpecs | ForEach-Object { "`"$_`"" }
    $featureSpecsList = $featureSpecsJson -join ",`n    "
    
    @"
{
  "ARCH_DOC": "$($archDoc -replace '\\', '/')",
  "DOCS_DIR": "$($docsDir -replace '\\', '/')",
  "ADR_DIR": "$($adrDir -replace '\\', '/')",
  "SPECS_DIR": "$($specsDir -replace '\\', '/')",
  "SPEC_COUNT": $specCount,
  "FEATURE_SPECS": [
    $featureSpecsList
  ],
  "PRODUCT_NAME": "$Product",
  "GROUND_RULES": "memory/ground-rules.md",
  "AGENT_TYPE": "$agentType",
  "REPO_ROOT": "$($repoRoot -replace '\\', '/')"
}
"@
} else {
    # Human-readable output
    Write-Host ""
    Write-Host "INFO: Architecture Documentation Setup Complete"
    Write-Host ""
    Write-Host "📋 Configuration:"
    Write-Host "   Product Name:       $Product"
    Write-Host "   Architecture Doc:   $archDoc"
    Write-Host "   Documentation Dir:  $docsDir"
    Write-Host "   ADR Directory:      $adrDir"
    Write-Host "   Feature Specs:      $specCount found in $specsDir\"
    Write-Host "   AI Agent Detected:  $(if ($agentType) { $agentType } else { 'none' })"
    Write-Host ""
    Write-Host "📝 Next Steps:"
    Write-Host "   1. Review and complete $archDoc"
    Write-Host "   2. Create C4 diagrams using Mermaid"
    Write-Host "   3. Document architecture decisions in $adrDir\"
    Write-Host "   4. Run feature implementation plans with /rainbow.design"
    Write-Host ""
}

exit 0
