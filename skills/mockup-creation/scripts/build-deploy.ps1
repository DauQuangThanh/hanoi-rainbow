# Build and Deploy Script (PowerShell)
# Builds the Vite project and prepares it for deployment
# Platform: Windows (PowerShell 5.1+), optional: Linux/macOS (PowerShell Core 7+)

[CmdletBinding()]
param()

# Set error action preference
$ErrorActionPreference = "Stop"

# Function for colored output
function Write-ColorOutput {
    param(
        [string]$Message,
        [string]$Color = "White"
    )
    Write-Host $Message -ForegroundColor $Color
}

# Function to draw separator
function Write-Separator {
    Write-ColorOutput "========================================" "Blue"
}

try {
    Write-Separator
    Write-ColorOutput "   Mockup Build & Deploy Script" "Blue"
    Write-Separator
    Write-Host ""

    # Check if package.json exists
    if (-not (Test-Path "package.json")) {
        Write-ColorOutput "Error: package.json not found" "Red"
        Write-Host "Make sure you're in the project root directory"
        exit 1
    }

    # Clean previous build
    Write-ColorOutput "→ Cleaning previous build..." "Yellow"
    if (Test-Path "dist") {
        Remove-Item -Path "dist" -Recurse -Force
    }
    Write-ColorOutput "✓ Clean complete" "Green"
    Write-Host ""

    # Install dependencies
    Write-ColorOutput "→ Installing dependencies..." "Yellow"
    npm install
    if ($LASTEXITCODE -ne 0) {
        throw "Failed to install dependencies"
    }
    Write-ColorOutput "✓ Dependencies installed" "Green"
    Write-Host ""

    # Run type check
    Write-ColorOutput "→ Running TypeScript type check..." "Yellow"
    npm run type-check
    if ($LASTEXITCODE -ne 0) {
        Write-ColorOutput "✗ Type check failed" "Red"
        Write-Host "Fix TypeScript errors before deploying"
        exit 1
    }
    Write-ColorOutput "✓ Type check passed" "Green"
    Write-Host ""

    # Build project
    Write-ColorOutput "→ Building project..." "Yellow"
    npm run build
    if ($LASTEXITCODE -ne 0) {
        throw "Build failed"
    }
    Write-ColorOutput "✓ Build complete" "Green"
    Write-Host ""

    # Check build size
    Write-ColorOutput "→ Analyzing build size..." "Yellow"
    $buildSize = (Get-ChildItem -Path "dist" -Recurse | Measure-Object -Property Length -Sum).Sum
    $buildSizeMB = [math]::Round($buildSize / 1MB, 2)
    Write-Host "  Build size: $buildSizeMB MB"

    # Count files
    $fileCount = (Get-ChildItem -Path "dist" -Recurse -File).Count
    Write-Host "  Files: $fileCount"
    Write-Host ""

    # Display deployment options
    Write-Separator
    Write-ColorOutput "   Deployment Options" "Blue"
    Write-Separator
    Write-Host ""
    Write-Host "Your build is ready in the 'dist' directory"
    Write-Host ""
    Write-ColorOutput "Option 1: Netlify" "Yellow"
    Write-Host "  1. Install Netlify CLI: npm install -g netlify-cli"
    Write-Host "  2. Deploy: netlify deploy --prod --dir=dist"
    Write-Host ""
    Write-ColorOutput "Option 2: Vercel" "Yellow"
    Write-Host "  1. Install Vercel CLI: npm install -g vercel"
    Write-Host "  2. Deploy: vercel --prod"
    Write-Host ""
    Write-ColorOutput "Option 3: GitHub Pages" "Yellow"
    Write-Host "  1. Push 'dist' to gh-pages branch"
    Write-Host "  2. Enable GitHub Pages in repository settings"
    Write-Host ""
    Write-ColorOutput "Option 4: Manual Upload" "Yellow"
    Write-Host "  Upload contents of 'dist' directory to your web server"
    Write-Host ""

    # Create deployment info file
    $deployInfo = @"
Build Information
=================
Build Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Build Size: $buildSizeMB MB
File Count: $fileCount
PowerShell Version: $($PSVersionTable.PSVersion)
Node Version: $(node --version)
npm Version: $(npm --version)

Deployment Instructions
=======================
This directory contains the production build of your Vue.js application.

To deploy:
1. Upload all files to your web server's public directory
2. Configure your web server to serve index.html for all routes (SPA mode)
3. Ensure proper MIME types are set for .js, .css, and .html files

Server Configuration Examples:

Nginx:
------
location / {
  try_files `$uri `$uri/ /index.html;
}

Apache (.htaccess):
-------------------
RewriteEngine On
RewriteBase /
RewriteRule ^index\.html$ - [L]
RewriteCond %{REQUEST_FILENAME} !-f
RewriteCond %{REQUEST_FILENAME} !-d
RewriteRule . /index.html [L]

Node.js (Express):
------------------
app.use(express.static('dist'))
app.get('*', (req, res) => {
  res.sendFile(path.join(__dirname, 'dist', 'index.html'))
})

IIS (web.config):
-----------------
<configuration>
  <system.webServer>
    <rewrite>
      <rules>
        <rule name="SPA Routes" stopProcessing="true">
          <match url=".*" />
          <conditions logicalGrouping="MatchAll">
            <add input="{REQUEST_FILENAME}" matchType="IsFile" negate="true" />
            <add input="{REQUEST_FILENAME}" matchType="IsDirectory" negate="true" />
          </conditions>
          <action type="Rewrite" url="/" />
        </rule>
      </rules>
    </rewrite>
  </system.webServer>
</configuration>
"@

    Set-Content -Path "dist\DEPLOY_INFO.txt" -Value $deployInfo -Encoding UTF8
    Write-ColorOutput "✓ Deployment info saved to dist\DEPLOY_INFO.txt" "Green"
    Write-Host ""

    # Ask if user wants to preview
    $response = Read-Host "Preview build locally? (y/n)"
    if ($response -eq 'y') {
        Write-ColorOutput "→ Starting preview server..." "Yellow"
        Write-Host "  Press Ctrl+C to stop"
        Write-Host ""
        npm run preview
    }

    Write-Separator
    Write-ColorOutput "   Build Complete!" "Green"
    Write-Separator
}
catch {
    Write-ColorOutput "Error: $_" "Red"
    exit 1
}
