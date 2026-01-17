#!/bin/bash

# Build and Deploy Script
# Builds the Vite project and prepares it for deployment

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}   Mockup Build & Deploy Script${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Check if package.json exists
if [ ! -f "package.json" ]; then
    echo -e "${RED}Error: package.json not found${NC}"
    echo "Make sure you're in the project root directory"
    exit 1
fi

# Clean previous build
echo -e "${YELLOW}→ Cleaning previous build...${NC}"
rm -rf dist
echo -e "${GREEN}✓ Clean complete${NC}"
echo ""

# Install dependencies
echo -e "${YELLOW}→ Installing dependencies...${NC}"
npm install
echo -e "${GREEN}✓ Dependencies installed${NC}"
echo ""

# Run type check
echo -e "${YELLOW}→ Running TypeScript type check...${NC}"
npm run type-check || {
    echo -e "${RED}✗ Type check failed${NC}"
    echo "Fix TypeScript errors before deploying"
    exit 1
}
echo -e "${GREEN}✓ Type check passed${NC}"
echo ""

# Build project
echo -e "${YELLOW}→ Building project...${NC}"
npm run build
echo -e "${GREEN}✓ Build complete${NC}"
echo ""

# Check build size
echo -e "${YELLOW}→ Analyzing build size...${NC}"
BUILD_SIZE=$(du -sh dist | cut -f1)
echo -e "  Build size: ${BUILD_SIZE}"

# Count files
FILE_COUNT=$(find dist -type f | wc -l | tr -d ' ')
echo -e "  Files: ${FILE_COUNT}"
echo ""

# Display deployment options
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}   Deployment Options${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo "Your build is ready in the 'dist' directory"
echo ""
echo -e "${YELLOW}Option 1: Netlify${NC}"
echo "  1. Install Netlify CLI: npm install -g netlify-cli"
echo "  2. Deploy: netlify deploy --prod --dir=dist"
echo ""
echo -e "${YELLOW}Option 2: Vercel${NC}"
echo "  1. Install Vercel CLI: npm install -g vercel"
echo "  2. Deploy: vercel --prod"
echo ""
echo -e "${YELLOW}Option 3: GitHub Pages${NC}"
echo "  1. Push 'dist' to gh-pages branch"
echo "  2. Enable GitHub Pages in repository settings"
echo ""
echo -e "${YELLOW}Option 4: Manual Upload${NC}"
echo "  Upload contents of 'dist' directory to your web server"
echo ""

# Create deployment info file
cat > dist/DEPLOY_INFO.txt << EOF
Build Information
=================
Build Date: $(date)
Build Size: $BUILD_SIZE
File Count: $FILE_COUNT
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
  try_files \$uri \$uri/ /index.html;
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
EOF

echo -e "${GREEN}✓ Deployment info saved to dist/DEPLOY_INFO.txt${NC}"
echo ""

# Ask if user wants to preview
read -p "Preview build locally? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo -e "${YELLOW}→ Starting preview server...${NC}"
    echo -e "  Press Ctrl+C to stop"
    echo ""
    npm run preview
fi

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}   Build Complete!${NC}"
echo -e "${GREEN}========================================${NC}"
