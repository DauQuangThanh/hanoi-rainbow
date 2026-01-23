# Deployment Guide

Comprehensive guide for deploying NuxtJS 4 (Vue) and Next.js (React) mockups with TailwindCSS v4 to various hosting platforms.

> **Note**: Both frameworks support SSR (Server-Side Rendering) and SSG (Static Site Generation). Choose based on your needs.

## Table of Contents

- [Build Configuration](#build-configuration)
  - [NuxtJS Configuration](#nuxtjs-configuration)
  - [Next.js Configuration](#nextjs-configuration)
- [Netlify Deployment](#netlify-deployment)
- [Vercel Deployment](#vercel-deployment)
- [GitHub Pages](#github-pages)
- [AWS S3 + CloudFront](#aws-s3--cloudfront)
- [Docker Deployment](#docker-deployment)
- [Environment Variables](#environment-variables)
- [Performance Optimization](#performance-optimization)

---

## Build Configuration

### NuxtJS Configuration

#### nuxt.config.ts for Production

```typescript
// https://nuxt.com/docs/4.x/api/configuration/nuxt-config
import tailwindcss from '@tailwindcss/vite'

export default defineNuxtConfig({
  // Rendering mode
  ssr: true, // Set to false for SPA mode
  
  // Nitro (server) configuration
  nitro: {
    compressPublicAssets: true,
    prerender: {
      crawlLinks: true,
      routes: ['/'] // Add your routes for SSG
    }
  },
  
  // Runtime config (for environment variables)
  runtimeConfig: {
    // Private keys (server-side only)
    apiSecret: '',
    // Public keys (exposed to client)
    public: {
      apiBase: process.env.API_BASE_URL || ''
    }
  },
  
  // App configuration
  app: {
    head: {
      title: 'My Mockup',
      meta: [
        { charset: 'utf-8' },
        { name: 'viewport', content: 'width=device-width, initial-scale=1' },
        { name: 'description', content: 'My mockup description' }
      ],
      link: [
        { rel: 'icon', type: 'image/x-icon', href: '/favicon.ico' }
      ]
    }
  },
  
  // TypeScript
  typescript: {
    strict: true,
    typeCheck: true
  },
  
  // Vite optimization with TailwindCSS v4
  vite: {
    plugins: [
      tailwindcss()
    ],
    build: {
      cssCodeSplit: true,
      rollupOptions: {
        output: {
          manualChunks: {
            // Split vendor chunks for better caching
          }
        }
      }
    }
  },
  
  // CSS files
  css: ['~/assets/css/main.css']
})
```

#### Build Commands (NuxtJS)

```json
{
  "scripts": {
    "dev": "nuxt dev",
    "build": "nuxt build",
    "generate": "nuxt generate",
    "preview": "nuxt preview",
    "postinstall": "nuxt prepare",
    "typecheck": "nuxi typecheck",
    "analyze": "nuxi analyze"
  }
}
```

**Build Types:**

- `npm run build` - SSR build (.output/ directory)
- `npm run generate` - Static site (SSG) build (.output/public/ directory)
- `npm run preview` - Preview production build locally

### Next.js Configuration

#### next.config.js for Production

```javascript
/** @type {import('next').NextConfig} */
const nextConfig = {
  // Rendering mode
  // output: 'export', // Uncomment for static export (SSG)
  
  // Performance optimizations
  poweredByHeader: false,
  compress: true,
  
  // Image optimization
  images: {
    // For static export, use unoptimized
    // unoptimized: true,
    remotePatterns: [
      {
        protocol: 'https',
        hostname: 'example.com',
      },
    ],
  },
  
  // TypeScript and linting
  typescript: {
    ignoreBuildErrors: false,
  },
  eslint: {
    ignoreDuringBuilds: false,
  },
  
  // Environment variables (public vars must start with NEXT_PUBLIC_)
  env: {
    CUSTOM_KEY: process.env.CUSTOM_KEY,
  },
  
  // Headers for security
  async headers() {
    return [
      {
        source: '/:path*',
        headers: [
          {
            key: 'X-Frame-Options',
            value: 'DENY',
          },
          {
            key: 'X-Content-Type-Options',
            value: 'nosniff',
          },
          {
            key: 'Referrer-Policy',
            value: 'strict-origin-when-cross-origin',
          },
        ],
      },
    ]
  },
}

export default nextConfig
```

#### Build Commands (Next.js)

```json
{
  "scripts": {
    "dev": "next dev",
    "build": "next build",
    "start": "next start",
    "lint": "eslint .",
    "lint:fix": "eslint --fix ."
  }
}
```

**Build Types:**

- `npm run build` - Production build (SSR, default)
- `npm run build` with `output: 'export'` in config - Static export (SSG)
- `npm run start` - Start production server (SSR only)

---

## Netlify Deployment

### NuxtJS on Netlify

##### SSG (Static Site) Deployment

1. **Create `netlify.toml`:**

```toml
[build]
  command = "npm run generate"
  publish = ".output/public"

[[redirects]]
  from = "/*"
  to = "/index.html"
  status = 200

[build.environment]
  NODE_VERSION = "20"

[[headers]]
  for = "/*"
  [headers.values]
    X-Frame-Options = "DENY"
    X-XSS-Protection = "1; mode=block"
    X-Content-Type-Options = "nosniff"
    Referrer-Policy = "strict-origin-when-cross-origin"

[[headers]]
  for = "/_nuxt/*"
  [headers.values]
    Cache-Control = "public, max-age=31536000, immutable"
```

#### SSR Deployment (using Nitro)

**Install Netlify adapter:**

```bash
npm install -D @netlify/functions
```

**Update nuxt.config.ts:**

```typescript
export default defineNuxtConfig({
  nitro: {
    preset: 'netlify'
  }
})
```

**Build and deploy:**

```bash
npm run build
# Output is in .output/
```

### Next.js on Netlify

#### SSR Deployment (Default)

1. **Create `netlify.toml`:**

```toml
[build]
  command = "npm run build"
  publish = ".next"

[build.environment]
  NODE_VERSION = "20"

[[plugins]]
  package = "@netlify/plugin-nextjs"

[[headers]]
  for = "/*"
  [headers.values]
    X-Frame-Options = "DENY"
    X-Content-Type-Options = "nosniff"
    Referrer-Policy = "strict-origin-when-cross-origin"
```

**Install Next.js plugin:**

```bash
npm install -D @netlify/plugin-nextjs
```

#### SSG (Static Export) Deployment

**Update next.config.js:**

```javascript
const nextConfig = {
  output: 'export',
  images: {
    unoptimized: true, // Required for static export
  },
}
```

**Update `netlify.toml`:**

```toml
[build]
  command = "npm run build"
  publish = "out"

[[redirects]]
  from = "/*"
  to = "/index.html"
  status = 200

[build.environment]
  NODE_VERSION = "20"
```

### Deployment Methods (Both Frameworks)

#### Method 1: Git-based Deployment
   - Go to <https://app.netlify.com>
   - Click "New site from Git"
   - Select your repository
   - Build settings are auto-detected from `netlify.toml`
   - Click "Deploy site"

### Method 2: Netlify CLI

```bash
# Install Netlify CLI
npm install -g netlify-cli

# Login to Netlify
netlify login

# Initialize Netlify site
netlify init

# Deploy
netlify deploy --prod
```

### Custom Domain Setup

```bash
# Add custom domain
netlify domains:add yourdomain.com

# Configure DNS (use Netlify DNS or add CNAME record)
# CNAME record: www -> your-site-name.netlify.app
```

---

## Vercel Deployment

> **Both NuxtJS and Next.js have first-class Vercel support - zero configuration needed!**

### NuxtJS on Vercel

#### Method 1: Git Integration (Recommended)

1. **No vercel.json needed** - Vercel auto-detects NuxtJS

2. **Deploy:**
   - Go to <https://vercel.com>
   - Click "New Project"
   - Import your Git repository
   - Vercel automatically detects Nuxt and configures everything
   - Click "Deploy"

#### Method 2: Vercel CLI

```bash
# Install Vercel CLI
npm install -g vercel

# Login
vercel login

# Deploy
vercel --prod
```

#### SSR vs SSG on Vercel (NuxtJS)

**SSR (default):** Runs Nuxt server on Vercel Edge Functions

**SSG:** Update nuxt.config.ts:

```typescript
export default defineNuxtConfig({
  nitro: {
    preset: 'vercel-static'
  }
})
```

Then run `npm run generate` for static deployment.

### Next.js on Vercel (Recommended Platform)

> **Vercel is the creator and recommended platform for Next.js**

#### Method 1: Git Integration (Easiest)

1. **Zero configuration needed** - Vercel auto-detects Next.js

2. **Deploy:**
   - Go to <https://vercel.com>
   - Click "New Project"
   - Import your Git repository
   - Vercel automatically configures everything
   - Click "Deploy"

#### Method 2: Vercel CLI

```bash
# Install Vercel CLI
npm install -g vercel

# Login
vercel login

# Deploy
vercel --prod
```

#### Automatic Features

- **SSR by default**: Server-Side Rendering with Edge Functions
- **ISR**: Incremental Static Regeneration automatically enabled
- **Image Optimization**: Built-in with Next.js Image component
- **Analytics**: Optional Vercel Analytics integration
- **Edge Middleware**: Runs on Vercel Edge Network

#### Environment Variables

Set in Vercel Dashboard:

1. Go to Project Settings → Environment Variables
2. Add variables (e.g., `NEXT_PUBLIC_API_URL`)
3. Redeploy to apply changes

---

## GitHub Pages

> **Note**: GitHub Pages only supports static sites (SSG)

### NuxtJS on GitHub Pages

#### Using GitHub Actions

1. **Update nuxt.config.ts for GitHub Pages:**

```typescript
export default defineNuxtConfig({
  app: {
    baseURL: '/your-repo-name/', // Replace with your repo name
    buildAssetsDir: 'assets',
  },
  // Use SSG
  ssr: false
})
```

2. **Create `.github/workflows/deploy.yml`:**

```yaml
name: Deploy to GitHub Pages

on:
  push:
    branches:
      - main

permissions:
  contents: read
  pages: write
  id-token: write

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4
        
      - name: Setup Node
        uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: 'npm'
          
      - name: Install dependencies
        run: npm ci
        
      - name: Build
        run: npm run generate
        env:
          NODE_ENV: production
          
      - name: Upload artifact
        uses: actions/upload-pages-artifact@v3
        with:
          path: .output/public
          
  deploy:
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    runs-on: ubuntu-latest
    needs: build
    steps:
      - name: Deploy to GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4
```

1. **Enable GitHub Pages:**
   - Go to repository Settings
   - Navigate to Pages
   - Source: GitHub Actions

---

## AWS S3 + CloudFront

### Setup Script

```bash
#!/bin/bash

# Configuration
BUCKET_NAME="your-mockup-bucket"
CLOUDFRONT_ID="your-distribution-id"
REGION="us-east-1"

# Build project
echo "Building project..."
npm run build

# Create S3 bucket
echo "Creating S3 bucket..."
aws s3 mb s3://$BUCKET_NAME --region $REGION

# Configure bucket for static website hosting
aws s3 website s3://$BUCKET_NAME \
  --index-document index.html \
  --error-document index.html

# Upload files
echo "Uploading files..."
aws s3 sync dist/ s3://$BUCKET_NAME \
  --delete \
  --cache-control "public, max-age=31536000, immutable" \
  --exclude "index.html"

# Upload index.html separately with no-cache
aws s3 cp dist/index.html s3://$BUCKET_NAME/index.html \
  --cache-control "no-cache, no-store, must-revalidate"

# Create CloudFront invalidation
echo "Creating CloudFront invalidation..."
aws cloudfront create-invalidation \
  --distribution-id $CLOUDFRONT_ID \
  --paths "/*"

echo "Deployment complete!"
```

### CloudFront Configuration

```json
{
  "Origins": [
    {
      "Id": "S3-mockup-bucket",
      "DomainName": "your-mockup-bucket.s3.amazonaws.com",
      "S3OriginConfig": {
        "OriginAccessIdentity": ""
      }
    }
  ],
  "DefaultCacheBehavior": {
    "TargetOriginId": "S3-mockup-bucket",
    "ViewerProtocolPolicy": "redirect-to-https",
    "Compress": true,
    "CachePolicyId": "658327ea-f89d-4fab-a63d-7e88639e58f6"
  },
  "CustomErrorResponses": [
    {
      "ErrorCode": 404,
      "ResponseCode": 200,
      "ResponsePagePath": "/index.html"
    },
    {
      "ErrorCode": 403,
      "ResponseCode": 200,
      "ResponsePagePath": "/index.html"
    }
  ]
}
```

---

## Docker Deployment

### Dockerfile

```dockerfile
# Build stage
FROM node:18-alpine AS builder

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci

# Copy source files
COPY . .

# Build application
RUN npm run build

# Production stage
FROM nginx:alpine

# Copy built files
COPY --from=builder /app/dist /usr/share/nginx/html

# Copy nginx configuration
COPY nginx.conf /etc/nginx/conf.d/default.conf

# Expose port
EXPOSE 80

# Start nginx
CMD ["nginx", "-g", "daemon off;"]
```

### nginx.conf

```nginx
server {
    listen 80;
    server_name localhost;
    root /usr/share/nginx/html;
    index index.html;

    # Enable gzip compression
    gzip on;
    gzip_vary on;
    gzip_min_length 1024;
    gzip_types text/plain text/css text/xml text/javascript application/javascript application/json;

    # Cache static assets
    location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$ {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }

    # SPA fallback
    location / {
        try_files $uri $uri/ /index.html;
    }

    # Security headers
    add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header X-XSS-Protection "1; mode=block" always;
}
```

### Docker Commands

```bash
# Build image
docker build -t mockup-app .

# Run container
docker run -d -p 8080:80 --name mockup mockup-app

# Stop container
docker stop mockup

# Remove container
docker rm mockup
```

### docker-compose.yml

```yaml
version: '3.8'

services:
  web:
    build: .
    ports:
      - "8080:80"
    restart: unless-stopped
    environment:
      - NODE_ENV=production
```

---

## Environment Variables

### .env Files

```bash
# .env.development
VITE_API_URL=http://localhost:3000/api
VITE_APP_TITLE=Mockup Dev

# .env.production
VITE_API_URL=https://api.production.com
VITE_APP_TITLE=Mockup
```

### Using Environment Variables

```typescript
// src/config/index.ts
export const config = {
  apiUrl: import.meta.env.VITE_API_URL,
  appTitle: import.meta.env.VITE_APP_TITLE,
  isDev: import.meta.env.DEV,
  isProd: import.meta.env.PROD
}
```

```vue
<script setup lang="ts">
import { config } from '@/config'

console.log(config.apiUrl)
</script>
```

---

## Performance Optimization

### Code Splitting

```typescript
// router/index.ts
import { createRouter, createWebHistory } from 'vue-router'

const router = createRouter({
  history: createWebHistory(),
  routes: [
    {
      path: '/',
      component: () => import('@/views/Home.vue') // Lazy loaded
    },
    {
      path: '/dashboard',
      component: () => import('@/views/Dashboard.vue')
    }
  ]
})
```

### Image Optimization

```bash
# Install image optimization plugin
npm install -D vite-plugin-imagemin
```

```typescript
// vite.config.ts
import imagemin from 'vite-plugin-imagemin'

export default defineConfig({
  plugins: [
    vue(),
    imagemin({
      gifsicle: { optimizationLevel: 7 },
      optipng: { optimizationLevel: 7 },
      mozjpeg: { quality: 80 },
      svgo: {
        plugins: [
          { name: 'removeViewBox', active: false },
          { name: 'removeEmptyAttrs', active: true }
        ]
      }
    })
  ]
})
```

### Bundle Analysis

```bash
# Install plugin
npm install -D rollup-plugin-visualizer
```

```typescript
// vite.config.ts
import { visualizer } from 'rollup-plugin-visualizer'

export default defineConfig({
  plugins: [
    vue(),
    visualizer({
      open: true,
      gzipSize: true,
      brotliSize: true
    })
  ]
})
```

### Preload/Prefetch

```typescript
// vite.config.ts
export default defineConfig({
  build: {
    rollupOptions: {
      output: {
        manualChunks(id) {
          // Split vendor chunks
          if (id.includes('node_modules')) {
            return 'vendor'
          }
        }
      }
    }
  }
})
```

### Service Worker (PWA)

```bash
# Install PWA plugin
npm install -D vite-plugin-pwa
```

```typescript
// vite.config.ts
import { VitePWA } from 'vite-plugin-pwa'

export default defineConfig({
  plugins: [
    vue(),
    VitePWA({
      registerType: 'autoUpdate',
      manifest: {
        name: 'Mockup App',
        short_name: 'Mockup',
        theme_color: '#ffffff',
        icons: [
          {
            src: '/icon-192.png',
            sizes: '192x192',
            type: 'image/png'
          }
        ]
      }
    })
  ]
})
```

---

## Deployment Checklist

- [ ] Run type checking: `npm run type-check`
- [ ] Build project: `npm run build`
- [ ] Test production build locally: `npm run preview`
- [ ] Verify environment variables are set correctly
- [ ] Check bundle size and optimize if needed
- [ ] Test on multiple browsers
- [ ] Verify mobile responsiveness
- [ ] Check accessibility (WCAG compliance)
- [ ] Set up analytics (Google Analytics, etc.)
- [ ] Configure error tracking (Sentry, etc.)
- [ ] Set up monitoring and uptime checks
- [ ] Configure SSL certificate
- [ ] Set up custom domain (if applicable)
- [ ] Create backup/rollback strategy
- [ ] Document deployment process
- [ ] Test deployed site functionality

This guide covers the most common deployment scenarios. Choose the platform that best fits your project requirements and team expertise.
