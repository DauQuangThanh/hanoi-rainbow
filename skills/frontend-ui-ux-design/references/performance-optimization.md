# Performance Optimization


### Frontend Performance

1. **Asset Optimization**
   - Compress images (WebP, AVIF formats)
   - Use responsive images (srcset, picture element)
   - Lazy load images and components
   - Minify CSS and JavaScript
   - Remove unused CSS (PurgeCSS)

2. **Critical Rendering Path**
   - Inline critical CSS
   - Defer non-critical JavaScript
   - Preload critical resources
   - Minimize render-blocking resources

3. **Core Web Vitals**
   - **LCP (Largest Contentful Paint)**: < 2.5s
     - Optimize images, use CDN, reduce server response time
   - **FID (First Input Delay)**: < 100ms
     - Minimize JavaScript, split code, use web workers
   - **CLS (Cumulative Layout Shift)**: < 0.1
     - Set image dimensions, reserve space for ads, avoid inserting content

4. **JavaScript Optimization**
   - Code splitting (route-based, component-based)
   - Tree shaking (remove unused code)
   - Lazy load non-critical components
   - Use React.memo, useMemo, useCallback (React)
   - Virtual scrolling for long lists
