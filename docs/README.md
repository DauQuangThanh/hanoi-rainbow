# 📚 Documentation

This folder contains the documentation source files for Hanoi Rainbow, built with [DocFX](https://dotnet.github.io/docfx/).

---

## 💻 Building Locally

To build and preview the documentation on your machine:

### 1. Install DocFX

```bash
dotnet tool install -g docfx
```

### 2. Build and Serve

```bash
cd docs
docfx docfx.json --serve
```

### 3. View in Browser

Open `http://localhost:8080` to see the documentation.

---

## 📁 File Structure

| File | Purpose |
|------|----------|
| `docfx.json` | DocFX configuration |
| `index.md` | Documentation homepage |
| `toc.yml` | Table of contents |
| `installation.md` | Installation instructions |
| `quickstart.md` | Quick start guide |
| `upgrade.md` | Upgrade instructions |
| `local-development.md` | Local development guide |
| `_site/` | Generated output (git-ignored) |

---

## 🚀 Deployment

Documentation is **automatically built and deployed** to GitHub Pages when changes are pushed to the `main` branch.

**Workflow:** `.github/workflows/docs.yml`

---

## 👉 Contributing

To update documentation:

1. Edit the `.md` files in this directory
2. Test locally with `docfx docfx.json --serve`
3. Commit your changes
4. Push to `main` branch
5. GitHub Actions will build and deploy automatically

---

## 📚 Resources

- 📖 [DocFX Documentation](https://dotnet.github.io/docfx/)
- 🌐 [Live Documentation](https://dauquangthanh.github.io/hainoi-rainbow/)
- 🐛 [Report Issues](https://github.com/dauquangthanh/hanoi-rainbow/issues/new)
