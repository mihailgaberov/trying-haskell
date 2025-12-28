# Quick Deployment Guide

## Render.com (Recommended - 5 minutes)

1. **Push to GitHub**
   ```bash
   git add .
   git commit -m "Ready for deployment"
   git push origin main
   ```

2. **Deploy on Render**
   - Go to [render.com](https://render.com)
   - Sign up with GitHub
   - Click "New +" → "Web Service"
   - Connect your repository
   - Settings:
     - **Environment:** `Docker`
     - **Dockerfile Path:** `Dockerfile`
   - Click "Create Web Service"
   - Wait ~10-15 minutes for first build
   - Done! Your app is live 🎉

## Test Locally with Docker

```bash
# Build
docker build -t trying-haskell .

# Run
docker run -p 3000:3000 trying-haskell

# Open browser
open http://localhost:3000
```

## What's Included

✅ Dockerfile - Multi-stage build for production  
✅ render.yaml - Render.com configuration  
✅ .dockerignore - Excludes unnecessary files  
✅ PORT environment variable support  
✅ Static file serving  
✅ Health check endpoint

## Your App Will Be Available At

- Render: `https://trying-haskell.onrender.com` (or your custom name)
- Local: `http://localhost:3000`

## Need Help?

See `DEPLOYMENT.md` for detailed instructions and troubleshooting.

