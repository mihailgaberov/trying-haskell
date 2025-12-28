# Deployment Guide

This guide covers deploying the Number Guessing Game to free hosting platforms.

## Prerequisites

- Git repository pushed to GitHub (or GitLab/Bitbucket)
- Docker installed (for local testing)

## Option 1: Render.com (Recommended - Easiest)

### Steps:

1. **Push to GitHub**

   ```bash
   git add .
   git commit -m "Add web UI and deployment config"
   git push origin main
   ```

2. **Create Render Account**

   - Go to [render.com](https://render.com)
   - Sign up with GitHub

3. **Create New Web Service**

   - Click "New +" → "Web Service"
   - Connect your GitHub repository
   - Select the `trying-haskell` repository

4. **Configure Service**

   - **Name:** `trying-haskell` (or any name you prefer)
   - **Environment:** `Docker`
   - **Region:** Choose closest to you
   - **Branch:** `main` (or your default branch)
   - **Root Directory:** Leave empty (root of repo)
   - **Dockerfile Path:** `Dockerfile`
   - **Docker Build Context:** `.` (root)

5. **Environment Variables** (Optional)

   - No environment variables needed for basic setup
   - `PORT` is automatically set by Render

6. **Deploy**

   - Click "Create Web Service"
   - Render will build and deploy automatically
   - First build takes ~10-15 minutes (downloading dependencies)
   - Subsequent builds are faster (~5 minutes)

7. **Access Your App**
   - Once deployed, you'll get a URL like: `https://trying-haskell.onrender.com`
   - The app will be live and accessible!

### Render.com Free Tier Limits:

- ✅ 750 hours/month (enough for always-on service)
- ✅ Auto-deploy from GitHub
- ✅ HTTPS automatically enabled
- ⚠️ Spins down after 15 minutes of inactivity (first request after spin-down takes ~30 seconds)

---

## Option 2: Fly.io

### Steps:

1. **Install Fly CLI**

   ```bash
   curl -L https://fly.io/install.sh | sh
   ```

2. **Login**

   ```bash
   fly auth login
   ```

3. **Create `fly.toml`** (see below)

4. **Deploy**
   ```bash
   fly deploy
   ```

### Create `fly.toml`:

```toml
app = "trying-haskell"
primary_region = "iad"

[build]
  dockerfile = "Dockerfile"

[http_service]
  internal_port = 3000
  force_https = true
  auto_stop_machines = true
  auto_start_machines = true
  min_machines_running = 0
  processes = ["app"]

[[vm]]
  memory_mb = 256
```

### Fly.io Free Tier:

- ✅ 3 shared VMs
- ✅ 160GB outbound data transfer
- ✅ Global edge network
- ✅ No spin-down (always available)

---

## Option 3: Railway

### Steps:

1. **Go to Railway**

   - Visit [railway.app](https://railway.app)
   - Sign up with GitHub

2. **New Project**

   - Click "New Project"
   - Select "Deploy from GitHub repo"
   - Choose your repository

3. **Configure**

   - Railway auto-detects Dockerfile
   - No additional config needed

4. **Deploy**
   - Railway builds and deploys automatically
   - Get your URL from the dashboard

### Railway Free Tier:

- ✅ $5 credit/month
- ✅ Simple setup
- ✅ Auto-deploy

---

## Local Docker Testing

Before deploying, test the Docker image locally:

```bash
# Build the image
docker build -t trying-haskell .

# Run the container
docker run -p 3000:3000 trying-haskell

# Test in browser
open http://localhost:3000
```

---

## Troubleshooting

### Build Fails on Render

**Issue:** Build timeout or dependency resolution fails

**Solution:**

- Ensure all dependencies are in `trying-haskell.cabal`
- Check that GHC version in Dockerfile matches your local (9.2)
- Try building locally with Docker first

### Port Issues

**Issue:** App doesn't start

**Solution:**

- Ensure `PORT` environment variable is used (already implemented)
- Check logs in Render dashboard

### Static Files Not Loading

**Issue:** CSS/JS not found

**Solution:**

- Verify `static/` directory is copied in Dockerfile
- Check file paths in HTML (should be `/style.css`, `/app.js`)

### Game State Lost on Restart

**Issue:** Games reset when server restarts

**Solution:**

- This is expected with in-memory storage
- For persistence, add Redis or PostgreSQL (future enhancement)

---

## Post-Deployment Checklist

- [ ] Test the app at the deployed URL
- [ ] Verify all static files load (CSS, JS)
- [ ] Test game flow (start → guess → win)
- [ ] Check mobile responsiveness
- [ ] Verify HTTPS is working

---

## Next Steps (Optional Enhancements)

1. **Add Domain Name**

   - Render: Add custom domain in dashboard
   - Fly.io: `fly domains add yourdomain.com`

2. **Add Persistence**

   - Use Redis for game state
   - Or PostgreSQL for full persistence

3. **Add Monitoring**

   - Render: Built-in logs
   - Fly.io: `fly logs`
   - Add health check endpoint (already included at `/health`)

4. **CI/CD**
   - Already set up with auto-deploy from GitHub
   - Add tests to run before deployment

---

## Quick Deploy Commands

### Render.com

```bash
# Just push to GitHub, Render handles the rest
git push origin main
```

### Fly.io

```bash
fly deploy
```

### Railway

```bash
# Push to GitHub, Railway auto-deploys
git push origin main
```

---

## Support

If you encounter issues:

1. Check the deployment platform's logs
2. Test Docker build locally first
3. Verify all files are committed to Git
4. Check environment variables

Good luck with deployment! 🚀
