# UI Options Analysis for Number Guessing Game

## Current Architecture Assessment

✅ **Strengths:**
- Clean separation: pure game logic (`Game` module) vs. I/O (`Main`)
- `GameState` and `GameEvent` are perfect for web APIs
- Library structure makes it easy to add multiple interfaces
- Pure functions (`applyEvent`) are easily testable and reusable

## Recommended Solutions (Ranked by Simplicity)

### 🥇 **Option 1: Scotty + Static HTML/JS (RECOMMENDED)**

**Why:** Simplest web framework, minimal dependencies, easy deployment

**Architecture:**
- REST API with Scotty (lightweight web framework)
- Simple HTML/JavaScript frontend (vanilla JS, no build step)
- Session-based state management (in-memory or Redis)

**Pros:**
- ✅ Minimal code (~100 lines for API)
- ✅ No frontend build tools needed
- ✅ Fast to implement (1-2 hours)
- ✅ Easy to deploy on Render/Fly.io
- ✅ Small dependency footprint

**Cons:**
- ⚠️ Session management needed (can use simple Map for MVP)
- ⚠️ No real-time updates (polling or refresh)

**Dependencies:**
```cabal
scotty >= 0.12
wai-extra >= 3.1
text >= 1.2
aeson >= 2.0  -- JSON encoding
```

**Deployment:** Render.com (free tier), Fly.io (free tier)

---

### 🥈 **Option 2: Servant + Static HTML/JS**

**Why:** Type-safe API, excellent documentation, production-ready

**Architecture:**
- Type-safe REST API with Servant
- Same simple HTML/JS frontend
- Better for larger projects

**Pros:**
- ✅ Type-safe API (compile-time guarantees)
- ✅ Auto-generated API docs
- ✅ Excellent for growing projects
- ✅ Strong ecosystem

**Cons:**
- ⚠️ More complex than Scotty
- ⚠️ Steeper learning curve
- ⚠️ More dependencies

**Dependencies:**
```cabal
servant >= 0.19
servant-server >= 0.19
warp >= 3.3
aeson >= 2.0
```

**Deployment:** Same as Option 1

---

### 🥉 **Option 3: WAI/Warp (Minimal)**

**Why:** Lowest-level, maximum control, minimal dependencies

**Architecture:**
- Direct WAI application
- Manual routing and JSON handling
- Same frontend approach

**Pros:**
- ✅ Minimal dependencies (just `warp`, `wai`, `aeson`)
- ✅ Full control
- ✅ Smallest binary size

**Cons:**
- ⚠️ More boilerplate
- ⚠️ Manual routing/parsing
- ⚠️ Less ergonomic

**Dependencies:**
```cabal
warp >= 3.3
wai >= 3.2
aeson >= 2.0
```

---

### ❌ **Not Recommended (Too Complex for MVP):**

- **Yesod:** Full-stack framework, overkill for this project
- **Miso/Reflex:** GHCJS compilation, complex setup, large bundle size
- **Threepenny-gui:** Desktop GUI, not web-deployable

---

## Implementation Plan for Option 1 (Scotty)

### File Structure:
```
trying-haskell/
├── src/
│   └── Game.hs              (existing)
├── app/
│   ├── Main.hs              (CLI - existing)
│   └── Web.hs               (NEW - web server)
├── static/                  (NEW)
│   ├── index.html
│   ├── style.css
│   └── app.js
├── trying-haskell.cabal
└── Dockerfile               (for deployment)
```

### API Endpoints:
```
POST /api/start     -> Start new game, returns gameId
POST /api/guess     -> {gameId, guess} -> returns new state
GET  /api/state/:id -> Get current game state
```

### Session Management:
- Simple `Map GameId GameState` in memory (for MVP)
- Can upgrade to Redis/PostgreSQL later if needed

---

## Deployment Options (Free Tiers)

### 1. **Render.com** ⭐ (Easiest)
- Free tier: 750 hours/month
- Auto-deploy from GitHub
- Supports Docker
- Simple configuration

**Setup:**
- Dockerfile required
- Environment variables for port
- Auto HTTPS

### 2. **Fly.io** ⭐ (Best for Haskell)
- Free tier: 3 shared VMs
- Excellent Haskell support
- Global edge deployment
- Simple `fly.toml` config

**Setup:**
- `fly.toml` configuration
- Dockerfile or native build
- Automatic HTTPS

### 3. **Railway**
- Free tier: $5 credit/month
- GitHub integration
- Simple setup

### 4. **Heroku** (Limited)
- Free tier discontinued, but has low-cost options
- Good documentation

---

## Recommended Next Steps

1. **Start with Option 1 (Scotty)** - fastest to implement
2. **Create minimal REST API** - 3 endpoints
3. **Simple HTML/JS frontend** - vanilla JavaScript
4. **Add Dockerfile** - for containerization
5. **Deploy to Render.com** - easiest free option

## Estimated Implementation Time

- **Scotty API:** 2-3 hours
- **Frontend:** 1-2 hours  
- **Docker setup:** 30 minutes
- **Deployment:** 30 minutes

**Total: ~4-6 hours for complete web UI**

---

## Code Example Structure (Scotty)

```haskell
-- app/Web.hs
module Web where

import Game
import Web.Scotty
import Data.Aeson
import Control.Monad.IO.Class
import System.Random

-- Simple in-memory game store
type GameStore = MVar (Map GameId GameState)

-- API routes
app :: GameStore -> ScottyM ()
app store = do
  post "/api/start" $ do
    secret <- liftIO $ randomRIO (1, 100)
    gameId <- liftIO $ newGameId
    let state = applyEvent NotStarted (StartGame secret)
    liftIO $ addGame store gameId state
    json $ object ["gameId" .= gameId, "state" .= state]
  
  post "/api/guess" $ do
    body <- jsonData
    -- ... handle guess
```

---

## Questions to Consider

1. **State persistence:** In-memory (simple) vs. Database (scalable)
2. **Real-time:** Polling (simple) vs. WebSockets (complex)
3. **Styling:** Plain CSS vs. Framework (Bootstrap/Tailwind)
4. **Mobile:** Responsive CSS vs. Native app (future)

---

## Conclusion

**Best choice for MVP:** Scotty + Static HTML/JS + Render.com

This combination gives you:
- ✅ Fastest implementation
- ✅ Simplest codebase
- ✅ Easy deployment
- ✅ Free hosting
- ✅ Room to grow (can add features incrementally)

