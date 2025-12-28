# Quick Start: Adding Web UI

## TL;DR - Recommended Approach

**Use Scotty + Static HTML/JS + Render.com**

- ✅ Simplest to implement (~4-6 hours)
- ✅ Minimal dependencies
- ✅ Free deployment
- ✅ Easy to maintain

## Why This Approach?

Your current architecture is **perfect** for a web API:

1. **Pure game logic** (`Game` module) - already separated from I/O
2. **State/Event pattern** - maps naturally to REST endpoints
3. **Library structure** - easy to add web executable alongside CLI

## Implementation Checklist

- [ ] Add `scotty`, `aeson`, `text`, `wai-extra` to cabal dependencies
- [ ] Create `app/Web.hs` with REST API (3 endpoints)
- [ ] Create `static/` directory with HTML/CSS/JS
- [ ] Add `executable trying-haskell-web` to cabal file
- [ ] Test locally with `cabal run trying-haskell-web`
- [ ] Create `Dockerfile` for deployment
- [ ] Deploy to Render.com (free tier)

## Key Design Decisions

### Session Management

**MVP:** In-memory `Map GameId GameState`  
**Future:** Redis or PostgreSQL if you need persistence

### Frontend

**MVP:** Vanilla JavaScript (no build tools)  
**Future:** Can add React/Vue later if needed

### API Style

**MVP:** REST with JSON  
**Future:** Can add WebSockets for real-time if needed

## File Structure After Implementation

```
trying-haskell/
├── src/
│   └── Game.hs              (unchanged - pure logic)
├── app/
│   ├── Main.hs              (CLI - unchanged)
│   └── Web.hs               (NEW - web server)
├── static/                  (NEW)
│   ├── index.html
│   ├── style.css
│   └── app.js
├── trying-haskell.cabal     (add web executable)
├── Dockerfile               (NEW - for deployment)
└── README.md
```

## Next Steps

1. Read `UI_OPTIONS.md` for detailed analysis
2. Follow `IMPLEMENTATION_EXAMPLE.md` for step-by-step code
3. Test locally
4. Deploy to Render.com

## Questions?

The implementation is straightforward because:

- Your game logic is already pure and testable
- No complex state management needed
- Simple request/response pattern
- Static frontend = no build complexity

**Estimated time to working web UI: 4-6 hours**
