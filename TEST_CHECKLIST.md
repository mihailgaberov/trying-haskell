# Testing Checklist

## Manual Testing Steps

### 1. Start the Server
```bash
cabal run trying-haskell-web
```

You should see:
- "Database initialized"
- "Starting web server on port 3000"

### 2. Test Reset Button (Feature 1)
- [ ] Open http://localhost:3000
- [ ] Click "Start New Game"
- [ ] Verify "Reset Game" link appears under input field
- [ ] Click "Reset Game"
- [ ] Should return to welcome screen

### 3. Test Leaderboard - Name Input (Feature 2)
- [ ] Start a new game
- [ ] Play until you win (or make guesses to trigger win state)
- [ ] When you win, verify name modal appears
- [ ] Test valid name (1-10 alphanumeric chars): "TestUser"
- [ ] Test invalid names:
  - [ ] Empty name (should show error)
  - [ ] Name with special chars: "<script>" (should be sanitized/rejected)
  - [ ] Name too long: "ThisIsTooLong" (should be truncated/rejected)
- [ ] Test "Skip" button (should show win screen without submitting)
- [ ] Test "Submit" button (should submit and show win screen)

### 4. Test Leaderboard Display
- [ ] From welcome screen, click "Leaderboards" button
- [ ] Should show leaderboard table
- [ ] Verify table shows: Place | Name | Attempts | Winning Number
- [ ] If empty, should show "No entries yet" message
- [ ] After submitting a name, verify it appears in leaderboard
- [ ] Test "Back to Game" button (should return to welcome screen)

### 5. Test API Endpoints (using curl or browser console)

**Get Leaderboard:**
```bash
curl -X GET http://localhost:3000/api/leaderboard
```

**Submit Entry:**
```bash
curl -X POST http://localhost:3000/api/leaderboard \
  -H "Content-Type: application/json" \
  -d '{"name":"TestUser","attempts":5,"secret":42}'
```

**Test Invalid Name:**
```bash
curl -X POST http://localhost:3000/api/leaderboard \
  -H "Content-Type: application/json" \
  -d '{"name":"<script>alert(1)</script>","attempts":3,"secret":10}'
```
Should return 400 error with message about invalid name.

### 6. Test Mobile Responsiveness
- [ ] Open browser dev tools
- [ ] Switch to mobile view
- [ ] Test all features on mobile viewport
- [ ] Verify modal is usable on mobile
- [ ] Verify leaderboard table is scrollable on mobile

### 7. Verify Database
- [ ] Check that `leaderboard.db` file is created in project root
- [ ] Verify entries persist after server restart

## Expected Behavior

✅ Reset button works and returns to welcome screen  
✅ Name modal appears on win  
✅ Name validation works (rejects invalid names)  
✅ Leaderboard displays top 10 entries  
✅ Entries sorted by attempts (ascending), then timestamp  
✅ Leaderboard persists across server restarts  
✅ Mobile-friendly UI  

## Known Issues to Check

- [ ] Database file location (should be in project root)
- [ ] Name sanitization (special chars should be filtered)
- [ ] Leaderboard sorting (lowest attempts first)

