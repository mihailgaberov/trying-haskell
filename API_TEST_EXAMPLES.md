# API Test Examples

## Correct Usage

### 1. Health Check (GET)
```bash
curl -X GET http://localhost:3000/health
# Response: {"status":"ok"}
```

### 2. Start a New Game (POST)
```bash
curl -X POST http://localhost:3000/api/start
# Response: {"gameId":"<uuid>","state":{"status":"playing","attempts":0,"hint":null}}
```

### 3. Make a Guess (POST with JSON body)
```bash
# First, start a game and save the gameId
GAME_ID=$(curl -s -X POST http://localhost:3000/api/start | grep -o '"gameId":"[^"]*"' | cut -d'"' -f4)

# Then make a guess
curl -X POST http://localhost:3000/api/guess \
  -H "Content-Type: application/json" \
  -d "{\"gameId\":\"$GAME_ID\",\"guess\":50}"

# Response: {"state":{"status":"playing","attempts":1,"hint":"Too low!"}}
# or: {"state":{"status":"won","attempts":1}}
```

### 4. Get Game State (GET)
```bash
curl -X GET http://localhost:3000/api/state/<gameId>
# Response: {"state":{"status":"playing","attempts":1,"hint":"Too low!"}}
```

## Common Mistakes

❌ **Wrong:** `curl -X POST http://localhost:3000/health`
- `/health` is a GET endpoint, not POST

❌ **Wrong:** `curl -X POST http://localhost:3000/api/guess`
- Missing JSON body. Need: `{"gameId":"...","guess":42}`

❌ **Wrong:** `curl -X POST http://localhost:3000/api/state/1`
- `/api/state/:gameId` is a GET endpoint, not POST

## Complete Game Flow Example

```bash
# 1. Start a game
RESPONSE=$(curl -s -X POST http://localhost:3000/api/start)
GAME_ID=$(echo $RESPONSE | grep -o '"gameId":"[^"]*"' | cut -d'"' -f4)
echo "Game started! ID: $GAME_ID"

# 2. Make guesses
curl -X POST http://localhost:3000/api/guess \
  -H "Content-Type: application/json" \
  -d "{\"gameId\":\"$GAME_ID\",\"guess\":50}"

curl -X POST http://localhost:3000/api/guess \
  -H "Content-Type: application/json" \
  -d "{\"gameId\":\"$GAME_ID\",\"guess\":25}"

# 3. Check state
curl -X GET http://localhost:3000/api/state/$GAME_ID
```

