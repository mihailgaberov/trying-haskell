# Implementation Example: Scotty Web Server

This is a concrete example of how to add a web UI using Scotty.

## Step 1: Update `trying-haskell.cabal`

Add to `library` section:
```cabal
library
  hs-source-dirs: src
  exposed-modules: Game
  build-depends:
      base >=4.7 && <5
      , aeson >= 2.0
  default-language: Haskell2010
```

Add new executable:
```cabal
executable trying-haskell-web
  main-is: Web.hs
  hs-source-dirs: app
  build-depends:
      base >=4.7 && <5
    , trying-haskell
    , scotty >= 0.12
    , aeson >= 2.0
    , text >= 1.2
    , random >= 1.2
    , containers >= 0.6
    , wai-extra >= 3.1
  default-language: Haskell2010
```

## Step 2: Create `app/Web.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Game
import Web.Scotty
import Data.Aeson
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import System.Random (randomRIO)
import Data.UUID
import Data.UUID.V4

-- Simple game store
type GameId = Text
type GameStore = MVar (Map GameId GameState)

-- JSON instances for GameState and GameEvent
instance ToJSON GameState where
  toJSON NotStarted = object ["status" .= ("notStarted" :: Text)]
  toJSON (Playing s a h) = object 
    [ "status" .= ("playing" :: Text)
    , "attempts" .= a
    , "hint" .= h
    ]
  toJSON (Won s a) = object
    [ "status" .= ("won" :: Text)
    , "attempts" .= a
    ]

-- Request/Response types
data StartRequest = StartRequest deriving (Show)
data GuessRequest = GuessRequest { gameId :: GameId, guess :: Int } deriving (Show)

instance FromJSON GuessRequest where
  parseJSON = withObject "GuessRequest" $ \o ->
    GuessRequest <$> o .: "gameId" <*> o .: "guess"

-- API routes
app :: GameStore -> ScottyM ()
app store = do
  -- Start new game
  post "/api/start" $ do
    secret <- liftIO $ randomRIO (1, 100)
    gameId <- liftIO $ toText <$> nextRandom
    let state = applyEvent NotStarted (StartGame secret)
    liftIO $ modifyMVar_ store $ \m -> 
      return (Map.insert gameId state m, ())
    json $ object 
      [ "gameId" .= gameId
      , "state" .= state
      ]

  -- Make a guess
  post "/api/guess" $ do
    req <- jsonData
    mState <- liftIO $ readMVar store >>= \m -> 
      return $ Map.lookup (gameId req) m
    case mState of
      Nothing -> do
        status 404
        json $ object ["error" .= ("Game not found" :: Text)]
      Just state -> do
        let newState = applyEvent state (Guess (guess req))
        liftIO $ modifyMVar_ store $ \m ->
          return (Map.insert (gameId req) newState m, ())
        json $ object ["state" .= newState]

  -- Get game state
  get "/api/state/:gameId" $ do
    gid <- param "gameId"
    mState <- liftIO $ readMVar store >>= \m ->
      return $ Map.lookup gid m
    case mState of
      Nothing -> do
        status 404
        json $ object ["error" .= ("Game not found" :: Text)]
      Just state -> json $ object ["state" .= state]

  -- Serve static files
  middleware $ staticPolicy (addBase "static")

main :: IO ()
main = do
  store <- newMVar Map.empty
  scotty 3000 $ app store
```

## Step 3: Create `static/index.html`

```html
<!DOCTYPE html>
<html>
<head>
  <title>Number Guessing Game</title>
  <link rel="stylesheet" href="/style.css">
</head>
<body>
  <div class="container">
    <h1>🎯 Number Guessing Game</h1>
    <div id="game-area">
      <button id="start-btn">Start New Game</button>
      <div id="game-state"></div>
      <div id="input-area" style="display: none;">
        <input type="number" id="guess-input" min="1" max="100" placeholder="Enter guess (1-100)">
        <button id="guess-btn">Guess</button>
      </div>
      <div id="message"></div>
    </div>
  </div>
  <script src="/app.js"></script>
</body>
</html>
```

## Step 4: Create `static/app.js`

```javascript
let currentGameId = null;

async function startGame() {
  const response = await fetch('/api/start', { method: 'POST' });
  const data = await response.json();
  currentGameId = data.gameId;
  updateUI(data.state);
  document.getElementById('input-area').style.display = 'block';
  document.getElementById('start-btn').style.display = 'none';
}

async function makeGuess() {
  const input = document.getElementById('guess-input');
  const guess = parseInt(input.value);
  
  if (isNaN(guess) || guess < 1 || guess > 100) {
    showMessage('Please enter a number between 1 and 100');
    return;
  }

  const response = await fetch('/api/guess', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ gameId: currentGameId, guess: guess })
  });
  
  const data = await response.json();
  updateUI(data.state);
  input.value = '';
}

function updateUI(state) {
  const stateDiv = document.getElementById('game-state');
  const messageDiv = document.getElementById('message');
  
  if (state.status === 'playing') {
    stateDiv.innerHTML = `
      <p>Attempts: ${state.attempts}</p>
      ${state.hint ? `<p class="hint">${state.hint}</p>` : ''}
    `;
    messageDiv.textContent = '';
  } else if (state.status === 'won') {
    stateDiv.innerHTML = `<p class="won">🎉 You won in ${state.attempts} attempts!</p>`;
    document.getElementById('input-area').style.display = 'none';
    document.getElementById('start-btn').style.display = 'block';
    currentGameId = null;
  }
}

function showMessage(msg) {
  document.getElementById('message').textContent = msg;
}

// Event listeners
document.getElementById('start-btn').addEventListener('click', startGame);
document.getElementById('guess-btn').addEventListener('click', makeGuess);
document.getElementById('guess-input').addEventListener('keypress', (e) => {
  if (e.key === 'Enter') makeGuess();
});
```

## Step 5: Create `static/style.css`

```css
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  max-width: 600px;
  margin: 50px auto;
  padding: 20px;
  background: #f5f5f5;
}

.container {
  background: white;
  padding: 30px;
  border-radius: 10px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.1);
}

h1 {
  text-align: center;
  color: #333;
}

button {
  background: #4CAF50;
  color: white;
  border: none;
  padding: 10px 20px;
  border-radius: 5px;
  cursor: pointer;
  font-size: 16px;
}

button:hover {
  background: #45a049;
}

#input-area {
  margin: 20px 0;
}

input[type="number"] {
  padding: 10px;
  font-size: 16px;
  border: 2px solid #ddd;
  border-radius: 5px;
  margin-right: 10px;
  width: 150px;
}

.hint {
  color: #ff9800;
  font-weight: bold;
}

.won {
  color: #4CAF50;
  font-size: 20px;
  font-weight: bold;
}
```

## Step 6: Create `Dockerfile`

```dockerfile
FROM haskell:9.2 as builder

WORKDIR /app
COPY *.cabal ./
RUN cabal update

COPY . .
RUN cabal build --dependencies-only
RUN cabal build

FROM debian:bullseye-slim
RUN apt-get update && apt-get install -y \
    libgmp10 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/dist-newstyle/build/*/trying-haskell-*/x/trying-haskell-web/build/trying-haskell-web/trying-haskell-web .
COPY static ./static

EXPOSE 3000
ENV PORT=3000
CMD ["./trying-haskell-web"]
```

## Step 7: Run Locally

```bash
# Install dependencies
cabal build

# Run web server
cabal run trying-haskell-web

# Open browser to http://localhost:3000
```

## Step 8: Deploy to Render.com

1. Push code to GitHub
2. Create new "Web Service" on Render
3. Connect GitHub repo
4. Set:
   - Build Command: `cabal build`
   - Start Command: `cabal run trying-haskell-web`
   - Environment: `PORT=3000`

Or use Dockerfile:
- Set Dockerfile path: `Dockerfile`
- Render will auto-detect and use it

## That's it! 🎉

You now have a working web UI that can be deployed for free.

