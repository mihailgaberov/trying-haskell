// Game state
let currentGameId = null;

// DOM elements
const welcomeScreen = document.getElementById("welcome-screen");
const gameScreen = document.getElementById("game-screen");
const winScreen = document.getElementById("win-screen");
const startBtn = document.getElementById("start-btn");
const guessBtn = document.getElementById("guess-btn");
const playAgainBtn = document.getElementById("play-again-btn");
const guessInput = document.getElementById("guess-input");
const attemptsCount = document.getElementById("attempts-count");
const hintArea = document.getElementById("hint-area");
const messageDiv = document.getElementById("message");
const winAttempts = document.getElementById("win-attempts");
const winSecret = document.getElementById("win-secret");
const resetLink = document.getElementById("reset-link");

// API base URL
const API_BASE = "";

// Start a new game
async function startGame() {
  try {
    const response = await fetch(`${API_BASE}/api/start`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
    });

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const data = await response.json();
    currentGameId = data.gameId;
    updateUI(data.state);

    // Show game screen
    welcomeScreen.style.display = "none";
    gameScreen.style.display = "block";
    winScreen.style.display = "none";

    // Focus input
    guessInput.focus();
    clearMessage();
  } catch (error) {
    showMessage("Error starting game: " + error.message, "error");
    console.error("Error starting game:", error);
  }
}

// Make a guess
async function makeGuess() {
  if (!currentGameId) {
    showMessage("Please start a game first", "error");
    return;
  }

  const guessValue = parseInt(guessInput.value);

  // Validate input
  if (isNaN(guessValue) || guessValue < 1 || guessValue > 100) {
    showMessage("Please enter a number between 1 and 100", "error");
    guessInput.focus();
    return;
  }

  try {
    const response = await fetch(`${API_BASE}/api/guess`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        gameId: currentGameId,
        guess: guessValue,
      }),
    });

    if (!response.ok) {
      if (response.status === 404) {
        showMessage("Game not found. Please start a new game.", "error");
        resetGame();
        return;
      }
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const data = await response.json();
    updateUI(data.state);

    // Clear input
    guessInput.value = "";
    guessInput.focus();
    clearMessage();
  } catch (error) {
    showMessage("Error making guess: " + error.message, "error");
    console.error("Error making guess:", error);
  }
}

// Update UI based on game state
function updateUI(state) {
  if (state.status === "playing") {
    attemptsCount.textContent = state.attempts;

    // Show hint if available
    if (state.hint) {
      hintArea.innerHTML = `<div class="hint ${
        state.hint.toLowerCase().includes("low") ? "hint-low" : "hint-high"
      }">${state.hint}</div>`;
    } else {
      hintArea.innerHTML = "";
    }
  } else if (state.status === "won") {
    // Game won!
    winSecret.textContent = state.secret;
    winAttempts.textContent = state.attempts;
    welcomeScreen.style.display = "none";
    gameScreen.style.display = "none";
    winScreen.style.display = "block";
    currentGameId = null;
  } else if (state.status === "notStarted") {
    // Reset to welcome screen
    resetGame();
  }
}

// Reset game to initial state
function resetGame() {
  currentGameId = null;
  guessInput.value = "";
  attemptsCount.textContent = "0";
  hintArea.innerHTML = "";
  welcomeScreen.style.display = "block";
  gameScreen.style.display = "none";
  winScreen.style.display = "none";
  clearMessage();
}

// Show message
function showMessage(msg, type = "info") {
  messageDiv.textContent = msg;
  messageDiv.className = `message message-${type}`;
  messageDiv.style.display = "block";
}

// Clear message
function clearMessage() {
  messageDiv.style.display = "none";
  messageDiv.textContent = "";
}

// Event listeners
startBtn.addEventListener("click", startGame);
guessBtn.addEventListener("click", makeGuess);
playAgainBtn.addEventListener("click", resetGame);
resetLink.addEventListener("click", (e) => {
  e.preventDefault();
  resetGame();
});

// Allow Enter key to submit guess
guessInput.addEventListener("keypress", (e) => {
  if (e.key === "Enter") {
    makeGuess();
  }
});

// Prevent form submission on Enter in input
guessInput.addEventListener("keydown", (e) => {
  if (e.key === "Enter") {
    e.preventDefault();
    makeGuess();
  }
});
