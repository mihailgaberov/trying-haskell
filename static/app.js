// Game state
let currentGameId = null;
let currentWinData = null; // Store {attempts, secret} when player wins

// DOM elements
const welcomeScreen = document.getElementById("welcome-screen");
const gameScreen = document.getElementById("game-screen");
const winScreen = document.getElementById("win-screen");
const leaderboardScreen = document.getElementById("leaderboard-screen");
const startBtn = document.getElementById("start-btn");
const guessBtn = document.getElementById("guess-btn");
const playAgainBtn = document.getElementById("play-again-btn");
const leaderboardBtn = document.getElementById("leaderboard-btn");
const backToGameBtn = document.getElementById("back-to-game-btn");
const guessInput = document.getElementById("guess-input");
const attemptsCount = document.getElementById("attempts-count");
const hintArea = document.getElementById("hint-area");
const messageDiv = document.getElementById("message");
const winAttempts = document.getElementById("win-attempts");
const winSecret = document.getElementById("win-secret");
const resetLink = document.getElementById("reset-link");
const nameModal = document.getElementById("name-modal");
const nameInput = document.getElementById("name-input");
const submitNameBtn = document.getElementById("submit-name-btn");
const skipNameBtn = document.getElementById("skip-name-btn");
const leaderboardBody = document.getElementById("leaderboard-body");

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
    // Game won! Store win data and show name modal
    winSecret.textContent = state.secret;
    winAttempts.textContent = state.attempts;
    currentWinData = { attempts: state.attempts, secret: state.secret };
    currentGameId = null;
    
    // Show name input modal first
    nameModal.style.display = "flex";
    nameInput.focus();
  } else if (state.status === "notStarted") {
    // Reset to welcome screen
    resetGame();
  }
}

// Reset game to initial state
function resetGame() {
  currentGameId = null;
  currentWinData = null;
  guessInput.value = "";
  attemptsCount.textContent = "0";
  hintArea.innerHTML = "";
  nameModal.style.display = "none";
  nameInput.value = "";
  welcomeScreen.style.display = "block";
  gameScreen.style.display = "none";
  winScreen.style.display = "none";
  leaderboardScreen.style.display = "none";
  clearMessage();
}

// Submit name to leaderboard
async function submitName() {
  if (!currentWinData) return;
  
  const name = nameInput.value.trim();
  if (!name || name.length === 0) {
    showMessage("Please enter a name", "error");
    return;
  }
  
  try {
    const response = await fetch(`${API_BASE}/api/leaderboard`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        name: name,
        attempts: currentWinData.attempts,
        secret: currentWinData.secret,
      }),
    });
    
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    // Hide modal and show win screen
    nameModal.style.display = "none";
    welcomeScreen.style.display = "none";
    gameScreen.style.display = "none";
    winScreen.style.display = "block";
    leaderboardScreen.style.display = "none";
    currentWinData = null;
  } catch (error) {
    showMessage("Error submitting to leaderboard: " + error.message, "error");
    console.error("Error submitting name:", error);
  }
}

// Skip name submission
function skipName() {
  nameModal.style.display = "none";
  welcomeScreen.style.display = "none";
  gameScreen.style.display = "none";
  winScreen.style.display = "block";
  leaderboardScreen.style.display = "none";
  currentWinData = null;
}

// Load and display leaderboard
async function loadLeaderboard() {
  try {
    const response = await fetch(`${API_BASE}/api/leaderboard`);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    const data = await response.json();
    displayLeaderboard(data.entries);
    
    // Show leaderboard screen
    welcomeScreen.style.display = "none";
    gameScreen.style.display = "none";
    winScreen.style.display = "none";
    leaderboardScreen.style.display = "block";
  } catch (error) {
    showMessage("Error loading leaderboard: " + error.message, "error");
    console.error("Error loading leaderboard:", error);
  }
}

// Display leaderboard entries
function displayLeaderboard(entries) {
  leaderboardBody.innerHTML = "";
  
  if (entries.length === 0) {
    leaderboardBody.innerHTML = "<tr><td colspan='4' style='text-align: center; padding: 20px;'>No entries yet. Be the first!</td></tr>";
    return;
  }
  
  entries.forEach((entry, index) => {
    const row = document.createElement("tr");
    row.innerHTML = `
      <td>${index + 1}</td>
      <td>${escapeHtml(entry.name)}</td>
      <td>${entry.attempts}</td>
      <td>${entry.secret}</td>
    `;
    leaderboardBody.appendChild(row);
  });
}

// Escape HTML to prevent XSS
function escapeHtml(text) {
  const div = document.createElement("div");
  div.textContent = text;
  return div.innerHTML;
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
leaderboardBtn.addEventListener("click", loadLeaderboard);
backToGameBtn.addEventListener("click", resetGame);
submitNameBtn.addEventListener("click", submitName);
skipNameBtn.addEventListener("click", skipName);

// Reset link event listener
if (resetLink) {
  resetLink.addEventListener("click", (e) => {
    e.preventDefault();
    resetGame();
  });
}

// Allow Enter key in name input
nameInput.addEventListener("keypress", (e) => {
  if (e.key === "Enter") {
    submitName();
  }
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
