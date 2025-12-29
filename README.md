# 🎯 Number Guessing Game

A web-based number guessing game built with Haskell, designed to teach binary search algorithm concepts through interactive gameplay.

## 🎮 Features

- **Interactive Gameplay**: Guess a number between 1 and 100 with real-time hints
- **Binary Search Learning**: Encourages optimal guessing strategy (selecting mid-points)
- **Leaderboard System**: Track top 10 players with their best attempts
- **Player Profiles**: Save your name and compete with others
- **Reset Functionality**: Start fresh anytime during gameplay
- **Responsive Design**: Works seamlessly on desktop and mobile devices

## 🚀 Live Demo

[Play the game](https://trying-haskell.onrender.com/)

> **Note**: The demo is hosted on Render.com's free tier. If the link doesn't load immediately, please wait a moment for the service to spin up.

## 🛠️ Technology Stack

- **Backend**: Haskell with [Scotty](https://github.com/scotty-web/scotty) web framework
- **Database**: SQLite for persistent leaderboard storage
- **Frontend**: Vanilla HTML, CSS, and JavaScript
- **Deployment**: Docker container on Render.com

## 📚 Learning Objectives

This project serves as a practical introduction to:

- Functional programming with Haskell
- Web development using Haskell
- RESTful API design
- Database integration with SQLite
- Binary search algorithm concepts

## 🎓 Binary Search Strategy

The game naturally teaches binary search by encouraging players to:

1. Start with the middle number (50)
2. Based on the hint, select the next mid-point
3. Continue narrowing down until the number is found
4. Minimize attempts through optimal strategy

## 🏗️ Building Locally

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler) 9.10.1 or later
- [Cabal](https://www.haskell.org/cabal/) 3.8 or later

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/trying-haskell.git
cd trying-haskell

# Build the project
cabal build

# Run the web server
cabal run trying-haskell-web
```

The server will start on `http://localhost:3000`

### Docker

```bash
# Build the Docker image
docker build -t trying-haskell .

# Run the container
docker run -p 3000:3000 trying-haskell
```

## 📁 Project Structure

```
trying-haskell/
├── app/
│   ├── Main.hs      # CLI version
│   └── Web.hs       # Web server
├── src/
│   ├── Game.hs      # Core game logic
│   └── Database.hs  # SQLite database operations
├── static/
│   ├── index.html   # Frontend UI
│   ├── app.js       # Client-side logic
│   └── style.css    # Styling
├── Dockerfile       # Container configuration
└── trying-haskell.cabal  # Project dependencies
```

## 🎯 How to Play

1. Click "Start New Game" to begin
2. Enter your guess (1-100) in the input field
3. Receive hints: "Too high!" or "Too low!"
4. Use binary search strategy: always guess the middle of the remaining range
5. Win by finding the secret number in the fewest attempts
6. Enter your name to appear on the leaderboard
7. View the leaderboard to see top players

## 📝 License

This project is open source and available under the [LICENSE](LICENSE) file.

## 🤝 Contributing

Contributions, issues, and feature requests are welcome! Feel free to check the [issues page](https://github.com/yourusername/trying-haskell/issues).

---

Built with ❤️ using Haskell
