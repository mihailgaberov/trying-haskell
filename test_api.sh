#!/bin/bash

echo "=== Testing Game API ==="
echo "1. Starting a game..."
GAME_RESPONSE=$(curl -s -X POST http://localhost:3000/api/start)
echo "$GAME_RESPONSE"
GAME_ID=$(echo "$GAME_RESPONSE" | grep -o '"gameId":"[^"]*"' | cut -d'"' -f4)
echo "Game ID: $GAME_ID"
echo ""

echo "2. Making a guess..."
GUESS_RESPONSE=$(curl -s -X POST http://localhost:3000/api/guess -H "Content-Type: application/json" -d "{\"gameId\":\"$GAME_ID\",\"guess\":50}")
echo "$GUESS_RESPONSE"
echo ""

echo "=== Testing Leaderboard API ==="
echo "3. Getting empty leaderboard..."
curl -s -X GET http://localhost:3000/api/leaderboard
echo ""
echo ""

echo "4. Submitting a valid entry..."
SUBMIT_RESPONSE=$(curl -s -X POST http://localhost:3000/api/leaderboard -H "Content-Type: application/json" -d '{"name":"TestUser","attempts":5,"secret":42}')
echo "$SUBMIT_RESPONSE"
echo ""

echo "5. Getting leaderboard (should have 1 entry)..."
curl -s -X GET http://localhost:3000/api/leaderboard
echo ""
echo ""

echo "6. Testing name sanitization (invalid name)..."
curl -s -X POST http://localhost:3000/api/leaderboard -H "Content-Type: application/json" -d '{"name":"<script>alert(1)</script>","attempts":3,"secret":10}'
echo ""
echo ""

echo "7. Testing name sanitization (too long)..."
curl -s -X POST http://localhost:3000/api/leaderboard -H "Content-Type: application/json" -d '{"name":"ThisNameIsTooLongForTheLimit","attempts":2,"secret":20}'
echo ""
echo ""

echo "8. Getting final leaderboard..."
curl -s -X GET http://localhost:3000/api/leaderboard
echo ""
echo ""

echo "=== Test Complete ==="

