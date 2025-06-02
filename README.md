# HolyHex

[![Netlify Status](https://api.netlify.com/api/v1/badges/09eee0e5-0c2e-4b34-a6e7-a6a56bfa8419/deploy-status)](https://app.netlify.com/projects/enchanting-zabaione-b3d7ad/deploys)

A web-based, interactive word puzzle game built with a unique hexagonal grid interface using Elm. Players guess 5-letter words to solve a hidden word within 6 attempts, with feedback provided through colored hexagons. The game includes a hint system, making it engaging and challenging.

## Table of Contents

- [Features](https://grok.com/chat/e653fd46-50d2-4c4e-a99f-b0aae30dc802?referrer=website#features)
- [Technologies](https://grok.com/chat/e653fd46-50d2-4c4e-a99f-b0aae30dc802?referrer=website#technologies)
- [Installation](https://grok.com/chat/e653fd46-50d2-4c4e-a99f-b0aae30dc802?referrer=website#installation)
- [Usage](https://grok.com/chat/e653fd46-50d2-4c4e-a99f-b0aae30dc802?referrer=website#usage)
- [Project Structure](https://grok.com/chat/e653fd46-50d2-4c4e-a99f-b0aae30dc802?referrer=website#project-structure)

## Features

- **Hexagonal Grid Interface**: A visually appealing honeycomb layout for word input.

- Dynamic Gameplay

  - Players guess 5-letter words in up to 6 rows.
  - Feedback is provided via colored hexagons:
    - **Green**: Correct letter in the correct position.
    - **Yellow**: Correct letter in the wrong position.
    - **Gray**: Letter not in the target word.

- **Hint System**: Provides hints for the target word to assist players.

- **Responsive Design**: Adapts to different screen sizes for a seamless experience.

- Word Validation

  - Uses a comprehensive database of valid guesses and target words.
  - Ensures guesses are valid 5-letter words.

- Game Controls

  - Start a new game or request a hint with intuitive buttons.
  - Displays game messages for feedback (e.g., "Correct!", "Invalid word").

## Technologies

- **Elm (0.19.1)**: A functional programming language for building reliable web applications.

- **HTML/CSS**: For structuring and styling the user interface.

- **JavaScript**: For initializing the Elm game.

- Dependencies

  - `elm/browser`: For browser interaction.
  - `elm/http`: For fetching word databases.
  - `elm/json`: For JSON decoding.
  - `elm/seed`: For selecting random target words.
  - `NoRedInk/elm-json-decode-pipeline`: For streamlined JSON decoding.

- Word Databases

  - `answerlist.json`: Contains 2,309 target words.
  - `validguess.json`: Contains valid guess words for input validation.

## Installation

To run the HolyHex game locally, follow these steps:

### Prerequisites

- **Node.js**: Required for running a local server and managing dependencies.

- Elm

  : Install Elm globally using npm:

  ```bash
  npm install -g elm
  ```

- A modern web browser (e.g., Chrome, Firefox).

### Steps

1. **Clone the Repository**:

   ```bash
   git clone <repository-url>
   cd holyhex
   ```

2. **Install Elm Dependencies**:
   Navigate to the project directory and install Elm dependencies:

   ```bash
   elm install
   ```

3. **Set Up Word Databases**:
   Ensure the `answerlist.json` and `validguess.json` files are in the project root or accessible via a server. These files contain the target words and valid guesses, respectively.

4. **Serve the Game**:
   Use a local server to serve the game. You can use `elm reactor` or a simple HTTP server:

   - Using Elm Reactor:

     ```bash
     elm reactor
     ```

     Open 

     ```
     http://localhost:8000
     ```

      and navigate to 

     ```
     index.html
     ```

   - Using a Node.js HTTP server:

     ```bash
     npm install -g http-server
     http-server .
     ```

     Open 

     ```
     http://localhost:8080
     ```

5. **Build the Game (Optional)**:
   To compile the Elm code into a production-ready JavaScript file:

   ```bash
   elm make src/Main.elm --output=main.js
   ```

## Usage

1. **Start the Game**:
   - Open the game in a browser.
   - The game initializes with a puzzle.
2. **Play the Game**:
   - Click on hexagons in the active row to focus and type letters using your keyboard.
   - Press **Enter** to submit a 5-letter word.
   - Use feedback from the colored hexagons to refine your guesses:
     - Green: Correct letter and position.
     - Yellow: Correct letter, wrong position.
     - Gray: Letter not in the word.
   - Complete rows in order, with up to 6 attempts to guess the target word.
3. **Use Hints**:
   - Click the "Hint" button to reveal the target word (useful for assistance).
4. **View Results**:
   - If you guess correctly, a success modal displays your achievement and the number of guesses.
   - If you exhaust all attempts, the game ends, and a modal shows the outcome.
5. **Start a New Game**:
   - Click "New Game" to reset the grid and select a new target word.

## Project Structure

```
holyhex/
├── src/
│   └── Main.elm          # Main Elm game logic
├── answerlist.json       # Target words database
├── validguess.json       # Valid guesses database
├── elm.json              # Elm project configuration
├── index.html            # HTML entry point
├── main.js               # Compiled Elm JavaScript
├── style.css             # CSS styles (optional, not provided in code)
└── README.md             # This file
```

- **Main.elm**: Contains the game logic, including the model, update, view, and subscriptions.
- **answerlist.json**: Contains 2,309 target words.
- **validguess.json**: Lists 14,854 valid guess words, including target words.
- **index.html**: The entry point that loads the Elm game.
- **main.js**: The compiled Elm code, handling the game’s functionality.

