# Chess.com Move Explorer

## Overview
Chess.com Move Explorer is an interactive Shiny application that allows chess players to analyze their game patterns and performance statistics by navigating through their moves. This tool helps players understand their opening preferences, success rates with different moves, and identify areas for improvement in their chess strategy.

## Features
- Browse your Chess.com game history move by move
- Filter games by:
  - Date range
  - Game type (Blitz, Bullet, Rapid)
  - Playing color (White/Black)
- View detailed statistics for each possible next move:
  - Number of games
  - Win/Draw/Loss percentages
  - Average opponent rating
- Interactive chessboard to visualize positions
- Engine evaluation of positions using Lichess API
- Option to go back moves or reset the sequence

## How It Works
The application downloads your recent games from Chess.com via their public API and breaks them down into move sequences. As you click on moves, the app dynamically updates to show statistics for the next possible moves in the selected sequence, allowing you to explore your personal opening repertoire and see which variations have brought you more success.

## Demo
Try the app online: [Chess.com Explorer on shinyapps.io](https://aligot.shinyapps.io/ChesscomExplorer/)

## Acknowledgments
This application would not be possible without the following services:

- [Chess.com API](https://www.chess.com/news/view/published-data-api) - For providing access to user game data. Chess.com® is a registered trademark.
- [Lichess API](https://lichess.org/api) - For providing position evaluation services. Lichess is an open-source, community-driven chess server.
- [rchess](https://github.com/jbkunst/rchess) - R package for chess move validation and FEN handling.

## Installation and Usage
To run the application locally:

1. Clone this repository
2. Install required R packages:
```R
install.packages(c("shiny", "httr", "jsonlite", "DT", "shinycssloaders", "rchess", "dplyr", "shinyjs", "shinyWidgets"))
```
3. Run the app in R:
```R
shiny::runApp()
```

## Limitations
- The Chess.com API has rate limits, so downloading a large number of games may take time
- Only the most recent games (up to approximately one year) are analyzed
- Position evaluations depend on the availability of the Lichess API

## Contributing
Contributions, suggestions, and feature requests are welcome! Feel free to submit a pull request or open an issue.