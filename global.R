library(httr)
library(jsonlite)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(rchess)
library(dplyr)
library(shinyjs)  # Add this for enabling/disabling buttons

# Function to extract moves from PGN
extract_moves_from_pgn <- function(pgn) {
  pgn <- gsub("\\[.*?\\]", "", pgn)  # Remove metadata
  pgn <- gsub("\\{.*?\\}", "", pgn)  # Remove annotations
  moves <- unlist(strsplit(pgn, "\\s+"))  # Split by whitespace
  moves <- moves[!grepl("^\\.\\.\\.$|^\\d+\\.\\.\\.$|^\\d+\\.$|^1/2-1/2$|^1-0$|^0-1$", moves)]  # Remove unwanted elements
  moves <- moves[moves != ""]  # Remove empty strings
  return(moves)
}

# convert moves to FEN
moves_to_fen <- function(moves) {
  game <- Chess$new()
  if (!is.null(moves) && moves != "") {
    moves_list <- unlist(strsplit(moves, " "))
    for (move in moves_list) {
      game$move(move)
    }
  }
  return(game$fen())
}

# Function to get evaluation from Lichess
get_lichess_eval <- function(fen) {
  tryCatch({
    url <- paste0("https://lichess.org/api/cloud-eval?fen=", URLencode(fen))
    response <- GET(url)
    
    if (status_code(response) == 200) {
      eval_data <- fromJSON(rawToChar(response$content))
      
      if (!is.null(eval_data$pvs)) {
        # Check if the evaluation is in centipawns (cp) or mate
        if (!is.null(eval_data$pvs$cp)) {
          score <- eval_data$pvs$cp[[1]] / 100  # Convert centipawns to pawns
          return(round(score, 2))
        } else if (!is.null(eval_data$pvs$mate)) {
          # Handle forced mate (return as a string, e.g., "Mate in 3")
          return(paste("Mate in", abs(eval_data$pvs$mate[[1]])))
        }
      }
    } else {
      return(NA)  # Return NA if no evaluation is found
    }
    
  }, error = function(e) {
    return(NA)  # Return NA in case of any error
  })
}

# Function to split moves into 30 columns
split_moves <- function(moves, num_moves = 50) {
  mymoves <- rep(NA, num_moves)
  mymoves[1:min(num_moves, length(moves))] <- moves[1:min(num_moves, length(moves))]
  return(mymoves)
}

# Function to fetch user game data from Chess.com
get_user_game_data <- function(username) {
  tryCatch({
    archives_url <- paste0("https://api.chess.com/pub/player/", username, "/games/archives")
    response <- GET(archives_url, timeout(10))  # Add a timeout
    if (status_code(response) == 200) {
      archives <- na.omit(rev(fromJSON(rawToChar(response$content))$archives)[1:12])
      games_df <- do.call(bind_rows, lapply(archives, function(archive) {
        print(archive)
        Sys.sleep(0.5)  # Add a delay between requests
        games_data <- fromJSON(archive)$games
        if (is.null(games_data$pgn)) return(NULL)
        mat_moves <- do.call(rbind, lapply(games_data$pgn, function(pgn) {
          split_moves(extract_moves_from_pgn(pgn))
        }))
        colnames(mat_moves) <- paste0("Moves_", 1:50)
        
        games_df <- data.frame(
          url = games_data$url,
          time_control = as.character(games_data$time_control),
          time_class = games_data$time_class,
          end_time = as.POSIXct(games_data$end_time, origin = "1970-01-01"),
          rated = games_data$rated,
          white_username = games_data$white$username,
          white_rating = games_data$white$rating,
          white_result = games_data$white$result,
          black_username = games_data$black$username,
          black_rating = games_data$black$rating,
          black_result = games_data$black$result,
          stringsAsFactors = FALSE
        )
        cbind(games_df, mat_moves)
      }))
      
      #cond <- which(games_df$time_class == "blitz")
      #if (length(cond) > 0) games_df <- games_df[cond, , drop = FALSE]
      
      games_df <- games_df %>%
        mutate(user_outcome = case_when(
          white_username == username & white_result == "win" ~ "win",
          white_username == username & white_result %in% c("agreed", "insufficient", "repetition", "stalemate", "timevsinsufficient") ~ "draw",
          black_username == username & black_result == "win" ~ "win",
          black_username == username & black_result %in% c("agreed", "insufficient", "repetition", "stalemate", "timevsinsufficient") ~ "draw",
          white_result == "draw" | black_result == "draw" ~ "draw",
          TRUE ~ "loss"
        ))
      
      return(games_df)
    } else {
      showNotification("Failed to fetch data from Chess.com. Please try again.", type = "error")
      return(NULL)
    }
  }, error = function(e) {
    showNotification("An error occurred while fetching data. Please try again.", type = "error")
    return(NULL)
  })
}
