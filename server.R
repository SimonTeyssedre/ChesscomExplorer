server <- function(input, output, session) {
  user_data <- reactiveVal(NULL)
  move_sequence <- reactiveVal("")
  
  # Initialize a chess game object
  chess_game <- reactiveVal(Chess$new())
  
  observeEvent(input$go, {
    req(input$username)
    withProgress(message = "Downloading games...", {
      games <- get_user_game_data(input$username)
      user_data(games)
      move_sequence("")
      chess_game(Chess$new())  # Reset the chess game
    })
  })
  
  output$move_sequence <- renderText({
    move_sequence()
  })
  
  observeEvent(input$reset, {
    move_sequence("")
    chess_game(Chess$new())  # Reset the chess game
  })
  
  move_stats <- reactive({
    req(user_data())
    
    df <- user_data()
    current_sequence <- trimws(move_sequence())
    
    df <- df %>% 
      filter(as.Date(end_time) >= input$date_filter[1] & 
               as.Date(end_time) <= input$date_filter[2])
    
    if (input$color_filter == "White") {
      df <- df %>% filter(white_username == input$username)
      df$Rating <- df$black_rating
    } else if (input$color_filter == "Black") {
      df <- df %>% filter(black_username == input$username)
      df$Rating <- df$white_rating
    }
    
    if (current_sequence != "") {
      sequence_moves <- unlist(strsplit(current_sequence, " "))
      for (i in seq_along(sequence_moves)) {
        df <- df %>% filter(df[[paste0("Moves_", i)]] == sequence_moves[i])
      }
    }
    next_move_col <- paste0("Moves_", ifelse(current_sequence == "", 1, 
                                             length(unlist(strsplit(current_sequence, " "))) + 1))
    df <- df %>%
      group_by(!!sym(next_move_col)) %>%
      summarise(
        games = n(),
        wins = sum(user_outcome == "win"),
        draws = sum(user_outcome == "draw"),
        losses = sum(user_outcome == "loss"),
        rating = mean(Rating, na.rm=TRUE),
      ) %>%
      arrange(desc(games)) %>%
      rename(Move = !!sym(next_move_col))
    
    df$rating <- round(df$rating,digits=0)
    
    if (nrow(df) > 0) {
      df <- df %>%
        mutate(
          wins = round(100 * wins / games, digits = 0),
          draws = round(100 * draws / games, digits = 0),
          losses = round(100 * losses / games, digits = 0),
          Rating =  round(rating, digits = 0),
          Bar = paste0(
            "<div style='display: flex; width: 100%; height: 25px; border-radius: 4px; overflow: hidden;'>",
            "<div style='background: #27ae60; width: ", wins, "%; display: flex; align-items: center; justify-content: center; color: white; font-size: 12px;'>", wins, "%</div>",
            "<div style='background: #f1c40f; width: ", draws, "%;'></div>",  # Removed text for draws
            "<div style='background: #e74c3c; width: ", losses, "%; display: flex; align-items: center; justify-content: center; color: white; font-size: 12px;'>", losses, "%</div>",
            "</div>"
          )
        )
    }
    
    cond <- which(is.na(df$Move))
    if(length(cond)>0) df <- df[-cond,,drop=FALSE]
    
    df
  })
  
  position_evals <- reactiveVal(list())
  
  # Format evaluation value
  format_eval <- function(eval) {
    if (is.null(eval) || length(eval) == 0) return("")
    if (is.na(eval)) return("")
    if (is.character(eval) && grepl("Mate", eval)) {
      return(paste0('<span class="eval-value eval-positive">', eval, '</span>'))
    }
    color <- if (eval >= 0) "eval-positive" else "eval-negative"
    sprintf('<span class="eval-value %s">%+.2f</span>', color, eval)
  }
  
  # Handle batch evaluation
  observeEvent(input$evaluate_all, {
    req(move_stats())
    
    # Disable button and show progress
    shinyjs::disable("evaluate_all")
    
    # Get all current move sequences
    moves_df <- move_stats()
    current_sequence <- move_sequence()
    
    # Create complete move sequences
    move_sequences <- lapply(moves_df$Move, function(move) {
      if (current_sequence == "") return(move)
      trimws(paste(current_sequence, move))
    })
    
    print(move_sequences)
    
    # Initialize progress bar
    total <- length(move_sequences)
    progress <- Progress$new(session, min = 0, max = total)
    progress$set(message = "Evaluating positions...")
    
    # Evaluate positions
    current_evals <- position_evals()
    for (i in seq_along(move_sequences)) {
      moves <- move_sequences[[i]]
      if (is.null(current_evals[[moves]])) {
        fen <- moves_to_fen(moves)
        eval <- get_lichess_eval(fen)
        print(paste("Evaluated:", moves, "->", eval))  # Debugging: Print the move sequence and evaluation
        current_evals[[moves]] <- eval
        progress$set(value = i)
        Sys.sleep(0.1)  # Rate limiting
      }
    }
    
    position_evals(current_evals)
    progress$close()
    shinyjs::enable("evaluate_all")
    
    # Debugging: Print the updated position_evals
    print("Updated position_evals:")
    print(position_evals())
  })
  
  # Render the chessboard
  output$chessboard <- rchess::renderChessboardjs({
    req(chess_game())
    
    # Get the current FEN position from the chess game
    fen <- chess_game()$fen()
    
    # Render the board with the current position
    rchess::chessboardjs(fen)
  })
  
  # Update the chess game when a move is selected
  observeEvent(input$move_stats_table_rows_selected, {
    selected_row <- input$move_stats_table_rows_selected
    if (!is.null(selected_row)) {
      selected_move <- move_stats()$Move[selected_row]
      current_sequence <- move_sequence()
      new_sequence <- trimws(paste(current_sequence, selected_move))
      
      # Split the move sequence into individual moves
      moves <- unlist(strsplit(new_sequence, " "))
      print("coucou")
      print(moves)
      # Reset the chess game and apply the moves one by one
      game <- Chess$new()
      for (move in moves) {
        game$move(move)
      }
      
      # Update the move sequence and chess game
      move_sequence(new_sequence)
      chess_game(game)
    }
  })
  
  output$move_stats_table <- renderDT({
    req(move_stats())
    
    # Add dependency on position_evals()
    position_evals()
    
    df <- move_stats()
    
    # Add evaluation column
    current_sequence <- move_sequence()
    df <- df %>%
      mutate(
        Eval = sapply(Move, function(move) {
          moves <- if (current_sequence == "") move else paste(current_sequence, move)
          print(paste(moves))  # Debugging: Print the combined move sequence
          eval <- position_evals()[[trimws(moves)]]
          if (is.null(eval)) return("")  # Handle NULL case explicitly
          format_eval(eval)
        })
      )
    
    # Debugging: Print the Eval column
    print("Eval column:")
    print(df$Eval)
    
    # Debugging: Print the table data
    print("Table data:")
    print(df)
    
    # Render the table without the EvalButton column
    datatable(
      df %>% select(Move, games, rating, Bar, Eval),
      escape = FALSE,
      class = 'cell-border stripe',
      options = list(
        pageLength = 10,
        autoWidth = FALSE,
        columnDefs = list(
          list(targets = 0, width = '30px', className = 'moves-column dt-center'),
          list(targets = 1, width = '30px', className = 'games-column dt-center'),
          list(targets = 2, width = '30px', className = 'games-column dt-center'),
          list(targets = 3, width = '200px', className = 'results-column'),
          list(targets = 4, width = '80px', className = 'evaluation-column dt-center')
        ),
        dom = 'frtip'
      ),
      rownames = FALSE,
      colnames = c("Move", "Games", "Rating", "Results", "Eval")
    )
  })
}
