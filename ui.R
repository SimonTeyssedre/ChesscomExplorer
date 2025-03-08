ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  div(class = "title-panel",
      h1("Chess.com Move Explorer", style = "margin: 0; font-weight: 300;"),
      p("Analyze your chess game patterns and statistics", style = "margin: 5px 0 0 0; opacity: 0.8;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      wellPanel(
        textInput("username", "Chess.com Username", value = "", 
                  placeholder = "Enter your username"),
        actionButton("go", "Download Games", 
                     icon = icon("download"), 
                     class = "btn-primary btn-block"),
        hr(),
        dateRangeInput("date_filter", "Date Range", 
                       start = Sys.Date() - 360,
                       end = Sys.Date()),
        selectInput("color_filter", "Color", 
                    choices = c("White", "Black")),
        prettyCheckboxGroup(
          inputId = "game_type_filter",
          label = "Game Type",
          choices = c("Blitz", "Bullet", "Rapid"),
          selected = "Blitz",
          status = "primary",
          animation = "pulse",
          icon = icon("chess"),
          inline = TRUE
        ),
        hr(),
        h4("Current Move Sequence", style = "color: #2c3e50;"),
        verbatimTextOutput("move_sequence"),
        actionButton("reset", "Reset Moves", 
                     icon = icon("undo"), 
                     class = "btn-primary btn-block",
                     style = "margin-top: 15px;"),
        actionButton("back", "Back One Move",  # Nouveau bouton Back
                     icon = icon("arrow-left"), 
                     class = "btn-info btn-block",
                     style = "margin-top: 10px;")
      )
    ),
    
    mainPanel(
      width = 9,
      h3("Move Statistics", style = "color: #2c3e50; margin-bottom: 20px;"),
      fluidRow(
        
        column(6,        rchess::chessboardjsOutput("chessboard", width = "500px", height = "500px")),
        column(6,
               div(style = "margin-bottom: 15px;",
                   actionButton("evaluate_all", "Evaluate All Positions", 
                                class = "btn-primary",
                                style = "margin-right: 10px;"),
                   span(id = "eval_status", "")
               ),
               withSpinner(DTOutput("move_stats_table"))
        )
      )
      # Chessboard placeholder
      
    )
  )
)