library(shiny)
library(rvest)
library(magrittr)

# UI ----------------
ui <- fluidPage(
   
   # Application title
   titlePanel("TF 1v1 game-by-game stats"),
    
   sidebarLayout(
      sidebarPanel(
         p("Enter players' names and load initial stats at the start of the match only."),
         p("After each round press the 'Get stats for most recent game' button. This updates the individual game stats as well as the summary stats. Tables and plots at the bottom are interactive."),
         br(),
         p("IMPORTANT: If games are less than 30 seconds, enter a time manually before getting most recent stats. This is to fix a TF bug where game times <30 seconds are not logged."),
         p("Even if gametime is between 30-35 seconds, enter 29 anyway, in case one/both players had shorter times logged (our stats data will only use the manual input if the Tetris Friends server has not logged the time, so doing this will not affect the accuracy of the stats).")
      ),
      mainPanel(
         h3("Player names"),
         textInput("name_1", NULL, placeholder = "Player 1 name"),
         textInput("name_2", NULL, placeholder = "Player 2 name"),
         actionButton("initial", label = "Load initial stats & start log"),
         textOutput("load_msg"),
         br(),
         h3("Stats for most recent game"),
         numericInput("manual_time", "Manually add time for short games", 0,
                      min = 0, max = 29),
         actionButton("update", label = "Get stats for most recent game"), 
         br(),
         tableOutput("stats_output"),
         br(),
         h3("Summary stats for match"),
         downloadButton("downloadData", label = "Download full match data"),
         br(),
         textOutput("games"),
         br(),
         checkboxGroupInput("summary_vars", "Stats to show:",
                            c("SPM", "TPM", "SPM/TPM ratio", 
                              "T-spins", "Tetrises", "Total Combos",
                              "B2Bs", "T-usage efficiency", "Singles",
                              "Doubles", "Triples"),
                            selected = c("SPM", "TPM"),
                            inline = T),
         tableOutput("summary_output"),
         br(),
         h3("Select stat to plot"),
         br(),
         p("Select one stat and which rounds to plot. The triangles in the plot represent won rounds."),
         selectInput("plot_stat", "Select stat to plot:",
                     c("SPM", "TPM", "SPM/TPM ratio", 
                       "T-spins", "Tetrises", "Total Combos",
                       "B2Bs", "T-usage efficiency", "Singles",
                       "Doubles", "Triples"),
                     selected = c("SPM")),
         sliderInput("plot_rounds", "Select rounds to plot:",
                     min = 1, max = 50, value = c(1, 50)),
         plotOutput("summary_plot")
      )
   )
)

# Server ----------------
server <- function(input, output, session) {
  
  getTFstats <- function(){
    webpage <- read_html(values$url)
    webpage1 <- read_html(values$url1)
    scrape <- data.frame(1)
    scrape1 <- data.frame(1)
    for (i in 4:20) {
      scrape[i - 3, ] <-
        html_nodes(webpage,
                   paste0("table:nth-child(", i, ")")) %>% 
        html_text()  %>% 
        .[length(.)]
      
      scrape1[i - 3, ] <-
        html_nodes(webpage1,
                   paste0("table:nth-child(", i, ")")) %>% 
        html_text()  %>% 
        .[length(.)]
    }
    
    p <- regexpr("[a-z]:|[A-Z]:| :|\\):", scrape[, 1])
    
    names <- substr(scrape[, 1], 1, p)
    nums <- substring(scrape[, 1], p + 2)
    nums1 <- substring(scrape1[, 1], p + 2)
    
    wins <-
      html_nodes(webpage,
                 paste0("table:nth-child(", 3, ")")) %>%
      html_text() %>% .[2] %>% 
      regmatches(gregexpr(pattern = "[0-9]+ Gold", text = .)) %>% unlist() %>% 
      regmatches(gregexpr(pattern = "[0-9]+", text = .)) %>% unlist() %>% as.numeric()
    
    wins1 <-
      html_nodes(webpage1,
                 paste0("table:nth-child(", 3, ")")) %>%
      html_text() %>% .[2] %>% 
      regmatches(gregexpr(pattern = "[0-9]+ Gold", text = .)) %>% unlist() %>% 
      regmatches(gregexpr(pattern = "[0-9]+", text = .)) %>% unlist() %>% as.numeric()
    
    for(i in 1:17){
      scrape[i, 1] <- names[i]
      scrape[i, 2] <- nums[i]
      scrape[i, 3] <- nums1[i]
    }
    
    a <- strsplit(as.character(scrape[6, 2:3]), ":")
    times <- numeric(0)
    for(i in 1:2){
      a[[i]] <- as.numeric(a[[i]])
      times[i] <- a[[i]][3] + a[[i]][2] * 60 + a[[i]][1]* (60^2)
    }
    
    scrape[18, ] <- c("GOLD", wins, wins1)
    
    for (i in 2:3){
      scrape[, i] <- suppressWarnings(as.numeric(scrape[, i]))
      scrape[6, i] <- times[i - 1]   
    }
    
    return(scrape)
  }
  
  calc_stats <- function(){
    df <- data.frame(Player = c(values$p1, values$p2),
                     `Time in seconds` = c(values$diff[6, 1], values$diff[6, 2]),
                     SPM = c(values$diff[17, 1] / values$diff[6, 1], values$diff[17, 2] / values$diff[6, 2]) * 60,
                     TPM = c(values$diff[7, 1] / values$diff[6, 1], values$diff[7, 2] / values$diff[6, 2]) * 60,
                     `SPM/TPM ratio` = c(values$diff[17, 1] / values$diff[7, 1], values$diff[17, 2] / values$diff[7, 2]),
                     `T-spins` = c(values$diff[14, 1], values$diff[14, 2]),
                     Tetrises = c(values$diff[11, 1], values$diff[11, 2]),
                     `Total Combos` = c(values$diff[12, 1], values$diff[12, 2]),
                     B2Bs = c(values$diff[15, 1], values$diff[15, 2]),
                     `T-usage efficiency` = c(values$diff[14, 1] / (values$diff[7, 1] / 7), values$diff[14, 2] / (values$diff[7, 2] / 7)),
                     `Singles` = c(values$diff[8, 1], values$diff[8, 2]),
                     `Doubles` = c(values$diff[9, 1], values$diff[9, 2]),
                     `Triples` = c(values$diff[10, 1], values$diff[10, 2]),
                     `Winner` = c(values$diff[18, 1], values$diff[18, 2]),
                     check.names = F)
    return(df)
  }
  
  add_legend <- function(...) {
    opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
                mar=c(0, 0, 0, 0), new=TRUE)
    on.exit(par(opar))
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend(...)
  }
  
  values <- reactiveValues(t1 = data.frame(NULL),
                           ld = data.frame(NULL),
                           fd = data.frame(NULL))
  
  observeEvent(input$initial, {
    # initialize all values
    values$t1 <- data.frame(NULL)
    values$ld <- data.frame(NULL)
    values$fd <- data.frame(NULL)
    
    values$p1 <- input$name_1
    values$p2 <- input$name_2
    
    values$url <- 
      paste0("http://harddrop.com/file/tfstats.php?username=", input$name_1)
    values$url1 <- 
      paste0("http://harddrop.com/file/tfstats.php?username=", input$name_2)
    
    values$t0 <- getTFstats()
    
    output$load_msg <- renderText({
      paste0("Initial stats for ", values$p1, " and ", values$p2, " loaded.")
    })
  })
  
  observeEvent(input$update, {
    values$t1 <- getTFstats()
    values$diff <- values$t1[, -1] - values$t0[, -1]
    if(any(values$diff[6, ] == 0)){
      values$diff[6, ] <- 
        values$diff[6, ] + as.numeric(values$diff[6, ] == 0) * input$manual_time
    }
    
    values$ld <- calc_stats()
    
    fd_0 <- rbind(values$fd, 
                  cbind(values$ld, data.frame(game = c(1, 1))))
    fd_0$game <- rep(1:(nrow(fd_0)/2), each = 2)
    values$fd <- fd_0
    
    values$last <- as.data.frame(t(values$ld), stringsAsFactors = F)
    values$last$Player <- row.names(values$last)
    names(values$last) <- (values$last[1, ])
    values$last <- values$last[-1, c(3, 1, 2)]
    
    output$stats_output <- renderTable(
      {values$last}
    )
    
    values$t0 <- values$t1
    updateNumericInput(session, "manual_time", value = 0)
  })

  output$summary_output <- renderTable({
    # if(values$on == 0) data.frame(NULL)
     # else if(nrow(values$fd) == 0) data.frame(NULL)
    if(nrow(values$fd) == 0) return(NULL)
     else {
       mins <- aggregate(values$fd[, input$summary_vars], list(Player = values$fd$Player), min, na.rm = T)
       names(mins)[-1] <- 
         if(length(input$summary_vars) == 1) paste0("Min ", input$summary_vars) 
         else paste0("Min ", names(mins)[-1])
       
       means <- aggregate(values$fd[, input$summary_vars], list(Player = values$fd$Player), mean, na.rm = T)
       names(means)[-1] <- 
         if(length(input$summary_vars) == 1) paste0("Mean ", input$summary_vars) 
         else paste0("Mean ", names(means)[-1])
       
       maxs <- aggregate(values$fd[, input$summary_vars], list(Player = values$fd$Player), max, na.rm = T)
       names(maxs)[-1] <- 
         if(length(input$summary_vars) == 1) paste0("Max ", input$summary_vars)
         else paste0("Max ", names(maxs)[-1])
       
       wintot <- aggregate(values$fd[, "Winner"], list(Player = values$fd$Player), sum, na.rm = T)
       names(wintot)[-1] <- "Wins"
       
       summ <- merge(merge(mins, means), maxs)
       summ <- merge(summ, wintot)
       summ <- summ[, c(1, length(summ), 2:(length(summ)-1))]
       return(
       summ[, c(1:2, 
                2 + order(substring(names(summ)[-(1:2)],
                                    1 + regexpr(" ", names(summ)[-(1:2)]))))]
       )
       }
    }
  )
  
  output$games <- renderText(
    {paste0("Rounds played so far: ", max(values$fd$game, 0))}
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(as.Date(Sys.time()), "_", values$p1, "_", values$p2, ".csv")
    },
    content = function(file) {
      write.csv(values$fd, file, row.names = FALSE)
    }
  )
  
  output$summary_plot <- renderPlot({
    if(nrow(values$fd) == 0) return(NULL)
    else{
    min_round <- min(input$plot_rounds[1], max(values$fd$game))
    max_round <- min(input$plot_rounds[2], max(values$fd$game))
    
    testplot <- values$fd[values$fd$game >= min_round & values$fd$game <= max_round, ]
    
    plot(1
         , type = "n", main = paste0(input$plot_stat)
         , xlab = "Round"
         , ylab = paste0(input$plot_stat)
         , xlim = c(min(testplot$game), 
                    max(testplot$game))
         # , xaxt = "n"
         , ylim = c(min(testplot[, input$plot_stat]), max(testplot[, input$plot_stat]))
    )
    lines(x = testplot$game[testplot$Player == values$p1], y = testplot[, input$plot_stat][testplot$Player == values$p1], col = "red")
    lines(x = testplot$game[testplot$Player == values$p2], y = testplot[, input$plot_stat][testplot$Player == values$p2], col = "blue")
    points(x = testplot$game[testplot$Player == values$p1 & testplot$Winner == 1],
           y = testplot[, input$plot_stat][testplot$Player == values$p1 & testplot$Winner == 1], pch = 24, col = "red", bg = "red", cex = 1.2)
    points(x = testplot$game[testplot$Player == values$p2 & testplot$Winner == 1],
           y = testplot[, input$plot_stat][testplot$Player == values$p2 & testplot$Winner == 1], pch = 24, col = "blue", bg = "blue", cex = 1.2)
    add_legend("topright", bty = "n", legend = c(values$p1, values$p2), lty = c(1, 1), col = c("red", "blue"))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)