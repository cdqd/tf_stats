library(shiny)
library(rvest)
library(magrittr)

# UI ----------------
ui <- fluidPage(
   
   # Application title
   titlePanel("TF 1v1 game-by-game stats"),
    
   sidebarLayout(
      sidebarPanel(
         p("Enter players' names and load initial stats at the start of the match only. If only logging one player, fill in Player 1 name and make sure Player 2 name is empty."),
         p("After each round press the 'Get stats for most recent game' button. This updates the individual game stats as well as the summary stats. Tables and plots at the bottom are interactive."),
         p(strong("IMPORTANT:"), "If games are less than 30 seconds, enter a time manually before getting most recent stats. This is to fix a TF bug where game times <30 seconds are not logged."),
         p("Even if gametime is between 30-35 seconds, enter 29 anyway, in case one/both players had shorter times logged. Our stats data will only use the manual input if the Tetris Friends server has not logged the time, so doing this will not affect the accuracy of the stats.")
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
                            c("SPM", "TPM", "T-spins/min", 
                              "Tetrises/min", "Total Combos/min",
                              "B2Bs/min", "Singles/min",
                              "Doubles/min", "Triples/min"),
                            selected = c("SPM", "TPM"),
                            inline = T),
         tableOutput("summary_output"),
         p(strong("Efficiency stats:")),
         tableOutput("efc_summary"),
         p(strong("Per round averages:")),
         tableOutput("supp_summary"),
         br(),
         h3("Select stat to plot"),
         br(),
         p("Select one stat and which rounds to plot. The triangles in the plot represent won rounds."),
         selectInput("plot_stat", "Select stat to plot:",
                     c("SPM", "TPM", "SPM/TPM ratio", 
                       "T-spins/min", "Tetrises/min", "Total Combos/min",
                       "B2Bs/min", "T-usage efficiency", "Singles/min",
                       "Doubles/min", "Triples/min"),
                     selected = c("SPM")),
         sliderInput("plot_rounds", "Select rounds to plot:",
                     min = 1, max = 50, value = c(1, 50)),
         plotOutput("summary_plot")
      )
   )
)

# Server ----------------
server <- function(input, output, session) {
  # Functions -------------
  getTFstats <- function() {
    webpage <- list()
    scrape <- list()
    nums <- list()
    wins <- list()
    
    for (k in 1:values$n_players) {
      webpage[[k]] <- read_html(values$url[k])
      
      temp <- character(0)
      for (i in 4:20) {
        temp[i - 3] <-
          html_nodes(webpage[[k]],
                     paste0("table:nth-child(", i, ")")) %>% 
          html_text()  %>% 
          .[length(.)]
      }
      scrape[[k]] <- temp
      
      wins[[k]] <-
        html_nodes(webpage[[k]],
                   paste0("table:nth-child(", 3, ")")) %>%
        html_text() %>% .[2] %>% 
        regmatches(gregexpr(pattern = "[0-9]+ Gold", text = .)) %>% unlist() %>% 
        regmatches(gregexpr(pattern = "[0-9]+", text = .)) %>% unlist() %>% as.numeric()
      
      if (k == 1) {
        p <- regexpr("[a-z]:|[A-Z]:| :|\\):", scrape[[k]])
        stat_name <- c(substr(scrape[[k]], 1, p), "GOLD")
      }
      
      nums[[k]] <- c(substring(scrape[[k]], p + 2), wins[[k]])
    }
    
    snapshot <- 
      data.frame(V1 = stat_name,
                 V2 = nums[[1]],
                 stringsAsFactors = F)
    
    if (values$n_players > 1) {
      snapshot[, 3] <- nums[[2]]
      }
    
    a <- strsplit(as.character(snapshot[6, -1]), ":")
    times <- numeric(0)
    for(i in 1:values$n_players) {
      a[[i]] <- as.numeric(a[[i]])
      times[i] <- a[[i]][3] + a[[i]][2] * 60 + a[[i]][1]* (60^2)
    }
    
    snapshot[-1] <- suppressWarnings(lapply(snapshot[-1], as.numeric))
    snapshot[6, -1] <- times
    
    return(snapshot)
  }
  
  calc_stats <- function() {
    dfs <- list()
    
    for(k in 1:values$n_players) {
      dfs[[k]] <-
        data.frame(Player = values$pn[k],
                   `Time in seconds` = values$diff[6, k],
                   SPM = values$diff[17, k] / values$diff[6, k] * 60,
                   TPM = values$diff[7, k] / values$diff[6, k] * 60,
                   `T-spins/min` = values$diff[14, k] / values$diff[6, k] * 60,
                   `Tetrises/min` = values$diff[11, k] / values$diff[6, k] * 60,
                   `Total Combos/min` = values$diff[12, k] / values$diff[6, k] * 60,
                   `B2Bs/min` = values$diff[15, k] / values$diff[6, k] * 60,
                   `Singles/min` = values$diff[8, k] / values$diff[6, k] * 60,
                   `Doubles/min` = values$diff[9, k] / values$diff[6, k] * 60,
                   `Triples/min` = values$diff[10, k] / values$diff[6, k] * 60,
                   `SPM/TPM ratio` = values$diff[17, k] / values$diff[7, k],
                   `T-usage efficiency` = values$diff[14, k] / (values$diff[7, k] / 7),
                   `Winner` = values$diff[18, k], 
                   check.names = F)
    }
    
    return(do.call(rbind, dfs))
  }
  
  add_legend <- function(...) {
    opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
                mar=c(0, 0, 0, 0), new=TRUE)
    on.exit(par(opar))
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend(...)
  }
  
  # Core server code ----------------
  values <- reactiveValues(t1 = data.frame(NULL),
                           ld = data.frame(NULL),
                           fd = data.frame(NULL))
  
  observeEvent(input$initial, {
    # initialize all values
    values$pn <- c(input$name_1, input$name_2)
    values$n_players <- sum(values$pn != "")
    
    values$t1 <- data.frame(NULL)
    values$ld <- data.frame(NULL)
    values$fd <- data.frame(NULL)
    
    values$url <- paste0("http://harddrop.com/file/tfstats.php?username=", values$pn)
    
    values$t0 <- getTFstats()
    
    output$load_msg <- renderText({
      paste0("Initial stats for ", values$pn[1], 
             if (values$pn[2] != "") paste0(" and ", values$pn[2]), 
             " loaded.")
    })
  })
  
  observeEvent(input$update, {
    values$t1 <- getTFstats()
    values$diff <- values$t1[-1] - values$t0[-1]
    if (any(values$diff[6, ] == 0)) {
      values$diff[6, ] <- 
        values$diff[6, ] + as.numeric(values$diff[6, ] == 0) * input$manual_time
    }
    
    values$ld <- calc_stats()
    
    fd_0 <- rbind(values$fd, 
                  cbind(values$ld, 
                        data.frame(game = 
                          if (values$n_players == 1) {nrow(values$fd) + 1
                          } else {
                            rep((nrow(values$fd) / 2 + 1), each = 2)}
                          )))
    values$fd <- fd_0
    
    # Prepare formatting for most recent game comparison table --
    
    values$last <- as.data.frame(t(values$ld), stringsAsFactors = F)
    values$last$Player <- row.names(values$last)
    names(values$last) <- (values$last[1, ])
    values$last <- values$last[-1, c(length(values$last), 1:values$n_players)]
    # round all numbers to 2 dp
    values$last[-1] <- lapply(values$last[-1], function(x) {round(as.numeric(x), 2)})
     
    values$t0 <- values$t1
    updateNumericInput(session, "manual_time", value = 0)
  })
  
  output$stats_output <- renderTable({
    if (nrow(values$ld) == 0) {
      return(NULL)
    } else return(values$last)
  }, align = "c")
  
  output$summary_output <- renderTable({
    if (nrow(values$fd) == 0) {
      return(NULL)
      } else {
      # Create min, max, mean stats as separate tables --
       mins <- aggregate(values$fd[, input$summary_vars], 
                         list(Player = values$fd$Player), 
                         min, na.rm = T)
       names(mins)[-1] <- 
         if (length(input$summary_vars) == 1) {
           paste0("Min ", input$summary_vars) 
           } else paste0("Min ", names(mins)[-1])
       
       # Design decision: The main summary table will show the time-weighted means
       tweight <- numeric(nrow(values$fd))
       for (i in 1:values$n_players) {
         tweight[values$fd$Player == values$pn[i]] <-
           sum(values$fd$Time[values$fd$Player == values$pn[i]], na.rm = T)
       }
       means <- aggregate(values$fd[, input$summary_vars] * 
                            (values$fd$`Time in seconds` / tweight), 
                          list(Player = values$fd$Player), 
                          sum, na.rm = T)
       names(means)[-1] <- 
         if (length(input$summary_vars) == 1) {
           paste0("Mean ", input$summary_vars)
           } else paste0("Mean ", names(means)[-1])
       
       maxs <- aggregate(values$fd[, input$summary_vars], 
                         list(Player = values$fd$Player), 
                         max, na.rm = T)
       names(maxs)[-1] <- 
         if (length(input$summary_vars) == 1) {
           paste0("Max ", input$summary_vars)
           } else paste0("Max ", names(maxs)[-1])
       
       wintot <- aggregate(values$fd[, "Winner"], list(Player = values$fd$Player), sum, na.rm = T)
       names(wintot)[-1] <- "Wins"
       
       # Merge min, max, mean tables for final display --
       summ <- merge(merge(mins, means), maxs)
       summ <- merge(summ, wintot)
       summ <- summ[, c(1, length(summ), 2:(length(summ)-1))]
       
       # Return table required order; Name of Player and Wins first, then stats
       return(
       summ[, c(1:2, 
                2 + order(substring(names(summ)[-(1:2)],
                                    1 + regexpr(" ", names(summ)[-(1:2)]))))]
       )
       }
    }, align = "c")
  
  # Summary stats for efficiency shown separately as a different denominator is used
  output$efc_summary <- renderTable({
    if (nrow(values$fd) == 0) {
      return(NULL)
    } else {
      efc <- list() 
      for(k in 1:values$n_players) {
        tmp <- values$fd[values$fd$Player == values$pn[k], ]
        efc[[k]] <-
          data.frame(Player = values$pn[k],
                     `Min SPM/TPM` = 
                       min(tmp$`SPM/TPM ratio`),
                     `Mean SPM/TPM` =
                       sum(tmp$`SPM` * (tmp$`Time in seconds` / 60)) / 
                       sum(tmp$`TPM` * (tmp$`Time in seconds` / 60)),
                     `Max SPM/TPM` = 
                       max(tmp$`SPM/TPM ratio`),
                     `Min T-usage` = 
                       min(tmp$`T-usage efficiency`),
                     `Mean T-usage` =
                       sum(tmp$`T-spins/min` * (tmp$`Time in seconds` / 60)) / 
                       (sum(tmp$`TPM` * (tmp$`Time in seconds` / 60)) / 7),
                     `Max T-usage` = 
                       max(tmp$`T-usage efficiency`),
                     check.names = F)
      }
      return(do.call(rbind, efc))
    }
  }, align = "c")
  
  output$supp_summary <- renderTable({
    if (nrow(values$fd) == 0) {
      return(NULL)
      } else {
        # Design decision: Un-timeweighted stats will be shown for APM and TPM as well
        supp <- list() 
        for(k in 1:values$n_players) {
          supp[[k]] <-
            data.frame(Player = values$pn[k],
                       `Mean SPM per round` = 
                         mean(values$fd$SPM[values$fd$Player == values$pn[k]]),
                       `Mean TPM per round` = 
                         mean(values$fd$TPM[values$fd$Player == values$pn[k]]),
                       check.names = F)
        }
        return(do.call(rbind, supp))
      }
  }, align = "c")
  
  output$games <- renderText(
    {paste0("Rounds played so far: ", max(values$fd$game, 0))}
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(as.Date(Sys.time()), "_", values$pn[1], 
             if (values$pn[2] != "") paste0("_", values$pn[2]), 
             ".csv")
    },
    content = function(file) {
      write.csv(values$fd, file, row.names = FALSE)
    }
  )
  
  output$summary_plot <- renderPlot({
    if (nrow(values$fd) == 0) {
      return(NULL)
      } else{
          min_round <- min(input$plot_rounds[1], max(values$fd$game))
          max_round <- min(input$plot_rounds[2], max(values$fd$game))
          
          testplot <- values$fd[values$fd$game >= min_round & values$fd$game <= max_round, ]
          
          plot(1
               , type = "n", main = paste0(input$plot_stat)
               , xlab = "Round"
               , ylab = paste0(input$plot_stat)
               , xlim = c(min(testplot$game), 
                          max(testplot$game))
               , ylim = c(min(testplot[, input$plot_stat]), max(testplot[, input$plot_stat]))
          )
          lines(x = testplot$game[testplot$Player == values$pn[1]], y = testplot[, input$plot_stat][testplot$Player == values$pn[1]], col = "red")
          points(x = testplot$game[testplot$Player == values$pn[1] & testplot$Winner == 1],
                 y = testplot[, input$plot_stat][testplot$Player == values$pn[1] & testplot$Winner == 1], pch = 24, col = "red", bg = "red", cex = 1.2)
          
          if (values$pn[2] != "") {
          lines(x = testplot$game[testplot$Player == values$pn[2]], y = testplot[, input$plot_stat][testplot$Player == values$pn[2]], col = "blue")
          points(x = testplot$game[testplot$Player == values$pn[2] & testplot$Winner == 1],
                 y = testplot[, input$plot_stat][testplot$Player == values$pn[2] & testplot$Winner == 1], pch = 24, col = "blue", bg = "blue", cex = 1.2)
          add_legend("topright", bty = "n", 
                     legend = c(values$pn[1], values$pn[2]), lty = c(1, 1), col = c("red", "blue"))
          }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)