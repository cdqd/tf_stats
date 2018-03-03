# library(shiny)
library(rvest)
library(magrittr)

p1 <- "crzy242"
p2 <- "OnePunMan"

url <- "http://harddrop.com/file/tfstats.php?username=crzy242"
url1 <- "http://harddrop.com/file/tfstats.php?username=OnePunMan"

getTFstats <- function(){
  webpage <- read_html(url)
  webpage1 <- read_html(url1)
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
  df <- data.frame(Player = c(p1, p2),
                   `Time in seconds` = c(diff[6, 1], diff[6, 2]),
                   SPM = c(diff[17, 1] / diff[6, 1], diff[17, 2] / diff[6, 2]) * 60,
                   TPM = c(diff[7, 1] / diff[6, 1], diff[7, 2] / diff[6, 2]) * 60,
                   `SPM/TPM ratio` = c(diff[17, 1] / diff[7, 1], diff[17, 2] / diff[7, 2]),
                   `T-spins` = c(diff[14, 1], diff[14, 2]),
                   Tetrises = c(diff[11, 1], diff[11, 2]),
                   `Total Combos` = c(diff[12, 1], diff[12, 2]),
                   B2Bs = c(diff[15, 1], diff[15, 2]),
                   `T-usage efficiency` = c(diff[14, 1] / (diff[7, 1] / 7), diff[14, 2] / (diff[7, 2] / 7)),
                   `Singles` = c(diff[8, 1], diff[8, 2]),
                   `Doubles` = c(diff[9, 1], diff[9, 2]),
                   `Triples` = c(diff[10, 1], diff[10, 2]),
                   `Winner` = c(diff[18, 1], diff[18, 2]),
                   check.names = F)
  return(df)
}

# Server ----------------
p1 <- "crzy242"
p2 <- "OnePunMan"

t1 <- data.frame(NULL)
last <- data.frame(NULL)
fd <- data.frame(NULL)

url <- 
  paste0("https://harddrop.com/file/tfcompare.php?username1=",
         p1,
         "&username2=",
         p2)

t0 <- getTFstats()

load_msg <-
  paste0("Initial stats for ", p1, " and ", p2, " loaded.")

t1 <- getTFstats()
diff <- t1[, -1] - t0[, -1]

add_time <- 30
if(any(diff[6, ] == 0)){
diff[6, ] <- diff[6, ] + as.numeric(diff[6, ] == 0) * add_time
}

last <- as.data.frame(t(calc_stats()), stringsAsFactors = F)
last$Player <- row.names(last)
names(last) <- (last[1, ])
last <- last[-1, c(3, 1, 2)]

ld <- calc_stats()
g <- data.frame(game = c(1, 1))
fd <- rbind(fd, cbind(ld, g))
fd$game <- rep(1:(nrow(fd)/2), each = 2)

mins <- aggregate(fd[, summary_vars], list(Player = fd$Player), min, na.rm = T)
names(mins)[-1] <- paste0("Min ", names(mins)[-1])

means <- aggregate(fd[, summary_vars], list(Player = fd$Player), mean, na.rm = T)
names(means)[-1] <- paste0("Mean ", names(means)[-1])

maxs <- aggregate(fd[, summary_vars], list(Player = fd$Player), max, na.rm = T)
names(maxs)[-1] <- paste0("Max ", names(maxs)[-1])

summ <- merge(merge(mins, means), maxs)

summary_output <- 
  summ[, c(1, 1 + order(substring(names(summ[-1]), 
                                   1 + regexpr(" ", names(summ[-1])))))]

t0 <- t1

games <- 
  {paste0("Rounds played so far: ", max(fd$game, 0))}


file <-
paste0(as.Date(Sys.time()), "_full_data_", p1, "_", p2, ".csv")

write.csv(fd, file, row.names = FALSE)

fd <- read.csv("test2.csv", check.names = F)[-(17:18), ]
summary_vars <- c("TPM", "SPM")

mins <- aggregate(fd[, summary_vars], list(Player = fd$Player), min, na.rm = T)
names(mins)[-1] <- paste0("Min ", names(mins)[-1])

means <- aggregate(fd[, summary_vars], list(Player = fd$Player), mean, na.rm = T)
names(means)[-1] <- paste0("Mean ", names(means)[-1])

maxs <- aggregate(fd[, summary_vars], list(Player = fd$Player), max, na.rm = T)
names(maxs)[-1] <- paste0("Max ", names(maxs)[-1])

wintot <- aggregate(fd[, "Winner"], list(Player = fd$Player), sum, na.rm = T)
names(wintot)[-1] <- "Wins"

summ <- merge(merge(mins, means), maxs)
summ <- merge(summ, wintot)
summ <- summ[, c(1, length(summ), 2:(length(summ)-1))]

2 + order(substring(names(summ)[-(1:2)],
                1 + regexpr(" ", names(summ)[-(1:2)])))

## generate graph --------
test <- read.csv("test2.csv", check.names = F)[-(17:18), ]

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

p1 <- "rakenroll"
p2 <- "COOLJACKS"
plot_var <- "T-spins"
last_n <- 3

last_n_rounds <- min(last_n, max(test$game))

testplot <- test[test$game >= (max(test$game) - last_n_rounds + 1), ]

plot(1
     , type = "n", main = paste0(plot_var)
     , xlab = "Round"
     , ylab = paste0(plot_var)
     , xlim = c(min(testplot$game), 
                max(testplot$game))
     # , xaxt = "n"
     , ylim = c(min(testplot[, plot_var]), max(testplot[, plot_var]))
     )
lines(x = testplot$game[testplot$Player == p1], y = testplot[, plot_var][testplot$Player == p1], col = "red")
lines(x = testplot$game[testplot$Player == p2], y = testplot[, plot_var][testplot$Player == p2], col = "blue")
points(x = testplot$game[testplot$Player == p1 & testplot$Winner == 1],
       y = testplot[, plot_var][testplot$Player == p1 & testplot$Winner == 1], pch = 24, col = "red", bg = "red", cex = 1.2)
points(x = testplot$game[testplot$Player == p2 & testplot$Winner == 1],
       y = testplot[, plot_var][testplot$Player == p2 & testplot$Winner == 1], pch = 24, col = "blue", bg = "blue", cex = 1.2)
add_legend("topright", bty = "n", legend=c(p1, p2), lty = c(1, 1), col = c("red", "blue"))


