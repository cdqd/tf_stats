# Log and analyse stats for 1vs1 Tetris Friends Arena matches	

[Link to Tetris Friends game](http://www.tetrisfriends.com/)

Tetris Friends Arena (live multiplayer Tetris) currently updates aggregate player statistics after each game played, but does not report any game-by-game statistics. This app was made to log these additional stats for two players during 1v1 matches (an important source of practice for top-level players during tournament season), to allow players/commentators to analyse and compare key metrics and playstyles.

The app should be started at the beginning of each match. After each round, it can scrape each player's TF profile and calculate differences -> calculate stats. Stats for the most recent round are shown, as well as interactive summary tables and plots for all the rounds logged previously. The user can also download the underlying stats for all rounds as a `.csv`.


#### You will need the following software to run the app:
- R version 3.4.3+
- The following packages installed for R:
  - `shiny`
  - `rvest`
  - `magrittr`
  - You can install all three by entering `install.packages("shiny", "rvest", "magrittr")` in the R console.

#### To start the app, enter this into your R console:

`shiny::runGitHub("tf_stats", "cdqd", launch.browser = T)`

Instructions for use are provided in the app. Please feel free to report any bugs.
