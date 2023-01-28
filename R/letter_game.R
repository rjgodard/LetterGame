letter_game <- function() {
  message(sprintf("\014Welcome to the letter game!
Press enter when you are ready to start.
Enter HINT if you want a hint. 
Enter DONE when you are finished guessing."))
  
  # GAME SET-UP
  
  # Selects a game set of 7 letters from the possible sets
  game <- sample(1:length(unique7), 1)
  game <- unique7[[game]]
  # Randomly selects one of the 7 letters to be the centre
  centre <- sample(1:7, 1)
  centre <- game[centre]
  
  # Identifies solutions
  excl <- setdiff(letters, game) # non-game letters
  solutions <- grep(centre, words4) # words containing centre
  for (i in 1:length(excl)) { # iteratively removes words containing excluded letters
    excl_solutions <- grep(excl[i], words4[solutions])
    solutions <- solutions[-c(excl_solutions)]
  }
  solutions <- data.frame(
    words = words4[solutions],
    points = NA
  )
  # finds pangrams containing all 7 game letters
  pangrams <- which(grepl(game[1], solutions$words) & 
                      grepl(game[2], solutions$words) & 
                      grepl(game[3], solutions$words) &
                      grepl(game[4], solutions$words) & 
                      grepl(game[5], solutions$words) & 
                      grepl(game[6], solutions$words) & 
                      grepl(game[7], solutions$words))
  
  # counts points
  solutions$points <- sapply(solutions$words, FUN = nchar)
  solutions$points[solutions$points == 4] <- 1 # 4-letter words are 1 point
  solutions$points[pangrams] <- solutions$points[pangrams] + 7 # pangrams are extra
  
  # Constructs hints data frames
  hints <- data.frame(matrix(0, nrow = 7, ncol = 12),
                      row.names = game)
  colnames(hints) <- 4:15
  for (i in 1:nrow(solutions)) {
    row <- strsplit(solutions$words[i], "")[[1]][1] # starting letter
    col <- as.character(nchar(solutions$words[i])) # number of letters
    hints[row, col] <- hints[row, col] + 1
  }
  hints <- hints[, colSums(hints) > 0]
  
  hints2_vec <- unname(sort(sapply(solutions$words, # beginning 2-letter combos
                                   FUN = substr, 
                                   start = 1, 
                                   stop = 2)))
  hints2 <- data.frame(
    letters = unique(hints2_vec),
    count = NA
  )
  for (i in 1:nrow(hints2)) {
    hints2$count[i] <- sum(grepl(hints2$letters[i], hints2_vec))
  }
  points_tally <- 0
  
  message(sprintf("The 7 letters are: %s, %s, %s, %s, %s, %s, and %s", 
                  game[1], game[2], game[3], game[4], game[5],
                  game[6], game[7]))
  
  message(sprintf("All words must include: %s", centre))
  
  message(sprintf("Total possible words: %s", nrow(solutions)))
  
  message(sprintf("Total possible points: %s", sum(solutions$points)))
  
  message(sprintf("Pangrams: %s (%s perfect)",
                  length(pangrams), 
                  sum(solutions$points[pangrams] == 14)))
  
  
  
  response <- "hi"
  while (response != "DONE") {
    response <- readline(sprintf("Enter your next word or HINT for a hint. " ))
    if (response != "HINT") {
      if (response %in% solutions$words) {
        points_plus <- solutions$points[solutions$words == response]
        message(sprintf("+%s!", points_plus))
        points_tally <- points_tally + points_plus
        message(sprintf("You have %s points", points_tally))
      } else {
        if (response != "DONE") message(sprintf("Not a solution"))
      }
    } else {
      message(sprintf("Number of words by starting letter and length: "))
      print(hints)
      message(sprintf("Number of words by starting two letters: "))
      print(hints2)
    }
    
  }
  
  
  message(sprintf("Thank you for playing! Your total score is %s.", 
                  points_tally))
  
  
}
