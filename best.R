
best <- function (state, outcome) {
  
  #read the data
  df <- data.frame()
  df <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  #check if the inputs are correct
  slist <- unique(df$State)
  olist <- c("heart attack", "heart failure", "pneumonia")
  
  if (isTRUE(match(state, slist) > 0) == FALSE) {
    stop("invalid state")
  }
  else if ((isTRUE(match(outcome, olist) > 0) == FALSE)) {
    stop("invalid outcome")
  }
  
  #find the best hospital in the state for the provided outcome
  bestdf <- df[, c(2, 7, 11, 17, 23)]
  outcome_index <- numeric()
  
  if (outcome == "heart attack") { 
    outcome_index <- 3
  } else if (outcome == "heart failure") {
    outcome_index <- 4
  } else if (outcome == "pneumonia") {
    outcome_index <- 5
  }
  
  finaldf <- bestdf[, c(1, 2, outcome_index)]
  names(finaldf) <- c("Hospital", "State", "Outcome")
  finaldf$Outcome <- suppressWarnings(as.numeric(as.character(finaldf$Outcome)))
  result <- finaldf[finaldf$State == state, ]
  besty <- result[order(result[3]), ]
  besty2 <- besty[complete.cases(besty),]
  return(as.character(besty2[1,1]))
  }
