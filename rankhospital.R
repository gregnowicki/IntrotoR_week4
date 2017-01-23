
rankhospital <- function(state, outcome, num = "best") {
  #read the data in
  df <- data.frame()
  df <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)  
  
  #check that the inputs for state & outcome are valid, otherwise stop the function
  slist <- unique(df$State)
  olist <- c("heart attack", "heart failure", "pneumonia")
  
  if (isTRUE(match(state, slist) > 0) == FALSE) {
    stop("invalid state")
  }
  else if ((isTRUE(match(outcome, olist) > 0) == FALSE)) {
    stop("invalid outcome")
  }
  
  #
  df2 <- df[, c(2, 7, 11, 17, 23)]
  outcome_index <- numeric()
  
  if (outcome == "heart attack") { 
    outcome_index <- 3
  } else if (outcome == "heart failure") {
    outcome_index <- 4
  } else if (outcome == "pneumonia") {
    outcome_index <- 5
  }
  
  #create a dataframe to get the hospital, state & rank as defined by the inputs
  df3 <- df2[, c(1, 2, outcome_index)]
  names(df3) <- c("Hospital", "State", "Outcome")
  df3$Outcome <- suppressWarnings(as.numeric(as.character(df3$Outcome)))
  df4 <- df3[df3$State == state, ]
  df5 <- df4[complete.cases(df4), ]
  df6 <- df5[order(df5$Outcome, df5$Hospital), ]
  
  #return the hospital name in that state with the given rank
  row_index <- numeric()
  if (num == "best") {
    row_index <- 1
  } else if (num == "worst") {
    row_index <- nrow(df6)
  } else {
    row_index <- num
  }
  
  result <- df6[row_index, 1]
  return(result)
}
