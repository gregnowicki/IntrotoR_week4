#rankall.R

rankall <- function(outcome, num = "best") {
  
  #read the data in:
  df <- data.frame()
  df <- read.csv("/Users/gregnowicki/Documents/Coursera/ProgrammingAssignment4/outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  #check that the input for outcome is valid:
  olist <- c("heart attack", "heart failure", "pneumonia")
  slist <- unique(df$State)
  if ((match(outcome, olist) > 0) != TRUE) { stop("invalid input") }
  
  #subset the data to only get the columns we're interested in:
  df2 <- df[, c(2, 7, 11, 17, 23)]
  outcome_index <- numeric()
  
  #create a column selector for subsetting
  if (outcome == "heart attack") { 
    outcome_index <- 3
  } else if (outcome == "heart failure") {
    outcome_index <- 4
  } else if (outcome == "pneumonia") {
    outcome_index <- 5
  }
  
  #subset the data further to only pull in the outcome we're interested in. Sort the list by state, outcome, name (in that order):
  df3 <- df2[, c(1:2, outcome_index)]
  colnames(df3) <- c("Hospital", "State", "Outcome")
  df3$Outcome <- suppressWarnings(as.numeric(as.character(df3$Outcome)))
  df3 <- df3[order(df3$State, df3$Outcome, df3$Hospital), ]
  df4 <- df3[complete.cases(df3),]
  
  #create a function to pass into lapply, to be used for row selection dependent on user input for num
  lafunc <- function(df) {
    if (num == "best") {
      df[1, ]
    }else if (num == "worst"){
      df[nrow(df), ]
    }else{
      df[num, ]
    }
  } 
  
  #split the sorted dataframe by state
  list1 <- split(df4, df4$State)
  
  #apply the lafunc function, and transpose the matrix
  list2 <- t(sapply(list1, lafunc))
  
  #return list2
  return(list2)
  
}
