best <- function(state, outcome) 
{
  originaldata <- read.csv("outcome-of-care-measures.csv")
  thebest <- NULL
  out_col <- NULL
  if(outcome == "heart attack")
  {
    out_col <- c(11)
  }
  if(outcome == "heart failure")
  {
    out_col <- c(17)
  }
  if(outcome == "pneumonia")
  {
    out_col <- c(23)
  }
  else
  {
    stop("invalid outcome")
  }
  
  statedata <- subset(outcome, 7 == state, c(2,out_col))
  
  return(thebest)
}