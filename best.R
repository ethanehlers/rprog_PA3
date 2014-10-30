best <- function(state, outcome) 
{
  originaldata <- read.csv("outcome-of-care-measures.csv")
  thebest <- NULL
  col_array <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia")))
  {
    stop("invalid outcome") 
  } 
  if(!is.element(state, originaldata$State))
  {
    stop("invalid state")
  }
  out_col <- col_array[outcome]
  statedata <- subset(originaldata, State == state, c(2,out_col))
  colnames(statedata) <- c("Name", "Score")
  attach(statedata)
  as.integer(statedata$Score)
  thebest <- statedata[order(Score),]
  return(thebest[1,1])
}