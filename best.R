best <- function(state, outcome) 
{
  originaldata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  thebest <- NULL
  st_data <- NULL
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
  st_data <- subset(originaldata, State == state, c(2,out_col))
  
  colnames(st_data) <- c("Name", "Score")
  st_data <- st_data[complete.cases("Score"),]
  
  transform(st_data, Score = as.numeric(as.character(Score)))
  attach(st_data)
  ##st_data <- suppressWarnings(st_data[order(as.numeric(as.character(Score))),])
  st_data <- st_data[order(Score, Name),]
  detach(st_data)
  thebest <- as.character(st_data[1,1])
  return(thebest)
}
