best <- function(state, outcome) 
{
  ## Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  # get the distinct values for states 
  states = unique(data[,8]) 
  if (!is.element(state, states)) 
      stop("Invalid State!")
  
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome, outcomes))
          stop("Invalid Outcome!")
      
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  
  
  
  
}
