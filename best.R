best <- function(state, outcome) 
{
  ## Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  # get the distinct values for states 
  states = unique(data[,7]) 
  if (!is.element(state, states)) 
      stop("Invalid State!")
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  # outcomes column index
  ocols <- c(11,17,13)
  if (!is.element(outcome, outcomes))
          stop("Invalid Outcome!")
  
  # the col index to the desired outcome 
  ocol = ocols[match(outcome,outcomes)]
  
  # get the outcomes that belongs to the state
  sdata <- data[data$State==state, ]
  
  # get the index to min  
  idx <- which.min(as.double(sdata[, ocol]))
  
  ## Return hospital name (coloumn 2) in that state with 
  ## lowest 30-day death rate
  sdata[idx,2]
}
