rankall <- function(outcome, num = "best") 
{
  ## Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  # get the distinct values for states 
  states = unique(data[,7]) 
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  # outcomes column namessubmi
  ocols <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
             "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  # check valid outcome 
  if (!is.element(outcome, outcomes))
          stop("Invalid Outcome!")
  
  # the col name to the desired outcome 
  colname = ocols[match(outcome,outcomes)]
  
  ## For each state, find the hospital of the given rank
  hospital<-character(0)
  i <- 0
  
  # loop thru the states 
  for (state in states)
  {
    i <- i+1
    
    # get the outcomes that belongs to the state
    sdata <- data[data$State==state, ]
    
    # sort the state outcome by hospital name and outcome   
    sortdata <- sdata[order(as.numeric(sdata[[colname]]),
                            sdata$Hospital.Name,
                            decreasing=FALSE,
                            na.last=NA),]
    ##sortdata <- sortdata[!is.na(sdata$Hospital.Name),]
    
    idx = num
    
    if (num =="best")
    {
      idx=1
    }
    else if (num == "worst")
    {
      idx = nrow(sortdata)
    }  
    ## Return hospital name (coloumn 2) in that state with 
    ## the given rank
    hospital[i] = sortdata[idx,"Hospital.Name"]
  }
  
  ## Return a data frame with the hospital names and 
  ## the (abbreviated) state name
  data.frame(hospital=hospital,state=states,row.names=states)
}
