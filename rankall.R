##setwd("C:/COMMUNISM/coursera/Data Science/2/work/week4/rprog-data-ProgAssignment3-data")
rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  neededColumns <- grep("^Hospital.30.Day.Death..Mortality..Rates.from.", names(outcomeData))
  
  figureOutOutcomesColumns <- function (outComeName)
  {
    neededColumns[grep(outComeName, names(outcomeData[neededColumns]))]
  }
    
  ##outcome checkin
  if(outcome == "heart attack") {
    neededColumns <- figureOutOutcomesColumns("Heart.Attack")
  }
  else if (outcome == "heart failure") {
    neededColumns <- figureOutOutcomesColumns("Heart.Failure")
  }
  else if (outcome == "pneumonia") {
    neededColumns <- figureOutOutcomesColumns("Pneumonia")
  }
  else {stop("invalid outcome")}
  
  outcomeData[,neededColumns] <- sapply(outcomeData[,neededColumns], function(x) as.numeric(x))
  outcomeDataClean <- outcomeData [!is.na(outcomeData [,neededColumns]),]
  outcomeDataClean <- outcomeDataClean[,c("State", "Hospital.Name", names(outcomeDataClean)[neededColumns])]
  outcome
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}