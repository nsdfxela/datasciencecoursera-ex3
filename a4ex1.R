##Assigment-3-week-4
##best("TX", "heart attack")

best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  neededColumns <- grep("^Hospital.30.Day.Readmission.Rates.from", names(outcomeData))
  
  figureOutOutcomesColumns <- function (outComeName)
  {
    neededColumns[grep(outComeName, names(outcomeData[neededColumns]))]
  }
  
  
  
  ## Check that state and outcome are valid
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
  
  outcomeData[,neededColumns] <- lapply(outcomeData[,neededColumns], function(x) as.numeric(x))
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}

