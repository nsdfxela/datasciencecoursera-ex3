##Assigment-3-week-4

best <- function(state, outcome) {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  neededColumns <- grep("^Hospital.30.Day.Readmission.Rates.from", names(outcome))
  outcome[,neededColumns] <- as.numeric (outcome[,neededColumns])
  lapply(outcome[,neededColumns], function(x) as.numeric(x))
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}

