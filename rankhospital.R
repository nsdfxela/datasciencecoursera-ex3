##rankhospital("MD", "heart failure", 5)
rankhospital <- function (state, outcome, num){
  
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  neededColumns <- grep("^Hospital.30.Day.Death..Mortality..Rates.from.", names(outcomeData))
  
  figureOutOutcomesColumns <- function (outComeName)
  {
    neededColumns[grep(outComeName, names(outcomeData[neededColumns]))]
  }
  
  
  
  ## Check that state and outcome are valid
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
  ##state checkin
  if(max(outcomeData[,"State"]==state) == 0)
    stop("invalid state")
  
  
  outcomeData[,neededColumns] <- sapply(outcomeData[,neededColumns], function(x) as.numeric(x))
  outcomeDataClean <- outcomeData [!is.na(outcomeData [,neededColumns]),]

  outcomeState <- outcomeDataClean[outcomeDataClean[,"State"]==state, ]
  ##outcomeState <- transform (outcomeState, rnk = rank(outcomeState[, neededColumns], ties.method="min"))
  
  index <- with(outcomeState, order(outcomeState[,neededColumns], outcomeState[,"Hospital.Name"]))
  ##outcomeState
  res <- outcomeState[index, ] ##sorted by neededColumns and then by Hospital.Name
  
  ##num checkin
  if(num == "best") num <- 1
  if(num == "worst") num <-length(res[,1]) 
  
  
  name <- res[num, "Hospital.Name"]
  if(is.null(name)) NA
  else name
  
}