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
  colnames(outcomeDataClean) <- c("state", "hospital name", "rate")
  
  f <- as.factor(outcomeDataClean[,"state"])
  outcomeDataGroupes <- split(outcomeDataClean, f)
  
  sortStateHospitals <- function(raw)
  {
    index <- with(raw, order(raw[,"rate"], raw[,"hospital name"]))
    raw[index, ]
  }
  
  ##result <- data.frame(hospital <- character(length(outcomeDataGroupes)),
  ##                     state <- character(length(outcomeDataGroupes)) )
  result <- data.frame()
  for(i in 1:length(outcomeDataGroupes)){
    
    numl <- num
    if(num == "best") numl <- 1
    if(num == "worst") numl <-length(outcomeDataGroupes[[i]][,1]) 
    
    outcomeDataGroupes[[i]] <- sortStateHospitals(outcomeDataGroupes[[i]])
                result [i,"hospital"]<- outcomeDataGroupes[[i]][numl,"hospital name"]
                result [i,"state"]<- names(outcomeDataGroupes[i])
                ##result [i,"state"]<- outcomeDataGroupes[[i]][num,"state"]
  }
  
  result
    
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}