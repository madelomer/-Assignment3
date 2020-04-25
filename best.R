outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])


outcome[2:10, c(11,19)] 
names(outcome)
ncol(outcome)
nrow(outcome)

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!(state %in% data$State)) {
    message <- "invalid state"
  }
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    message <- "invalid outcome"
  }
  else{
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    keys <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
    outcomeKey <- keys[outcome]
    
 
    dataPerState <- split(data, data$State)
    dataOurState <- dataPerState[[state]]
    dataOurState <- dataOurState[ order(dataOurState["Hospital.Name"]), ]
    dataOutcome <- suppressWarnings(as.numeric(dataOurState[, outcomeKey]))
    good <- complete.cases(dataOutcome)
    dataOutcome <- dataOutcome[good]
    dataOurState <- dataOurState[good,]
    minimum <- min(dataOutcome)
    index <- match(minimum, dataOutcome)
    message <- dataOurState[index, 2]
  }
  message
}
best("NY", "pneumonia")