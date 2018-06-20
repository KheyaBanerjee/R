#
# Final project
# CACDS R course
# Kheya Banerjee
# June 2018
#

# Loading packages


# Reading the csv data file

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(data)
ncol(data)
names(data)


# Plotting the histogram for mortality rate

data[, 11] <- as.numeric(data[, 11])
hist(data[, 11], xlab = "30-day Death Rate", main = "Heart Attack 30-day Death Rate")


################################################################
#......2. FINDING THE BEST HOSPITAL IN A STATE.................#
################################################################

best <- function(state, outcome) {
  
  ## Read the outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  ## Check that state and outcome are valid
  if (!state %in% unique(data$State)) {
    stop("invalid state")
  }
  switch(outcome, `heart attack` = {
    col <- 11
  }, `heart failure` = {
    col <- 17
  }, pneumonia = {
    col <- 23
  }, stop("invalid outcome"))
  
  ## Return hospital name in that state with lowest 30-day death rate
  df <- data[data$State == state, c(2, col)]
  return(df[which.min(df[, 2]), 1])
  
}

best("TX", "heart attack")

best("TX", "heart failure")

best("MD", "heart attack")

best("MD", "pneumonia")

best("BB", "heart attack")

best("NY", "hert attack")







################################################################
#......3. RANKING HOSPITALS BY OUTCOME IN A STATE..............#
################################################################




rankhospital <- function(state, outcome, num = "best") {
  
  ## Read the outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  ## Check that state and outcome are valid
  if (!state %in% unique(data$State)) {
    stop("invalid state")
  }
  switch(outcome, `heart attack` = {
    col <- 11
  }, `heart failure` = {
    col <- 17
  }, pneumonia = {
    col <- 23
  }, stop("invalid outcome"))
  
  df <- data[data$State == state, c(2, col)]
  df <- na.omit(df)
  nhospital = nrow(df)
  
  switch(num, best = {
    num <- 1
  }, worst = {
    num <- nhospital
  })
  if (num > nhospital) {
    return(NA)
  }
  
  
  ## Return hospital name in that state with the given rank 30-day death rate
  ord <- order(df[, 2], df[, 1])
  df[ord, ][num, 1]
}


rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst")

rankhospital("MN", "heart attack", 5000)




################################################################
#..........4. RANKING HOSPITALS IN ALL STATE...................#
################################################################


rankall <- function(outcome, num = "best") {
  
  ## Read the outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  
  ## Check that state and outcome are valid
  states <- unique(data$State)
  switch(outcome, `heart attack` = {
    col <- 11
  }, `heart failure` = {
    col <- 17
  }, pneumonia = {
    col <- 23
  }, stop("invalid outcome"))
  
  ## Return hospital name in that state with the given rank 30-day death rate
  data <- data[, c(2, 7, col)]  # leave only name, state, and death rate
  data <- na.omit(data)
  
  #return(head(data))
  
  rank_in_state <- function(state) {
    df = data[data[, 2] == state, ]
    nhospital = nrow(df)
    switch(num, best = {
      num <- 1
    }, worst = {
      num <- nhospital
    })
    if (num > nhospital) {
      result <- NA
    }
    ord <- order(df[, 3], df[, 1])
    result <- df[ord, ][num, 1]
    c(result, state)
  }
  
  output <- do.call(rbind, lapply(states, rank_in_state))
  output <- output[order(output[, 2]), ]
  rownames(output) <- output[, 2]
  colnames(output) <- c("hospital", "state")
  data.frame(output)
  #return(data.frame(output))
}

head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)

tail(rankall("heart failure"), 10)