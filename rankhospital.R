rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  x <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  lev <- levels(x$State)
  otcm <-  c("heart attack", "heart failure","pneumonia")
  if ((state %in% lev)&(outcome %in% otcm)){
    switch(outcome,
           "heart attack"={ocm="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"},
           "heart failure"={ocm="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"},
           "pneumonia"={ocm="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
    )
    t <- x[x$State==state,ocm]
    s <- x[x$State==state,"Hospital.Name"]
    t=="Not Available" -> p
    t -> z
    for (i in 1:length(t)){if(p[i]){z[i]=NA}}
    as.numeric(as.character(z)) -> o
    f <- as.character(s) 
    df <- data.frame(o,f)
    sortdf <- df[with(df, order(o,f)), ]
    rsortdf <- df[with(df, order(-o,f)), ]
    if(num=="best") {
      num <- 1
      ans <- as.character(sortdf[num,2])
    }
    else if(num=="worst"){
      num <- 1
      ans <- as.character(rsortdf[num,2])
    }
    else {
      ans <- as.character(sortdf[num,2])
    }
    return(ans)
  }
  else if(!(state %in% lev)){
    stop("invalid state")
  }
  else if(!(outcome %in% otcm)){
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
