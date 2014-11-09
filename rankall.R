rankall <- function(outcome, num = "best") {
  ## Read outcome data
  x <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  otcm <-  c("heart attack", "heart failure","pneumonia")
  if (outcome %in% otcm){
    switch(outcome,
           "heart attack"={ocm="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"},
           "heart failure"={ocm="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"},
           "pneumonia"={ocm="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
    )
    t <- x[,ocm]
    hospital <- x[,"Hospital.Name"]
    state <- x[,"State"]
    t=="Not Available" -> p
    t -> z
    for (i in 1:length(t)){if(p[i]){z[i]=NA}}
    as.numeric(as.character(z)) -> o
    df <- data.frame(o,hospital,state)
    df[complete.cases(df),] -> g
    sortdf <- g[with(g, order(state,o,hospital)), ]
    snam <- levels(sortdf$state)
    i <- 1
    b <- 0
    rank <- c()
    for (j in 1:nrow(sortdf)){
      if(sortdf$state[j]==snam[i]){
        b <- b+1
        rank <- append(rank,b) 
        }
      else{
        i <- i+1
        b <- 1
        rank <- append(rank,b) 
      }
    }
    if(num=="best") {
      num <- 1
    }
    else if(num=="worst"){
      num <- max(rank)
    }
    else{
      num <- num
    }
    new <- data.frame(rank,sortdf)
    nsortdf <- new[with(new, order(rank,state,o,hospital)), ]
    z <- subset(nsortdf,rank==num,select=c("hospital","state"))
    if(nrow(z)!=length(snam)){
      temp <- tmp <- c()
      for (i in 1:length(snam)){
        if(!(snam[i] %in% z$state)){
          temp <- append(temp,NA)
          tmp <- append(tmp,snam[i])
        }
      }
      jiond <- data.frame(tmp,temp)
      names(jiond) <- c("state","hospital")
    }
    else{
      jiond <- c()
    }
    slast <- rbind.data.frame(z,jiond)
    tlast <- slast[with(slast, order(state,hospital)), ]
    ans <- tlast
  }
  else{
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return(ans)
}

