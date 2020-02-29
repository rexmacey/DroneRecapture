library(tidyverse, quietly = TRUE)
nTrials <- 1000
nPerDCA <- 500
nCapturedPerFlight <- 20
nDaysToMate <- 21

# probOfMating <- 1/2000 # Used to kill off some bees
# to do reduce period in a DCA by flight time in archive
nDCAsVisited <- 20
nDCAsVisitedByKeeper <- 3
daysInSimulation <- 10
daysActive <- c(1,2,3,8,9,10)

periodsPerDay <- 12 # of 15 minutes.  When researcher is looking for drones
periodsPerRest <- 1
periodsPerFlight <- 2 # bee drone, not UAV
UAVFlightsPerPeriod <- 1 # 

nDrones <- nPerDCA / (periodsPerFlight / (periodsPerFlight * nDCAsVisited + periodsPerRest * nDCAsVisited))

probAtRest <- periodsPerRest / (periodsPerRest + periodsPerFlight)
probAtDCA <- (1-probAtRest) / nDCAsVisited

set.seed(101)
guessPer2Location <- function(per1Location, nDCAsVisited){
  out <- numeric(length(per1Location))
  idx <- per1Location == 0
  out[idx] <- sample(1:nDCAsVisited, sum(idx), TRUE) # random DCA 
  out[!idx] <- sample(0:1, sum(!idx), TRUE) * per1Location[!idx] # previous DCA or rest
  return(out)
}
calcNextPerLocation <- function(penultLocation, ultLocation,nDCAsVisited){
  out <- numeric(length(penultLocation))
  idx <- ultLocation == 0
  out[idx] <- sample(1:nDCAsVisited, sum(idx), TRUE) 
  idx <- penultLocation == 0
  out[idx] <- ultLocation[idx]
  # otherwise rest
  return(out)
}
calcKeeperLocation <- function(periodsPerDay, nDCAsVisitedByKeeper){
  if(nDCAsVisitedByKeeper > periodsPerDay) warning(paste("Can't visit", nDCAsVisitedByKeeper, "in ", periodsPerDay, "periods."))
  out <- floor(0:(periodsPerDay-1)/(periodsPerDay/nDCAsVisitedByKeeper)) +1
  return(out)
}
keeperLocation <- calcKeeperLocation(periodsPerDay, nDCAsVisitedByKeeper)


simResults <- matrix(0, nrow= nTrials, ncol = daysInSimulation)
colnames(simResults) <- paste0("Day",1:daysInSimulation)

for(trial in 1:nTrials){
  droneTable <- tibble(Num = 1:nDrones,
                       MatingDay = sample(1:nDaysToMate, nDrones, TRUE),
                       Marked = 0, 
                       Per1Location = sample(0:nDCAsVisited, size = nDrones, replace = TRUE, prob = c(probAtRest, rep(probAtDCA, nDCAsVisited))))
  
  
  if(periodsPerDay >= 2){
    droneTable <- droneTable %>% mutate(Per2Location = guessPer2Location(Per1Location, nDCAsVisited))
  }
  if(periodsPerDay >= 3){
    droneTable[, paste0("Per", 3:periodsPerDay, "Location")] = NA 
    for(i in 3:periodsPerDay){
      ultLoc <- paste0("Per", i-1, "Location")
      penultLoc <- paste0("Per", i-2, "Location")
      droneTable[,paste0("Per", i, "Location")] <- calcNextPerLocation(droneTable[,penultLoc, drop=TRUE], droneTable[,ultLoc, drop=TRUE], nDCAsVisited)
    }
  }
  
  recaptureTableDiff <- matrix(0, nrow = daysInSimulation, ncol=periodsPerDay) # different DCA
  recaptureTableSame <- matrix(0, nrow = daysInSimulation, ncol=periodsPerDay) # same DCA
  markedByDay <- numeric(daysInSimulation)
  
  for(day in 1:daysInSimulation){
    if(day %in% daysActive){
      # for each periods
      for(period in 1:periodsPerDay){
        currentDCA <- keeperLocation[period]
        for(j in 1:UAVFlightsPerPeriod){
          # capture some bees 
          temp <- droneTable %>% filter(get(paste0("Per",period,"Location")) == currentDCA) %>%
            sample_n(nCapturedPerFlight)
          # check for marking
          recaptureTableDiff[day, period] <- as.numeric(temp %>% filter(Marked != 0 & Marked != currentDCA) %>% count())
          recaptureTableSame[day, period] <- as.numeric(temp %>% filter(Marked != 0 & Marked == currentDCA) %>% count())
          markedByDay[day] <- as.numeric(markedByDay[day] + temp %>% filter(Marked == 0) %>% count())
          # mark if not marked
          droneTable <- droneTable %>% mutate(Marked = ifelse(Num %in% temp$Num & Marked == 0, currentDCA, Marked))
        }
      }
    }
    # end of day
    # replace those dying from old age
    
    droneTable <- droneTable %>% mutate(MatingDay = MatingDay +1) %>% 
      mutate(Marked = ifelse(MatingDay > nDaysToMate, 0, Marked)) %>%
      mutate(MatingDay = ifelse(MatingDay > nDaysToMate, 1, MatingDay))
  }
  
  #print(paste("Number of drones", nrow(droneTable)))
  #markedByDay
  #rowSums(recaptureTableDiff)
  #rowSums(recaptureTableSame)
  simResults[trial,] <- rowSums(recaptureTableDiff)
}


summary(simResults)
