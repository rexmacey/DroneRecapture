library(tidyverse, quietly = TRUE)
nPerDCA <- 500
nCapturedPerSession <- 50
nDaysToMate <- 21

minutesFlightTime <- 30 # to travel to and from and stay in DCA
milesToDCA <- 0.5 
flightMPH <- 15
minutesRestTime <- 30
flightsPerDay <- 4 # number of trips a drone makes per day/
hoursWorkDay <- 4 # hours in a day a drone flies and rests to determine trips per day
numberDCAsVisited <- 1  # for future use
daysInSimulation <- 10
daysActive <- c(1,2,3,8,9,10)

calcMinutesAtDCA <- function(minutesflightTime, milesToDCA, flightMPH){
  milesRoundTrip <- 2 * milesToDCA
  hoursRoundTrip <- milesRoundTrip / flightMPH
  minutesRoundTrip = hoursRoundTrip * 60
  return(minutesflightTime - minutesRoundTrip)
}

minutesAtDCA <- calcMinutesAtDCA(minutesFlightTime, milesToDCA, flightMPH)

sim <- 