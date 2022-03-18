setwd('\rootfolder\')
library(xlsx)
library(dplyr)
library(ggplot2)

NBAstats <- read.csv(file = "NBAstatsSeason19_20.csv", header = T)
names(NBAstats)

# Calculate the metrics, create the data frame, by player for each team
ds <- NBAstats %>% 
  group_by(TEAM_CITY,PLAYER_NAME) %>% 
  summarise(avgPTS = mean(PTS, na.rm = T), 
            minPTS = min(PTS, na.rm = T),
            maxPTS = max(PTS,na.rm = T),
            medPTS = median(PTS, na.rm = T),
            lowerQuantilePTS = quantile(PTS, 0.25),
            upperQuantilePTS = quantile(PTS, 0.75)) %>%
  arrange(desc(avgPTS))

# Calculate the metrics, create the data frame, by team
ds2 <- (NBAstats %>% 
          group_by(TEAM_CITY, GAME_ID) %>% 
          summarise(totalPTS = sum(PTS, na.rm = T))) %>% 
  group_by(TEAM_CITY) %>% 
  summarise(avgPTS = mean(totalPTS, na.rm = T), 
            minPTS = min(totalPTS, na.rm = T),
            maxPTS = max(totalPTS,na.rm = T),
            medPTS = median(totalPTS, na.rm = T),
            stdDevPTS = sd(totalPTS,na.rm = T),
            lowerQuantilePTS = quantile(totalPTS, 0.25),
            upperQuantilePTS = quantile(totalPTS, 0.75)) %>%
  arrange(desc(avgPTS))

##### ds3 Calculates the probability of winning against other teams using the total Avg Scored point of each team
ds3 <- data.frame(Team1 = character(),
                  Team2 = character(),
                  ProbT1W = double(),
                  ProbT2W = double())
counterJ <- 0

for (i in 1:30){
  for (j in 1:30){
      ds3[j + counterJ,1] <- ds2[i,1]
      ds3[j + counterJ,2] <- ds2[j,1]
      ds3[j + counterJ,3] <- ds2[i,2] /(ds2[j,2]+ds2[i,2])
      ds3[j + counterJ,4] <- 1-(ds2[i,2] /(ds2[j,2]+ds2[i,2]))
  }
  counterJ <- counterJ+30
}

### Simulates the 20 matches with the potential outcomes
ds4 <- data.frame(Team1 = character(),
                  Team2 = character(),
                  Prob_T1_Win = double(),
                  Prob_T2_Win = double(),
                  Outcome = double(),
                  Winner = character(),
                  Points = double())
counterJ <- 0
j <- 0

for (i in 1:30){
  while (j <= 20){
    x <- sample(1:30,1)
    w <- round(as.numeric(sample(1:100000,1)/100000),4) #Generates a random number between 0 and 1
    if (x != i){
      ds4[j + counterJ,1] <- toString(ds2[i,1])
      ds4[j + counterJ,2] <- toString(ds2[x,1])
      z <- round(as.numeric(ds2[i,2] /(ds2[x,2]+ds2[i,2])),4)
      ds4[j + counterJ,3] <- z
      ds4[j + counterJ,4] <- round(as.numeric(1-(ds2[i,2] /(ds2[x,2]+ds2[i,2]))),4)
      ds4[j + counterJ,5] <- w
      if (w <= z){ # the random number is equals/lesser than the probability of T1 to win, then T1 wins
        ds4[j + counterJ,6] <- ds2[i,1]
        ds4[j + counterJ,7] <- as.integer(1)
      }else { # Else T2 Wins
        ds4[j + counterJ,6] <- ds2[x,1]
        ds4[j + counterJ,7] <- as.integer(0)
      }
      j = j + 1
    }
  }
  counterJ <- counterJ+20
  j <- 0
}

totalPointsLeague <- sum(ds4$Points) #Total points won by every team in the league

### final data frame with the results based on how many matches they won vs how many matches were played.
ds5 <- ds4 %>% 
  group_by(Team1) %>% 
  summarise(totalPoints = sum(Points),
            LikelihoodOfWinningLeague =round(totalPoints/totalPointsLeague*100,2)) %>%
  arrange(desc(totalPoints))
