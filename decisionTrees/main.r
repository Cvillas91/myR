# Decision Trees 
#
# Details:
#  
# Return to your submission for Project #2. Now that you've learned about predictions with decision trees, 
# how can your model be modified to predict outcomes?
# Go back to your project and refactor your code so that you are implementing at least one set of 
# decision trees rather than traditional procedural logic.
#
# Once you've refactored your code, run your simulation again. 
# Are the results the same? Are they different? Why or why not? Write up your thoughts in a 
# short paragraph of comments and include the comment in your refactored code.

#############################################

setwd('rootfolder')
library(xlsx)
library(dplyr)
library(tree)

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

##### Up to here is the same as project #2
############ REFACTORING THE CODE #######################

# Sample train and test the tree model
ds4sample <- sample(1:nrow(ds4),0.8* nrow(ds4))
train <- ds4[ds4sample,]
test <- ds4[-ds4sample,]

tree1 <- tree(Points ~ Prob_T1_Win + Prob_T2_Win + Outcome, data = train)
PointsPred <- predict(tree1,test)
Actual_Pred <- data.frame(actuals = test$Points, predicted = round(PointsPred, digits =1))

# Applying the model to all data and comparing it with the actual results
tree2 <- tree(Points ~ Prob_T1_Win + Prob_T2_Win + Outcome, data = ds4)
PointsPred2 <- predict(tree2,ds4)
Final_Results <- data.frame(Team1 = ds4$Team1, FinalPointNoModel = ds4$Points, FinalPointTreeModel = round(PointsPred2, digits =1))

ds6 <- Final_Results %>% 
  group_by(Team1) %>% 
  summarise(FinalPointNoModel = sum(FinalPointNoModel),
            FinalPointTreeModel = sum(FinalPointTreeModel),
            LikelihoodOfWinningLeagueNoModel =round(FinalPointNoModel/totalPointsLeague*100,2),
            LikelihoodOfWinningLeagueTreeModel =round(FinalPointTreeModel/totalPointsLeague*100,2)) %>%
  arrange(desc(FinalPointTreeModel))

#### THOUGHTS ABOUT THE RESULTS
# The results are almost the same between both (predicted and non predicted models) that hints us that the
# model tree in this scenarios is very accurate, this is mainly because the outcome of this variable model is 
# either 1 or 0, (Win or Lose). The model also receives quite a large number of observations so that can also help
# on the accuracy of the model. It is a great tool when analyzing the data, simpler code-wise speaking, easier to
# read and interpret.
