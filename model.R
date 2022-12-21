
####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Importing libraries
library(RCurl) # for downloading the iris CSV file
library(randomForest)
library(caret)
library(tidyverse)

# Importing the Iris data set
soccer <- read.csv(file = "C:/Users/srira/OneDrive/Documents/R/work_dir/soccer_up.csv") 

soccer_1 <- soccer %>%
  mutate (WinTeam= case_when(Winner > 0 ~ 'Away',
                             Winner < 0 ~ 'Home',
                             Winner == '0' ~ 'Draw')) %>%
  select('home_team', 'away_team',  'neutral', 'WinTeam') 

#soccer_1$home_team = as.factor(soccer_1$home_team)
#soccer_1$away_team = as.factor(soccer_1$away_team)
#soccer_1$neutral <- factor(soccer_1$neutral, levels = c( "TRUE", "FALSE"))
#soccer_1$WinTeam <- factor(soccer_1$WinTeam, levels = c("Away", "Draw", "Home"))



TrainingIndex <- createDataPartition(soccer_1$WinTeam, p=0.8, list = FALSE)
TrainingSet <- soccer_1[TrainingIndex,] # Training Set
TestingSet <- soccer_1[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

TrainSet$WinTeam <- factor(TrainSet$WinTeam, levels = c("Away", "Draw", "Home"))
TrainSet$home_team = as.factor(TrainSet$home_team)
TrainSet$away_team = as.factor(TrainSet$away_team)
#soccer_1$country <- factor(soccer_1$country, levels = c(unique(soccer_1$country)))
#soccer_1$Continent <- factor(soccer_1$Continent, levels = c(unique(soccer_1$Continent)))
TrainSet$neutral <- factor(TrainSet$neutral, levels = c( "TRUE", "FALSE"))


# Building Random forest model

model <- randomForest(WinTeam ~ ., data = TrainSet, ntree = 500, mtry = 3, importance = TRUE)

# Save model to RDS file
saveRDS(model, "soccer.rds")
  