library(readxl)
library(ggplot2)
library(plyr)
library(reshape2)
library(waffle)
library(googleVis)
library(corrplot)
library(tidyverse)
library(data.table)
library(car)
library(lattice)
library(Hmisc)
library(caret)
library(RWeka)
library(rpart)
library(e1071)
library(partykit)
library(OneR)
library(mlbench)
library(FSelector)
library(tidyverse)
library(rpart)
library(class)
library(dplyr)
library(neuralnet)

Cont <- read_excel("NBA Salary by Contract.xlsx")

# Creating variables

## Points per Game

Cont <- Cont %>%
  mutate(PPG = PTS / G)
Cont$PPG <- as.numeric(Cont$PPG)
Cont <- Cont %>%
  mutate(PPG2 = PTS2 / G2)
Cont$PPG2 <- as.numeric(Cont$PPG2)
Cont <- Cont %>%
  mutate(PPG3 = PTS3 / G3)
Cont$PPG3 <- as.numeric(Cont$PPG3)
## ATR percentage
Cont <- Cont %>%
  mutate(ATRper = ASTper / TOVper)
Cont$ATRper <- as.numeric(Cont$ATRper)
Cont <- Cont %>%
  mutate(ATRper2 = ASTper2 / TOVper2)
Cont$ATRper2 <- as.numeric(Cont$ATRper2)
Cont <- Cont %>%
  mutate(ATRper3 = ASTper3 / TOVper3)
Cont$ATRper3 <- as.numeric(Cont$ATRper3)

## Standardizing Win Shares
Cont <- Cont %>%
  mutate(WS_st = (WS - mean(WS)) / sd(WS))
Cont <- Cont %>%
  mutate(WS_st2 = (WS2 - mean(WS2)) / sd(WS2))
Cont <- Cont %>%
  mutate(WS_st3 = (WS3 - mean(WS3)) / sd(WS3))

## Standardizing Player Efficiency Rating
Cont <- Cont %>%
  mutate(PER_st = (PER - mean(PER)) / sd(PER))
Cont <- Cont %>%
  mutate(PER_st2 = (PER2 - mean(PER2)) / sd(PER2))
Cont <- Cont %>%
  mutate(PER_st3 = (PER3 - mean(PER3)) / sd(PER3))

## Standardizing Value Over Replacement Player
Cont <- Cont %>%
  mutate(VORP_st = (VORP - mean(VORP)) / sd(VORP))
Cont <- Cont %>%
  mutate(VORP_st2 = (VORP2 - mean(VORP2)) / sd(VORP2))
Cont <- Cont %>%
  mutate(VORP_st3 = (VORP3 - mean(VORP3)) / sd(VORP3))

## Standardizing Box Plus Minus
Cont <- Cont %>%
  mutate(BPM_st = (BPM - mean(BPM)) / sd(BPM))
Cont <- Cont %>%
  mutate(BPM_st2 = (BPM2 - mean(BPM2)) / sd(BPM2))
Cont <- Cont %>%
  mutate(BPM_st3 = (BPM3 - mean(BPM3)) / sd(BPM3))

## Combining all standardized advanced statistics
Cont <- Cont %>%
  mutate(ADV = WS_st + BPM_st + VORP_st + PER_st)
Cont <- Cont %>%
  mutate(ADV2 = WS_st2 + BPM_st2 + VORP_st2 + PER_st2)
Cont <- Cont %>%
  mutate(ADV3 = WS_st3 + BPM_st3 + VORP_st3 + PER_st3)

## Standardizing combination of all standardized advanced statistics
Cont <- Cont %>%
  mutate(ADV = (ADV - mean(ADV)) / sd(ADV))
Cont$ADV <- as.numeric(Cont$ADV)
Cont <- Cont %>%
  mutate(ADV2 = (ADV2 - mean(ADV2)) / sd(ADV2))
Cont$ADV2 <- as.numeric(Cont$ADV2)
Cont <- Cont %>%
  mutate(ADV3 = (ADV3 - mean(ADV3)) / sd(ADV3))
Cont$ADV3 <- as.numeric(Cont$ADV3)



## Removing NA and Infinite values
Cont$ATRper[is.infinite(Cont$ATRper)] <- 0
Cont$ATRper2[is.infinite(Cont$ATRper2)] <- 0
Cont$ATRper3[is.infinite(Cont$ATRper3)] <- 0
Cont$ATRper[is.na(Cont$ATRper)] <- median(Cont$ATRper, na.rm = TRUE)
Cont$ATRper2[is.na(Cont$ATRper2)] <- median(Cont$ATRper2, na.rm = TRUE)
Cont$ATRper3[is.na(Cont$ATRper3)] <- median(Cont$ATRper3, na.rm = TRUE)

# Creating categotical (ordinal) variable for salary (4 levels)

firop <- subset(Cont, `Next cont` >= 0.25)
firop$sal_type <- 'first_option'
secop <- subset(Cont, `Next cont` < 0.25 & `Next cont` >= 0.15)
secop$sal_type <- 'second_option'
rp <- subset(Cont, `Next cont` < 0.15 & `Next cont` >= 0.05)
rp$sal_type <- 'role_player'
min <- subset(Cont, `Next cont` < 0.05)
min$sal_type <- 'minimum'
NBA <- rbind(firop, secop, rp, min)
NBA$sal_type <- as.factor(NBA$sal_type)

## Rounding Numeric Variables
NBA$ADV <- round(NBA$ADV, digits = 4)
NBA$ADV2 <- round(NBA$ADV2, digits = 4)
NBA$ADV3 <- round(NBA$ADV3, digits = 4)
NBA$PPG <- round(NBA$PPG, digits = 2)
NBA$PPG2 <- round(NBA$PPG2, digits = 2)
NBA$PPG3 <- round(NBA$PPG3, digits = 2)
NBA$ATRper <- round(NBA$ATRper, digits = 2)
NBA$ATRper2 <- round(NBA$ATRper2, digits = 2)
NBA$ATRper3 <- round(NBA$ATRper3, digits = 2)
NBA$TRBper2 <- round(NBA$TRBper2, digits = 1)
NBA$STLper2 <- round(NBA$STLper2, digits = 1)
NBA$BLKper2 <- round(NBA$BLKper2, digits = 1)
NBA$TOVper2 <- round (NBA$TOVper2, digits = 1)
NBA$USGper2 <- round(NBA$USGper2, digits = 1)
NBA$G2 <- round(NBA$G2, digits = 2)
NBA$Age2 <- round(NBA$Age, digits = 2)
NBA$FGper2 <- round(NBA$FGper, digits = 4)
NBA$`Next cont` <- as.numeric(NBA$`Next cont`)

NBAsub <- NBA[, -c(8, 15, 16, 17, 19, 22, 29, 30, 31, 33, 34, 37, 44, 45, 46, 48, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67)]
NBAsub2 <- NBAsub[, -c(1, 2, 3, 5, 6, 9, 12, 14, 15, 18, 21, 23, 27, 30, 32, 33)]


# Looking at correlation for each potential descriptive feature to the target feature
print(cor(NBAsub$`Next cont`, NBAsub2$G))
print(cor(NBAsub$`Next cont`, NBAsub2$G2))
print(cor(NBAsub$`Next cont`, NBAsub2$G3))

print(cor(NBAsub$`Next cont`, NBAsub2$PPG))
print(cor(NBAsub$`Next cont`, NBAsub2$PPG2))
print(cor(NBAsub$`Next cont`, NBAsub2$PPG3))

print(cor(NBAsub$`Next cont`, NBAsub2$ADV))
print(cor(NBAsub$`Next cont`, NBAsub2$ADV2))
print(cor(NBAsub$`Next cont`, NBAsub2$ADV3))

print(cor(NBAsub$`Next cont`, NBAsub2$ATRper))
print(cor(NBAsub$`Next cont`, NBAsub2$ATRper2))
print(cor(NBAsub$`Next cont`, NBAsub2$ATRper3))

print(cor(NBAsub$`Next cont`, NBAsub2$TRBper))
print(cor(NBAsub$`Next cont`, NBAsub2$TRBper2))
print(cor(NBAsub$`Next cont`, NBAsub2$TRBper3))

print(cor(NBAsub$`Next cont`, NBAsub2$STLper))
print(cor(NBAsub$`Next cont`, NBAsub2$STLper2))
print(cor(NBAsub$`Next cont`, NBAsub2$STLper3))

print(cor(NBAsub$`Next cont`, NBAsub2$BLKper))
print(cor(NBAsub$`Next cont`, NBAsub2$BLKper2))
print(cor(NBAsub$`Next cont`, NBAsub2$BLKper3))

print(cor(NBAsub$`Next cont`, NBAsub2$USGper))
print(cor(NBAsub$`Next cont`, NBAsub2$USGper2))
print(cor(NBAsub$`Next cont`, NBAsub2$USGper3))

print(cor(NBAsub$`Next cont`, NBAsub$ASTper))
print(cor(NBAsub$`Next cont`, NBAsub$ASTper2))
print(cor(NBAsub$`Next cont`, NBAsub$ASTper3))

print(cor(NBAsub$`Next cont`, NBAsub$TOVper))
print(cor(NBAsub$`Next cont`, NBAsub$TOVper2))
print(cor(NBAsub$`Next cont`, NBAsub$TOVper3))

print(cor(NBAsub$`Next cont`, NBAsub$Age))
print(cor(NBAsub$`Next cont`, NBAsub$Age2))
print(cor(NBAsub$`Next cont`, NBAsub$Age3))

# Plots

NBA %>%
  ggplot(aes(x = Year)) +
  geom_histogram(color = 'red', binwidth = 1) +
  labs(x = 'Year',
       y = 'Players',
       title = 'Player Count by Year')

NBA %>%
  ggplot(aes(x = sal_type2)) +
  geom_bar(color = 'red') +
  labs(x = 'Salary Type',
       title = 'Player Count by Salary Type')
text(x = bp, y = tbl.train, label = round(tbl.train, digits=2), 
     pos = 3, cex = 0.8, col = "red")

NBA %>%
  ggplot(aes(x = Age)) +
  geom_histogram(color = 'red', binwidth = 1) +
  labs(x = 'Age',
       y = 'Players',
       title = 'Players by Age')

NBA %>%
  ggplot(aes(x = G)) +
  geom_histogram(color = 'red', binwidth = 8) +
  labs(x = 'Games Played',
       y = 'Players',
       title = 'Players by Games Played')

NBA %>%
  ggplot(aes(x = Pos)) +
  geom_bar(color = 'red')+
  labs(x = 'Position',
       y = 'Count',
       title = 'Player Count by Position')

NBA %>%
  ggplot(aes(x = PPG)) +
  geom_histogram(color = 'red', binwidth = 2) +
  labs(x = 'Points per Game (first year of contract)',
       y = 'Count',
       title = 'Player Count by Points per Game')

NBA %>%
  ggplot(aes(x = PPG2)) +
  geom_histogram(color = 'red', binwidth = 2) +
  labs(x = 'Points per Game (middle years of contract)',
       y = 'Count',
       title = 'Player Count by Points per Game')

NBA %>%
  ggplot(aes(x = PPG3)) +
  geom_histogram(color = 'red', binwidth = 2) +
  labs(x = 'Points per Game (last year of contract)',
       y = 'Count',
       title = 'Player Count by Points per Game')

NBA %>%
  ggplot(aes(x = `Next cont`)) +
  geom_histogram(color = 'red', binwidth = 0.05) +
  labs(x = 'Player Salary',
       y = 'Count',
       title = 'Player Count by Salary as Percentage of Teams Salary Cap')

NBA %>%
  ggplot(aes(x = ADV, y = `Next cont`)) +
  geom_point(color = 'red') +
  geom_smooth(method = 'lm') +
  labs(x = 'ADV (first year of contract)',
       y = 'Next Contract Salary',
       title = 'Advanced Statistic vs Player Salary as a Percentage of Teams Salary Cap')

NBA %>%
  ggplot(aes(x = ADV2, y = `Next cont`)) +
  geom_point(color = 'red') +
  geom_smooth(method = 'lm') +
  labs(x = 'ADV (middle years of contract)',
       y = 'Next Contract Salary',
       title = 'Advanced Statistic vs Player Salary as a Percentage of Teams Salary Cap')

NBA %>%
  ggplot(aes(x = ADV3, y = `Next cont`)) +
  geom_point(color = 'red') +
  geom_smooth(method = 'lm') +
  labs(x = 'ADV (last year of contract)',
       y = 'Next Contract Salary',
       title = 'Advanced Statistic vs Player Salary as a Percentage of Team Salary Cap')

NBA %>%
  ggplot(aes(x = PPG, y = `Next cont`)) +
  geom_point(color = 'red') +
  geom_smooth(method = 'lm') +
  labs(x = 'PPG (first year of contract)',
       y = 'Next Contract Salary',
       title = 'Points per Game vs Player Salary as a Percentage of Teams Salary Cap')

NBA %>%
  ggplot(aes(x = PPG2, y = `Next cont`)) +
  geom_point(color = 'red') +
  geom_smooth(method = 'lm') +
  labs(x = 'PPG (middle years of contract)',
       y = 'Next Contract Salary',
       title = 'Points per Game vs Player Salary as a Percentage of Teams Salary Cap')

NBA %>%
  ggplot(aes(x = PPG3, y = `Next cont`)) +
  geom_point(color = 'red') +
  geom_smooth(method = 'lm') +
  labs(x = 'PPG (last year of contract)',
       y = 'Next Contract Salary',
       title = 'Points per Game vs Player Salary as a Percentage of Teams Salary Cap')

ddply(NBA, .(sal_type), plyr::summarize, 
      count=length(sal_type),
      Salm=mean(`Next cont`), 
      PPGm=mean(PPG3),
      ADVm=mean(ADV3),
      PPGsd=sd(PPG3),
      ADVsd = sd(ADV3))

# Summary Tables and Numbers

str(NBAsub)
print(summary(NBAsub))
print(nrow(NBAsub))
print(ncol(NBAsub))

# Correlation plot

corr <- cor(NBAsub2[,1:26])
corrplot(corr, method="circle")
corrplot(corr, method="number")
corrplot(corr, method = "color")
corrplot.mixed(corr, upper = 'shade', lower = 'number') 


#### Lab 4 ###

NBA <- subset(NBA, `Next cont` <= 0.5)
NBAsub <- NBA[, -c(8, 15, 16, 17, 19, 22, 29, 30, 31, 33, 34, 37, 44, 45, 46, 48, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67)]
NBAsub2 <- NBAsub[, -c(1, 2, 3, 5, 6, 9, 12, 14, 15, 18, 21, 23, 27, 30, 32, 33)]
NBA_var <- NBA[, c(68, 69, 70, 50, 51, 52, 55, 43, 21, 6, 71)]

# Showing importance of each variable

NBA.scores <- random.forest.importance(sal_type ~ ., NBA_var )
print(NBA.scores)

# Creating an Evaluation function (Tree)

evaluator.NBA_var.tree <- function(subset) {
  # Use k-fold cross validation
  k <- 5
  splits <- runif(nrow(NBA_var))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- NBA_var[test.idx, , drop=FALSE]
    train <- NBA_var[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "sal_type"), train)
    error.rate = sum(test$sal_type != predict(tree, test, type="class")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

# Forward Search

writeLines("\nRun forward greedy search - NBA_var\n")
subset <- forward.search(names(NBA_var)[-11], evaluator.NBA_var.tree)


f <- as.simple.formula(subset, "sal_type")


# Backwards Search
writeLines("\nRun backward greedy search - NBA_var\n")
subset <- backward.search(names(NBA_var)[-11], evaluator.NBA_var.tree)

b <- as.simple.formula(subset, "sal_type")

# Hill Climing Search
writeLines("\nRun hill climb search - SG_var\n")
subset <- hill.climbing.search(names(NBA_var)[-11], evaluator.NBA_var.tree)

h <- as.simple.formula(subset, "sal_type")

# Exhuastive Search
writeLines("\nRun exhaustive search - PG_var\n")
subset <- exhaustive.search(names(NBA_var)[-11], evaluator.NBA_var.tree)

e <- as.simple.formula(subset, "sal_type")

print(f)
print(b)
print(h)
print(e)

### Lab 5 ###

# Data Visualization

NBA_list <- NBA_var[, c(3, 5, 6, 7, 8, 9, 11)]
NBA$sal_type2 <- factor(NBA$sal_type, order = TRUE, levels = c('minimum', 'role_player', 'second_option', 'first_option'))

bwplot(sal_type2 ~ PPG2, data=NBA, 
       panel=panel.bpplot, probs=seq(.01, .49, by=.01),
       datadensity=TRUE,
       ylab="sal_type", xlab="PPG2")

bwplot(sal_type2 ~ PPG3, data=NBA, 
       panel=panel.bpplot, probs=seq(.01, .49, by=.01),
       datadensity=TRUE,
       ylab="sal_type", xlab="PPG3")

bwplot(sal_type2 ~ ADV3, data=NBA, 
       panel=panel.bpplot, probs=seq(.01, .49, by=.01),
       datadensity=TRUE,
       ylab="sal_type", xlab="ADV3")

bwplot(sal_type2 ~ ATRper3, data=NBA, 
       panel=panel.bpplot, probs=seq(.01, .49, by=.01),
       datadensity=TRUE,
       ylab="sal_type", xlab="ATRper3")

bwplot(sal_type2 ~ USGper3, data=NBA, 
       panel=panel.bpplot, probs=seq(.01, .49, by=.01),
       datadensity=TRUE,
       ylab="sal_type", xlab="USG_per3")

bwplot(sal_type2 ~ G2, data=NBA, 
       panel=panel.bpplot, probs=seq(.01, .49, by=.01),
       datadensity=TRUE,
       ylab="sal_type", xlab="G2")

histogram(~ PPG3 | sal_type2, data=NBA, 
          main="histogram of NBA Salary Brackets\n(PPG3 by sal_type)")

NBA.sal.table <- table(NBA_list$sal_type) / nrow(NBA_list)
bp <- barplot(NBA.sal.table, 
              ylim=c(0, 1),
              main = "Class Proportions in the\nNBA Data Set",
              xlab="Proportion",
              ylab="Class")
text(x = bp, y = NBA.sal.table, label = round(NBA.sal.table, digits=2), 
     pos = 3, cex = 0.8, col = "red")

# Nominal Model Creation

set.seed(1)
trainSet <- createDataPartition(NBA_list$sal_type, p=.6)[[1]]
NBA.train <- NBA_list[trainSet,]
NBA.test <- NBA_list[-trainSet,]

NBA.model.nom <- J48(sal_type ~ ., data=NBA.train)
summary(NBA.model.nom)

# Nominal Model Evaluation

NBA.predict.nom <- predict(NBA.model.nom, NBA.test)
NBA.eval.nom <- confusionMatrix(NBA.predict.nom, NBA.test$sal_type)
print(NBA.eval.nom)

# Regression Model Creation

NBA_list2 <- NBAsub2[, c(26, 19, 20, 23, 17, 7, 1)]

set.seed(1)
trainSet <- createDataPartition(NBA_list2$`Next cont`, p=.6)[[1]]
NBA.train <- NBA_list2[trainSet,]
NBA.test <- NBA_list2[-trainSet,]

NBA.model.reg <- rpart(`Next cont` ~., data = NBA_list2)
summary(NBA.model.reg)

plot(NBA.model.reg, uniform=TRUE,
     main="Regression Tree for NBA salaries")
text(NBA.model.reg, use.n=TRUE, all=TRUE, cex=.5)

printcp(NBA.model.reg)

# Regression Model Evaluation

NBA.predict.reg <- predict(NBA.model.reg, NBA.test)

plot(NBA.predict.reg, NBA.test$`Next cont`,
     main="Regression Tree Preditions vs. Actual",
     xlab="Predicted", ylab="Actual")
abline(0, 1, lty=2)
legend("topleft", c("Data", "Perfect Model"), pch=c(1, NA), lty=c(NA, 2))

## MSE
NBA.predict.reg.mse <- mean((NBA.predict.reg - NBA.test$`Next cont`)^2)
print(paste("Mean Squared Error (MSE):", NBA.predict.reg.mse))

## RMSE
NBA.predict.reg.rmse <- sqrt(NBA.predict.reg.mse)
print(paste("Root Mean Squared Error (RMSE)", NBA.predict.reg.rmse))

## MAE
NBA.predict.reg.mae <- mean(abs(NBA.predict.reg - NBA.test$`Next cont`))
print(paste("Mean Absolute Error (MAE):", NBA.predict.reg.mae))

# Automatic Pruning

NBA.model.reg.prune <- prune(NBA.model.reg, cp=0.05)
printcp(NBA.model.reg.prune)

# Rule set (RIPPER)

set.seed(1)
trainSet <- createDataPartition(NBA_list$sal_type, p=.6)[[1]]
NBA.train <- NBA_list[trainSet,]
NBA.test <- NBA_list[-trainSet,]

NBA.model.rules <- JRip(sal_type ~ ., data=NBA.train)
print(NBA.model.rules)
NBA.predict.rules <- predict(NBA.model.rules, NBA.test)

# Rule Set Evaluation

NBA.predict.rules <- predict(NBA.model.rules, NBA.test)
NBA.eval.rules <- confusionMatrix(NBA.predict.rules, NBA.test$sal_type)
print(NBA.eval.rules)

# 1R
## Binning

NBA.binned <- optbin(sal_type ~., NBA_list)

## Model creation

set.seed(0)
trainSet <- sample(seq_len(nrow(NBA.binned)), nrow(NBA.binned) * .6)
NBA.binned.train <- NBA.binned[trainSet,]
NBA.binned.test <- NBA.binned[-trainSet,]

NBA.oner.model <- OneR(NBA.binned.train, verbose = TRUE)

print(NBA.oner.model)
str(NBA.oner.model)
summary(NBA.oner.model)

# Evaluate 1R model

NBA.oner.pred <- predict(NBA.oner.model, NBA.binned.test)
eval_model(NBA.oner.pred, NBA.binned.test)

#Naive Bayes Model

set.seed(6)
trainSet <- sample(seq_len(nrow(NBA_list)), nrow(NBA_list) * .6)
NBA.train <- NBA_list[trainSet,]
NBA.test <- NBA_list[-trainSet,]

## Making sure the class split is the same for the training data

tbl.data <- table(NBA_list$sal_type) / nrow(NBA_list)
tbl.train <- table(NBA.train$sal_type) / nrow(NBA.train)
tbl.2 <- table(NBA$sal_type2) / nrow(NBA)
print(tbl.data)
print(tbl.train)

op <- par(mfrow=c(1,2))
bp <- barplot(tbl.data, 
              ylim=c(0, .7),
              main = "Class Proportions in the\nNBA Data Set",
              xlab="Proportion",
              ylab="Class")
text(x = bp, y = tbl.data, label = round(tbl.data, digits=2), 
     pos = 3, cex = 0.8, col = "red")

bp <- barplot(tbl.2, 
              ylim=c(0, .7),
              main = "Class Proportions",
              xlab="Proportion",
              ylab="Class")
text(x = bp, y = tbl.2, label = round(tbl.2, digits=2), 
     pos = 3, cex = 0.8, col = "red")

# Creating the model

NBA.nb.model <- naiveBayes(sal_type ~ .,data = NBA.train)
print(NBA.nb.model)
str(NBA.nb.model)

summary(NBA.nb.model)

# Evaluating the Model

NBA.nb.pred <- predict(NBA.nb.model, NBA.test)
eval_model(NBA.nb.pred, NBA.test$sal_type)

### Lab 6 ###

# Normalizing Descriptive Features
normFeature <- function(data) {
  (data - min(data)) / (max(data) - min(data))
}

NBA_list$PPG2_norm <- normFeature(NBA_list$PPG2)
NBA_list$PPG3_norm <- normFeature(NBA_list$PPG3)
NBA_list$ATRper3_norm <- normFeature(NBA_list$ATRper3)
NBA_list$USGper3_norm <- normFeature(NBA_list$USGper3)
NBA_list$G2_norm <- normFeature(NBA_list$G2)

# Splitting the Training and Testing data (60-40 split)
set.seed(0)
trainSet <- createDataPartition(NBA_list$sal_type, p=.6)[[1]]
NBA.train <- NBA[trainSet,]
NBA.test <- NBA[-trainSet,]

# Defining descriptive features for train and test. Defining target feature for training

NBA.knn.train <- NBA.train[, c(1,8,9,10,11,12)]
NBA.knn.test <- NBA.test[, c(1,8,9,10,11,12)]
NBA.traincl <- NBA.train$sal_type


# Creating predictions using 1, 3, and 10 nearest neighbors
NBA.pred.knn.1 <- knn(NBA.knn.train, NBA.knn.test, NBA.traincl)
NBA.pred.knn.3 <- knn(NBA.knn.train, NBA.knn.test, NBA.traincl, k=3)
NBA.pred.knn.10 <- knn(NBA.knn.train, NBA.knn.test, NBA.traincl, k=10)

# Calculation of performance for KNN classifiers

NBA.eval.knn.1 <- confusionMatrix(NBA.pred.knn.1, NBA.test$sal_type)
NBA.eval.knn.3 <- confusionMatrix(NBA.pred.knn.3, NBA.test$sal_type)
NBA.eval.knn.10 <- confusionMatrix(NBA.pred.knn.10, NBA.test$sal_type)

# Display the evaluation results for the KNN classifiers

print(NBA.eval.knn.1$table)
print(NBA.eval.knn.3$table)
print(NBA.eval.knn.10$table)

# Plotting decision boundaries for k 1-15
## Creating Simplified Data Set
NBA.id <- 1:nrow(NBA_list)
NBA.ADV3 <- NBA_list$ADV3
NBA.PPG3 <- NBA_list$PPG3_norm
NBA.first_option <- NBA$sal_type == "first_option"
NBA.simple <- data.frame(id = NBA.id, ADV3 = NBA.ADV3,
                         PPG3 = NBA.PPG3, first_option <- NBA.first_option)

test <- expand.grid(ADV3=seq(min(NBA.simple[,2]), max(NBA.simple[,2]),
                                by=0.02),
                    PPG3=seq(min(NBA.simple[,3]), max(NBA.simple[,3]), 
                               by=0.02))
test2 <- data.frame(test)

# Loop to use kNN on the simplified data frame 

for (k in 1:15) {
  # Predict the test points and calculate the probabilities
  NBA.pred.knn <- knn(NBA.simple[,2:3], test, NBA.simple[,4], k=k, prob=TRUE)
  
  # Obtain the probabilities from the prediction results
  prob <- attr(NBA.pred.knn , "prob")
  
  # Setup a data frame of class predictions (2-class)
  dataf <- bind_rows(
    mutate(test, prob=prob, cls="FALSE",
           prob_cls=ifelse(NBA.pred.knn==cls, 1, 0)),
    mutate(test, prob=prob, cls="TRUE",
           prob_cls=ifelse(NBA.pred.knn==cls, 1, 0)))
  
  # Plot the resulting test points, training points, and decision boundary
  thePlot <- ggplot(dataf) +
    geom_point(aes(x=ADV3, y=PPG3, col=first_option),
               data = mutate(test, first_option=NBA.pred.knn),
               size=1.2) + 
    geom_contour(aes(x=ADV3, y=PPG3, z=prob_cls, group=cls, color=cls),
                 bins=2,
                 data=dataf) +
    geom_point(aes(x=ADV3, y=PPG3, col=cls),
               size=3,
               data=data.frame(ADV3=NBA.simple[,2], PPG3=NBA.simple[,3], cls=NBA.first_option)) +
    ggtitle(paste("First Option kNN Model (k=", k, ") with Decision Boundary", sep="")) +
    theme(plot.title = element_text(face="bold", size=14, hjust=0.5)) +
    labs(x="Advanced Statistic (normalized)", y="Points per Game (normalized)")
  
  # Show the plot
  print(thePlot)
  
  # Save the plot to a file
  filename <- paste("Lab9.knn", k, ".decisionboundary.png", sep="")
  png(filename, width=500, height=500)
  print(thePlot)
  dev.off()
  
  # Plot the resulting test points, training points, and decision boundary
  # Represent the point classification proabaility with point size
  thePlot <- ggplot(dataf) +
    geom_point(aes(x=ADV3, y=PPG3, col=first_option, size=prob),
               data = mutate(test, first_option=NBA.pred.knn)) + 
    scale_size(range=c(0.8, 2)) +
    geom_contour(aes(x=ADV3, y=PPG3, z=prob_cls, group=cls, color=cls),
                 bins=2,
                 data=dataf) +
    geom_point(aes(x=ADV3, y=PPG3, col=cls),
               size=3,
               data=data.frame(ADV3=NBA.simple[,2], PPG3=NBA.simple[,3], cls=NBA.first_option)) +
    geom_point(aes(x=ADV3, y=PPG3),
               size=3, shape=1,
               data=data.frame(ADV3=NBA.simple[,2], PPG3=NBA.simple[,3], cls=NBA.first_option)) +
    ggtitle(paste("First Option kNN Model (k=", k, ")\nwith Decision Boundary and Probability",
                  sep="")) +
    theme(plot.title = element_text(face="bold", size=14, hjust=0.5)) +
    labs(x="Advanced Statistic (normalized)", y="Points per Game (normalized)")
  
  # Show the plot
  print(thePlot)
  
  # Save the plot to a file
  filename <- paste("Lab9.knn", k, ".decisionboundaryprob.png", sep="")
  png(filename, width=500, height=500)
  print(thePlot)
  dev.off()
} # end for loop for generating knn decision boundary plots


### Lab 6 P2 ###


# partitioning the data
set.seed(0)
trainSet <- createDataPartition(NBA_list$sal_type, p=.6)[[1]]
NBA.list.train <- NBA_list[trainSet,]
NBA.list.test <- NBA_list[-trainSet,]

# plotting linearly separable linear regression (logistic model)

NBAglm <- glm(sal_type ~ ., NBA.list.train, 
              family = binomial(link = "logit"))

slope <- coef(NBAglm)[2]/(-coef(NBAglm)[3])
intercept <- coef(NBAglm)[1]/(-coef(NBAglm)[3]) 
plot(NBA.disc.train[,c(1,3)], pch=as.numeric(NBA.disc.train$sal_type))
abline(intercept, slope)

# Creating an SVM model

NBAsvm <- svm(sal_type ~ ., data = NBA.list.train)


plot(NBAsvm, NBA.list.train, PPG3~ADV3)


#Predict Logistic

NBAglm_pred <- predict(NBAglm,
                       NBA.list.test, type = 'response')

NBAglm_pred <- factor(NBAglm_pred, levels = c('first_option', 'second_option', 'role_player', 'minimum'))
NBAglm_conMat <- 
  confusionMatrix(as.factor(NBAglm_pred),
                  as.factor(as.numeric(NBA.list.test$sal_type)))
print(NBAglm_conMat$table)



iris.disc.pred.logistic <- predict(iris.disc.model.logistic, 
                                   iris.disc.test[,3:5], type="response")
iris.disc.eval.logistic.conMat <- 
  confusionMatrix(as.factor(round(iris.disc.pred.logistic + 1)), 
                  as.factor(as.numeric(iris.disc.test$species)))
print(iris.disc.eval.logistic.conMat$table)

# Predict SVM

NBAsvm_eval <- predict(NBAsvm, NBA.list.test[,1:6])
NBAsvm_conMat <- confusionMatrix(
  NBAsvm_eval, NBA.list.test$sal_type)
print(NBAsvm_conMat$table)






