# Read data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
str(train)              
Survived <- train$Survived
train <- subset(train, select = -(Survived))
# Merge train and test data
data <- rbind(train,test)
# Take name variable out
data <- subset(data, select = -(Name))
summary(data)
str(data)
# Transform variable
data$Pclass <- as.factor(data$Pclass)
data$SibSp <- as.factor(data$SibSp)
data$Parch <- as.factor(data$Parch)
str(data)
data$Ticket <- as.character(data$Ticket)
data$Cabin <- as.character(data$Cabin)
str(data)
# Imputation to data
library(mice)
temData <- mice(data, m=5, maxit = 10)
fullData <- complete(temData)
train <- fullData[1:891,]
test <- fullData[892:1309,]
train <- cbind(train, Survived)
train$Survived <- as.factor(train$Survived)
# Random Forest
library(randomForest)
RF = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Pclass:Sex + Pclass:Age + Age:Sex, data = train, ntree = 20000, nodesize = 200)
pred = predict(RF, newdata = test)
MySubmission = data.frame(PassengerId = test$PassengerId, Survived = pred)
write.csv(MySubmission, "RF.csv", row.names =FALSE)
# 0.77512, rank 2886
# extracts passengers titles
train1 <- read.csv("train.csv")
Name <- train1$Name
train <- cbind(Name, train)
rm(train1)
test1 <- read.csv("test.csv")
Name <- test1$Name
test <- cbind(test, Name)
rm(test1)
train <- subset(train, select = (-Survived))
fullData <- rbind(train, test)
fullData$Title <- gsub('(. * ,)|(\\..*)', '', fullData$Name)
table(fullData$Sex, fullData$Title)
str(fullData)
library(stringr)
fullData$title <- str_sub(fullData$Name, str_locate(fullData$Name, ",")[ , 1] + 2, str_locate(fullData$Name, "\\.")[ , 1] - 1)
# Another way extract title: strsplit(combi$Name[1], split='[,.]')[[1]][2]
# combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
# combi$Title <- sub(' ', '', combi$Title)
# http://trevorstephens.com/kaggle-titanic-tutorial/r-part-4-feature-engineering/
fullData <- subset(fullData, select =(-Title))
table(fullData$title, fullData$Sex)
male_noble_names <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Rev", "Sir")
fullData$title[fullData$title %in% male_noble_names] <- "male_noble"
female_noble_names <- c("Lady", "Mlle", "Mme", "Ms", "the Countess")
fullData$title[fullData$title %in% female_noble_names] <- "female_noble"
str(fullData)
fullData$title <- as.factor(fullData$title)
# Randomforest model 1
train <- fullData[1:891,]
test <- fullData[892:1309,]
train <- cbind(train, Survived)
train$Survived <- as.factor(train$Survived)
RF1 = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title, data = train, ntree = 20000)
pred1 = predict(RF1, newdata = test)
MySubmission = data.frame(PassengerId = test$PassengerId, Survived = pred1)
write.csv(MySubmission, "RF1.csv", row.names =FALSE)
# ACC = 0.79426 rank = 1347
# Tree model
library(caret)
library(e1071)
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.002, 0.01, 0.002))
train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
library(rpart)
library(rpart.plot)
rpart <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title, data = train, method = "class", cp = 0.002)
prp(rpart)
pred2 <- predict(rpart, newdata = test, type = "class") 
MySubmission = data.frame(PassengerId = test$PassengerId, Survived = pred2)
write.csv(MySubmission, "RPART.csv", row.names =FALSE)
# ACC = 0.77
# Extraxt Surname
fullData$Name <- as.character(fullData$Name)
strsplit(fullData$Name, split = '[,.]')[[1]][1]
fullData$Surname <- sapply(fullData$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})
# Alone or Family
fullData$SibSp <- as.numeric(fullData$SibSp)
fullData$Parch <- as.numeric(fullData$Parch)
fullData$Fsize <- (fullData$SibSp -1) + (fullData$Parch-1) + 1
fullData$Family <- paste(fullData$Surname, fullData$Fsize, sep = '_')
# Discretize family size
fullData$FsizeD[fullData$Fsize == 1] <- 'single'
fullData$FsizeD[fullData$Fsize > 1 && fullData$Fsize <= 4] <- 'small'
fullData$FsizeD[fullData$Fsize >=5 ] <- 'large'
# Random forest
str(fullData)
fullData$Fsized <- as.factor(fullData$FsizeD)
train <- fullData[1:891,]
test <- fullData[892:1309,]
train <- cbind(train, Survived)
train$Survived <- as.factor(train$Survived)
RF2 = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + Fsized, data = train, ntree = 20000)
pred2 = predict(RF2, newdata = test)
MySubmission = data.frame(PassengerId = test$PassengerId, Survived = pred2)
write.csv(MySubmission, "RF2.csv", row.names =FALSE)
# Variable Important
importantce <- importance(RF2)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"],2))
