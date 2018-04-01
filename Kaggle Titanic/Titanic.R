# Setting Working Directory
setwd("~/Personal/Data Science Projects/Titanic")

# Importing Training and test data
train <- read.csv("~/Personal/Data Science Projects/Titanic/train.csv")
test <- read.csv("~/Personal/Data Science Projects/Titanic/test.csv")

# Inspect the data set
str(train)

# Count of Survived
table(train$Survived)

# Proportion of Survived
prop.table(table(train$Survived))

# Adding Survived column to test data
test$Survived <- rep(0,418)

table(train$Sex)

prop.table(table(train$Sex, train$Survived),1)

# Making a prediction based on Sex
test$Survived[test$Sex == 'female'] <- 1

# Analyzing Age Variable
summary(train$Age)

train$Age[is.na(train$Age)] <- mean(train$Age, na.rm = T)

train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data = train, FUN = sum)

aggregate(Survived ~ Child + Sex, data = train, FUN = length)

aggregate(Survived ~ Child + Sex, data = train, FUN = function(x){sum(x)/length(x)})
# or 
aggregate(Survived ~ Child + Sex, data = train, FUN = mean)

# Analyzing Fare variable
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = mean)

# Make a new prediction based on Fare and class
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0


# Decision Tree
library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")

# Draw the tree
plot(fit)
text(fit)

# Load other libraries for plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")

table(Prediction)

# Feature Engineering
test$Survived <- NA
combi <- rbind(train,test)

combi$Name <- as.character(combi$Name)
strsplit(combi$Name[1], split = '[,.]')

# Fetching the Title
combi$Title <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'

combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

train <- combi[1:891,]
test <- combi[892:1309,]

# Making Random Forest
library(randomForest)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare +
                      Embarked + Title + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=400)

Prediction <- predict(fit, test)


# Making new feature engineering
combi$Cabin1 <- substr(combi$Cabin,1,1)

combi$Ticket1 <- substr(as.character(combi$Ticket),1,1)
combi$Ticket1 <- factor(combi$Ticket1)
train <- combi[1:891,]
test <- combi[892:1309,]

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare +
                      Embarked + Title + FamilyID2 + Ticket1,
                    data=train, 
                    importance=TRUE, 
                    ntree=400)

Prediction <- predict(fit, test)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare +
                       Embarked + Title + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree= 400)

table(Prediction)

# Final One
library(party)

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age +  Fare +
                 Embarked + Title + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

# Making SUbmission data frame
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "submit_final.csv", row.names = F)