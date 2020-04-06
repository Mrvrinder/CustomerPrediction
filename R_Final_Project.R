## Read Training and Test Data

trainData <- read.csv("trainset.csv")
testData <- read.csv("testset.csv")
View(trainData)
View(testData)

## Install Packages

install.packages("ggplot2")
install.packages("partykit")
install.packages("RWeka")
install.packages("caret")
install.packages("ROCR")

## Call Libraries

library(ggplot2)
library(partykit)
library(RWeka)
library(caret)
library(ROCR)

## Data Exploration...decide classes

classes <- data.frame(age = class(trainData$age),
                 job = class(trainData$job),
                marital = class(trainData$marital),
                education = class(trainData$education),
                housing = class(trainData$housing),
                loan = class(trainData$loan),
                contact = class(trainData$contact),
                month = class(trainData$month),
                day_of_week = class(trainData$day_of_week),
                duration = class(trainData$duration),
                campaign = class(trainData$campaign),
                pdays = class(trainData$pdays),
                poutcome = class(trainData$poutcome),
                nr.employed = class(trainData$nr.employed),
                Subscribed = class(trainData$Subscribed),
                stringsAsFactors = FALSE)


## Plot Numeric Classes

pl1 <- ggplot(trainData, aes(age))
pl1 + geom_density(fill = "red", alpha = "0.7")

pl2 <- ggplot(trainData, aes(duration))
pl2 + geom_density(fill = "green", alpha = "0.7")

pl3 <- ggplot(trainData, aes(campaign))
pl3 + geom_density(fill = "blue", alpha = "0.7")

pl4 <- ggplot(trainData, aes(pdays))
pl4 + geom_density(fill = "yellow", alpha = "0.7")

pl5 <- ggplot(trainData, aes(nr.employed))
pl5 + geom_density(fill = "purple", alpha = "0.7")

## Identify Special Cases for Numeric Classes (campaign, pdays, nr.employed)

##campaign -- possible sentinel values -- 42 values but many are outliers
summary(trainData$campaign)
campaign_values <- as.data.frame(table(trainData$campaign))

##pdays -- possible sentinel values -- no significant values other than 999
summary(trainData$pdays)
pdays_values <-as.data.frame(table(trainData$pdays))

##nr.employed -- does not look continuous-- 7 values -- 5176.3 = 1 instance , 5017.5 = 171 instances
summary(trainData$nr.employed)
nr.employed_values <- as.data.frame(table(trainData$nr.employed))

## Data Exploration for Factors

##job -- 12 values -- unknown = 260 , student = 313

job_values <- as.data.frame(table(trainData$job))
job_values

##marital -- 4 values -- unknown = 51

marital_values <- as.data.frame(table(trainData$marital))
marital_values

##education -- 8 values -- unknown = 1227 , illiterate = 15

education_values <- as.data.frame(table(trainData$education))
education_values

##housing -- 3 values -- unknown = 713

housing_values <- as.data.frame(table(trainData$housing))
housing_values

##loan -- 3 values -- unknown = 713

loan_values <- as.data.frame(table(trainData$loan))
loan_values

##contact -- 2 values

contact_values <- as.data.frame(table(trainData$contact))
contact_values

##month -- 10 values -- dec = 1 , sep = 106 , mar = 126 , oct = 132 , apr = 442

month_values <- as.data.frame(table(trainData$month))
month_values

##day_of_week -- 5 values

day_of_week_values <- as.data.frame(table(trainData$day_of_week))
day_of_week_values

##poutcome -- 3 values -- success = 299

poutcome_values <- as.data.frame(table(trainData$poutcome))
poutcome_values

##Subscribed -- 2 values -- no = 26075 , yes = 3196

subscribed_values <- as.data.frame(table(trainData$Subscribed))
subscribed_values

##Info Gain Before Data Cleanup

IG_pre_cleanup <- sort(InfoGainAttributeEval(Subscribed ~ . , data = trainData), decreasing = TRUE)
barplot(IG_pre_cleanup , las=2)

## Cleaning the Data

  cleanedData <- trainData
cleanedData$nr.employed[cleanedData$nr.employed == "5176.3"] <- NA
cleanedData$nr.employed[cleanedData$nr.employed == "5017.5"] <- NA
  cleanedData$job[cleanedData$job == "unknown"] <- NA
  cleanedData$job[cleanedData$job == "student"] <- NA
cleanedData$marital[cleanedData$marital == "unknown"] <- NA
  cleanedData$education[cleanedData$education == "unknown"] <- NA
  cleanedData$education[cleanedData$education == "illiterate"] <- NA
cleanedData$housing[cleanedData$housing == "unknown"] <- NA
  cleanedData$loan[cleanedData$loan == "unknown"] <- NA
cleanedData$month[cleanedData$month == "dec"] <- NA
cleanedData$month[cleanedData$month == "sep"] <- NA
cleanedData$month[cleanedData$month == "mar"] <- NA
cleanedData$month[cleanedData$month == "oct"] <- NA
cleanedData$month[cleanedData$month == "apr"] <- NA
  cleanedData$poutcome[cleanedData$poutcome == "success"] <- NA
cleanedData$pdays[cleanedData$pdays == 999] <- NA
  
## set nr.employed as factor

cleanedData$nr.employed <- as.factor(cleanedData$nr.employed)

## Clean Numerical/Integer Data

regression_train_cleaned <- cleanedData
regression_test_cleaned <- cleanedTest



## Clean testData

cleanedTest <- testData
cleanedTest$nr.employed <- as.factor(cleanedTest$nr.employed)

## Check cleaned data for removed values

nr.employed_clean <- as.data.frame(table(cleanedData$nr.employed))
job_clean <- as.data.frame(table(cleanedData$job))
marital_clean <- as.data.frame(table(cleanedData$marital))
education_clean <- as.data.frame(table(cleanedData$education))
housing_clean <- as.data.frame(table(cleanedData$housing))
loan_clean <- as.data.frame(table(cleanedData$loan))
month_clean <- as.data.frame(table(cleanedData$month))
poutcome_clean <- as.data.frame(table(cleanedData$poutcome))

nr.employed_clean
job_clean
marital_clean
education_clean
housing_clean
loan_clean
month_clean
poutcome_clean

## Info Gain Cleaned

IG_cleaned <- sort(InfoGainAttributeEval(Subscribed ~ . , na.action = na.pass , data = cleanedData) , decreasing = TRUE)
barplot(IG_cleaned , las=2)


## Formulae

formula1 <- Subscribed ~ nr.employed + duration
formula2 <- Subscribed ~ nr.employed + duration + pdays
formula3 <- Subscribed ~ job + campaign + marital + day_of_week + loan
formula4 <- Subscribed ~ .

## Ctree Model post cleanup

## Ctree 1

CTree1 <- ctree(formula = formula1, data = cleanedData)
plot(CTree1)
testCTree1 <- predict(CTree1, newdata=cleanedTest)
table(testCTree1, cleanedTest$Subscribed)
confMat1 <- table(testCTree1,cleanedTest$Subscribed)
accuracy1 <- sum(diag(confMat1))/sum(confMat1)

#Ctree 2

CTree2 <- ctree(formula = formula2, na.action = na.pass, data = cleanedData)
plot(CTree2)
testCTree2 <- predict(CTree2, newdata=cleanedTest)
table(testCTree2, cleanedTest$Subscribed)
confMat2 <- table(testCTree2,cleanedTest$Subscribed)
accuracy2 <- sum(diag(confMat2))/sum(confMat2)

#Ctree 3 -- what a meme it just said everything was no and was right -- this is actually a bad model

CTree3 <- ctree(formula = formula3, na.action = na.exclude, data = cleanedData)
plot(CTree3)
testCTree3 <- predict(CTree3, newdata=cleanedTest)
table(testCTree3, cleanedTest$Subscribed)
confMat3 <- table(testCTree3,cleanedTest$Subscribed)
accuracy3 <- sum(diag(confMat3))/sum(confMat3)

#Ctree 4

CTree4 <- ctree(formula = formula4, na.action = na.omit, data = cleanedData)
plot(CTree4)
testCTree4 <- predict(CTree4, newdata = cleanedTest)
table(testCTree4, cleanedTest$Subscribed)
confMat4 <- table(testCTree4, cleanedTest$Subscribed)
accuracy4 <- sum(diag(confMat4))/sum(confMat4)

#J48Tree_1 ***not working error with csv encoding

j48_1 <- J48(formula = formula4, na.action = na.pass, data = cleanedData)
plot(j48_1)
testj48_1 <- predict(j48_1, newdata = cleanedTest)
table(testj48_1, cleanedTest$Subscribed)
confMat5 <- table(testj48_1, cleanedTest$Subscribed)
accuracy5 <- sum(diag(confMat5))/sum(confMat5)

