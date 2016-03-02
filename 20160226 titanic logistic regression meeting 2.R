#decision tree
install.packages(rpart)
library(rpart)
#cforest 
install.packages('party')
library(party)
#randomForest
install.packages('randomForest')
library(randomForest)

#more efficiency 
library(readr)
#for visualisation purpose of missing value pattern#
library(VIM)
#for impute missing value purpose 
library(mice)
#for count frequency purpose
install.packages("plyr")
library(plyr)

options( contrasts = c( "contr.treatment",
                        "contr.poly" ))

set.seed(888)

# na.strings=c("","NA"), impute blank cell as NA 
train<- read.csv("train.csv",sep=",", na.strings=c("","NA"))

test<- read.csv("test.csv",sep=",",na.strings=c("","NA"))
contrasts(train$Sex)
#data massage 
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Name <- as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
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

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

train <- combi[1:891,]
test <- combi[892:1309,]

#to get column index of variable
grep("Ticket", colnames(train))
grep("Name", colnames(train))
grep("Cabin", colnames(train))
grep("Surname", colnames(train))
grep("FamilyID", colnames(train))
#delete variables by index use -
train<-train[,-c(4,9,11,15,16)]
#way 1 of imputation of missing value
#plot pattern of missing value
train_aggr = aggr(train, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                  labels=names(train), cex.axis=.7, gap=3, 
                  ylab=c("Proportion of missingness","Missingness Pattern"))

?mice
train <- mice(train,m=8,maxit=25,meth='cart',seed=888,printFlag=TRUE)

train<- complete(train,1)

train$Sexchild<-ifelse (train$Age<=14 ,"Child",
             ifelse(train$Sex=="female" & train$Age>14,"women","men" ))


#test

test_aggr = aggr(test, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                 labels=names(test), cex.axis=.7, gap=3, 
                 ylab=c("Proportion of missingness","Missingness Pattern"))
#to get column index of variable
grep("Survived", colnames(test))
grep("Ticket", colnames(test))
grep("Name", colnames(test))
grep("Cabin", colnames(test))
grep("Surname", colnames(test))
grep("FamilyID", colnames(test))
test<-test[,-c(2,4,7,11,15,16)]
test <- mice(test,m=8,maxit=3,meth='cart',seed=888)
test<- complete(test,1)

test$Sexchild<-ifelse (test$Age<=14 ,"Child",
                        ifelse(test$Sex=="female" & test$Age>14,"women","men" ))

test$Sexchild<-as.factor(test$Sexchild)
#way 2 of imputation of missing value
random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=F)
  return (imputed)
}
train$Age=random.imp(train$Age)
test$Age=random.imp(test$Age)
test$Fare=random.imp(test$Fare)


#model 1 rpart

fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Title+
             FamilyID2,data=train, method="class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit_rpart <- data.frame(PassengerId = test$PassengerId, Survived=Prediction)
count(submit_rpart,"Survived")
write.csv(submit_rpart, file = "submission11_rpart.csv", row.names = FALSE)

#model 2 randomforest 
train$Survived<-as.factor(train$Survived)
fit2 <- randomForest(Survived ~Pclass + Sex + Age + Fare + Title+Pclass*Sex+ FamilyID2, data=train, importance=TRUE, ntree=1500)
varImpPlot(fit2)

Prediction2 <- predict(fit2, test)
submit_rforest<-data.frame(PassengerId = test$PassengerId, Survived=Prediction2)

count(submit_rforest,"Survived")
write.csv(submit_rforest, file = "submission12_rforest.csv", row.names = FALSE)

#model 3 cforest
set.seed(888)
train$Sexchild<-as.factor(Sexchild)
fit3<- cforest(as.factor(Survived) ~ Pclass+Age+FamilyID2+FamilySize+Title+Sexchild+Fare+Pclass*Fare+Pclass*Title+Title*Sexchild+Pclass*FamilyID2+Pclass*Sexchild+Pclass*Age,data = train, controls=cforest_unbiased(ntree=1000))
# standard importance
varimp(fit3)
# the same modulo random variation
varimp(fit3, pre1.0_0 = TRUE)

# conditional importance, may take a while...
varimp(fit3, conditional = TRUE)
#https://journal.r-project.org/archive/2009-2/RJournal_2009-2_Strobl~et~al.pdf
Prediction3 <- predict(fit3, test, OOB=TRUE, type = "response")

submit_cforest<-data.frame(PassengerId = test$PassengerId, Survived=Prediction3)
?cforest
count(submit_cforest,"Survived")
write.csv(submit_cforest, file = "submission29_cforest.csv", row.names = FALSE)
submit_cforestI<-data.frame(test, Survived=Prediction3)
write.csv(submit_cforestI, file = "submission23_cforestI.csv", row.names = FALSE)
#submission23_cforest：0.818.. corresponding to kaggle sub22

#submission22_cforest：0.81383.. corresponding to kaggle sub20

#model 4 Bagged CART
library(mlbench)
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
?trainControl
seed <- 7
metric <- "Accuracy"

fit.treebag <- train(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title+Pclass*Sex+ 
                       FamilyID2, data=train, method="treebag", metric=metric, trControl=control)
Prediction4=predict(fit.treebag, test)
submit_tbag<-data.frame(PassengerId = test$PassengerId, Survived=Prediction4)
count(submit_tbag,"Survived")
write.csv(submit_cforest, file = "submission14_tbag.csv", row.names = FALSE)




fit.gbm <- train(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title+Pclass*Sex+ 
                   FamilyID2, data=train,  method="gbm", metric=metric, trControl=control, verbose=FALSE)
Prediction5=predict(fit.gbm, test)
submit_gbm<-data.frame(PassengerId = test$PassengerId, Survived=Prediction5)
count(submit_gbm,"Survived")
write.csv(submit_gbm, file = "submission15_gbm.csv", row.names = FALSE)
