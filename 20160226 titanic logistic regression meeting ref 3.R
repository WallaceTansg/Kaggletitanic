library(data.table)

train<-data.table(read.csv("train.csv"))
test<-data.table(read.csv("test.csv"))

library(rpart)
library(rattle)

library(rpart.plot)
library(RColorBrewer)
#way1
fit <- rpart(Survived ~ Pclass + Sex + Age + Fare, data=train, method="class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

count(submit,"Survived")
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)


#way 2 : Improvment

test2<-test
test2$Survived<-NA
all_data<-rbind(test2,train)
all_data$Name<-as.character(all_data$Name)
all_data$Title <- sapply(all_data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
all_data$Title <- sub(' ', '', all_data$Title)

#Reduce Complexity
all_data$Title[all_data$Title %in% c('Mme', 'Mlle','Ms')] <- 'Mrs'
all_data$Title[all_data$Title %in% c('Capt', 'Don', 'Major', 'Sir','Col','Dr','Rev')] <- 'Sir'
all_data$Title[all_data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
all_data$Title <-as.factor(all_data$Title)

#Calculate age/fare within titles and replace missing values
AverageAge<-data.table(aggregate(Age ~ Title, data = all_data, median))
names(AverageAge)<-c("Title","AverageAge")

AverageFare<-data.table(aggregate(Fare ~ Title, data = all_data, median))
names(AverageFare)<-c("Title","AverageFare")

all_data<-merge(all_data,AverageAge,by="Title")
all_data<-merge(all_data,AverageFare,by="Title")
all_data$Age<-mapply(all_data$Age,all_data$AverageAge, FUN=function(x,y) {if(is.na(x)){y} else{x}})
all_data$Fare<-mapply(all_data$Fare,all_data$AverageFare, FUN=function(x,y) {if(is.na(x)){y} else{x}})

#Split data back into Test/Train
all_data<-all_data[order(PassengerId)]
train<-all_data[1:891,]
test<-all_data[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Title, data=train, method="class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myseconddtree.csv", row.names = FALSE)

set.seed(69)
library(randomForest)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

#way 3

library(party)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ConditionalInference.csv", row.names = FALSE)
