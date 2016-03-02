
#Package
 
library(MASS)

#for uploading glm , step, stepAIC etc functions
install.packages("rms",repos="http://cran.us.r-project.org")
library(rms)

#for uploading rml function for concordance index-C and bootstrap calibration etc

#read data 

Datalogistic<- read.csv("train.csv",sep=",")

#Model Selection

#remove not meaningful variables(features) for modelling and missing observations 

Datalogistic$Name<-NULL
Datalogistic$Ticket<-NULL
Datalogistic$Cabin<-NULL
Datalogistic$PassengerId<-NULL
#investigate the distribution of Age with missing value
hist(Datalogistic$Age)
#imputation of missing values 
#http://www.stat.columbia.edu/~gelman/arm/missing.pdf
random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

Datalogistic$Age=random.imp(Datalogistic$Age)
#after imputation of hist age
hist(Datalogistic$Age)
# below modelling , all dont consider interaction between variables (features) only consider main effect
#the best model before model diagnosis is pclass+sex+Age+sibsp based on likehood ratio test
# prove the method use LRT: http://www.stat.columbia.edu/~martin/W2024/R11.pdf
# only one line coding to obtain the best model, the result is the same as the first way. 
Fullmodel<-glm(Survived~1+., family=binomial(link = "logit"), data=Datalogistic)
summary(Fullmodel)

#AIC and BIC index model selection approach
#the best model before model diagnosis is also pclass+sex+Age+sibsp based on AIC and BIC criterion
bestmodelAIC<-step(glm(Survived~1,family=binomial, data=Datalogistic),scope=list(lower=~1,upper=~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked), direction="both", criterion = "AIC")

summary(bestmodelAIC)

bestmodelBIC<-step(glm(Survived~1,family=binomial, data=Datalogistic),scope=list(lower=~1,upper=~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked), direction="both",criterion = "BIC")

summary(bestmodelBIC)

#Model Diagnosis
#with sex
BestMdiagno<-lrm(Survived~1+Sex+Pclass+Age+SibSp, x=TRUE, y=TRUE, data=Datalogistic)

BestMdiagno

cal<-calibrate(BestMdiagno, B=500)

plot(cal)


#without sex
BestMdiagno2<-lrm(Survived~1+Pclass+Age+SibSp, x=TRUE, y=TRUE, data=Datalogistic)

BestMdiagno2

cal2<-calibrate(BestMdiagno2, B=500)

plot(cal2)
#adjusted residual 
att.h <- lm.influence(bestmodelAIC)$hat
att.h
att.bestmodel <- residuals(bestmodelAIC, type="pearson")/sqrt( 1-att.h )
plot(att.bestmodel)
abline(h=c(-1.96,1.96))

#
is.factor(testlogistic$Pclass)
testlogistic<- read.csv("test.csv",sep=",")
hist(testlogistic$Age)

testlogistic$Age=random.imp(testlogistic$Age)
hist(testlogistic$Age)

pred = data.frame(predict(bestmodelAIC, newdata=testlogistic,type="response"))
testlogisticP<-data.frame(testlogistic,pred)
#0.5 abt 3200 rank ,  0.6 2200,
testlogisticP$Survived<-ifelse(testlogisticP$predict.bestmodelAIC..newdata...testlogistic..type....response..>=0.65,1,0)

submission<-testlogisticP[,c(1,13)]
write.csv(submission,"submission3.csv")
write.csv(testlogisticP,"testlogisticP.csv")

###################################################
testlogistic$Pclass<-as.factor(testlogistic$Pclass)
testlogistic$SibSp<-as.factor(testlogistic$SibSp) 
testlogistic$Suvived<-as.factor(testlogistic$Survived) 
#####################################################

#K-Fold Cross Validation(http://www.r-bloggers.com/evaluating-logistic-regression-models/)

testlogistic<- read.csv("test.csv",sep=",")
testlogistic<-testlogistic[,c(1,2,4,5,6)]
testlogistic<-na.omit(testlogistic)

library(caret)
ctrl <- trainControl(method = "boot", number = 20, savePredictions = TRUE)

testlogistic$survived<-as.factor(testlogistic$survived)
mod_fit <- train(Survived~1+Pclass+Age+SibSp,  data=testlogistic, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit, newdata=testlogistic)
confusionMatrix(data=pred, testlogistic$survived)


