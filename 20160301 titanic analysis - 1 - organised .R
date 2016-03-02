
#for count function 
install.packages('plyr')
library('plyr')
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

missing <- is.na(Datalogistic$Age)
n.missing <- sum(missing)
a.obs <-Datalogistic$Age[!missing]
imputed <- Datalogistic$Age
imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)

Datalogistic$Age=random.imp(Datalogistic$Age)
#after imputation of hist age
hist(Datalogistic$Age)
# below modelling , all dont consider interaction between variables (features) only consider main effect
#the best model before model diagnosis is pclass+sex+Age+sibsp based on likehood ratio test
# prove the method use LRT: http://www.stat.columbia.edu/~martin/W2024/R11.pdf
# only one line coding to obtain the best model, the result is the same as the first way. 
dxpplk;
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


#without sex, seems fit better 
BestMdiagno2<-lrm(Survived~1+Pclass+Age+SibSp, x=TRUE, y=TRUE, data=Datalogistic)

BestMdiagno2

cal2<-calibrate(BestMdiagno2, B=500)

plot(cal2)
#adjusted residual check 
att.h <- lm.influence(bestmodelAIC)$hat
att.h
att.bestmodel <- residuals(bestmodelAIC, type="pearson")/sqrt( 1-att.h )
plot(att.bestmodel)
abline(h=c(-1.96,1.96))

#load test dataset 

testlogistic<- read.csv("test.csv",sep=",")
hist(testlogistic$Age)
#impute missing value 
testlogistic$Age=random.imp(testlogistic$Age)
hist(testlogistic$Age)
#prediction 
pred = data.frame(predict(bestmodelAIC, newdata=testlogistic,type="response"))
testlogisticP<-data.frame(testlogistic,pred)
#define survived and non-survived
testlogisticP$Survived<-ifelse(testlogisticP$predict.bestmodelAIC..newdata...testlogistic..type....response..>=0.6,1,0)
#count of survived and non-survived
count(testlogisticP,"Survived")
#for submission only keep ID and survived variables 
submission<-testlogisticP[,c(1,13)]
write.csv(submission,"submissiongood.csv")
#for keep all variables including survived variable
write.csv(testlogisticP,"testlogisticP.csv")


#####################################################

#K-Fold Cross Validation(http://www.r-bloggers.com/evaluating-logistic-regression-models/)
#for trainControl
library(caret)
Datalogistic$Pclass<-as.factor(Datalogistic$Pclass)
testlogistic$Pclass<-as.factor(testlogistic$Pclass)
ctrl3 <- trainControl(method = "cv", number = 20,p=0.8, savePredictions = TRUE,trim=TRUE, selectionFunction = "best")
Datalogistic$Survived<-as.factor(Datalogistic$Survived)
mod_fit3 <- train(Survived~1+.,  data=Datalogistic, method="glm", family="binomial",
                  trControl = ctrl3, tuneLength = 25)
#use train dataset to produce prediction for self validation according to confusionMatrix function
#which produce sensitivity and specificity for false positive and negaitve etc ...to know how good is the model
predtrain3= predict(mod_fit3,newdata=Datalogistic)
confusionMatrix(data=predtrain3, Datalogistic$Survived)
#to produce prediction of test dataset 
pred2 = predict(mod_fit3, newdata=testlogistic)


