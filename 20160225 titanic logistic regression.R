
#Package
 
library(MASS)

#for uploading glm , step, stepAIC etc functions
install.packages("rms",repos="http://cran.us.r-project.org")
library(rms)

#for uploading rml function for concordance index-C and bootstrap calibration etc

#read data 

Datalogistic<- read.csv("train.csv",sep=",")

#Default contrast in R 

options( contrasts = c( "contr.treatment", "contr.poly" ))

#Model Selection

#remove not meaningful variables(features) for modelling and missing observations 

Datalogistic$Name<-NULL
Datalogistic$Ticket<-NULL
Datalogistic$Cabin<-NULL
Datalogistic$PassengerId<-NULL
Datalogistic<-na.omit(Datalogistic)

Fullmodel<-glm(Survived~1+., family=binomial, data=Datalogistic)
summary(Fullmodel)


# only one line coding to obtain the best model, the result is the same as the first way. the best model is Oppy+Country
#AIC index model selection approach

bestmodelAIC<-step(glm(Survived~-1,family=binomial, data=Datalogistic),scope=list(lower=~-1,upper=~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked), direction="both", criterion = "AIC")
bestmodelBIC<-step(glm(Survived~-1,family=binomial, data=Datalogistic),scope=list(lower=~-1,upper=~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked), direction="both",criterion = "BIC")



summary(bestmodel)
# the best model according to AIC index is also Oppy+Country 


#Model Diagnosis
#with sex
BestMdiagno<-lrm(Survived~-1+Sex+Pclass+Age+SibSp, x=TRUE, y=TRUE, data=Datalogistic)

BestMdiagno

cal<-calibrate(BestMdiagno, B=500)

plot(cal)


#without sex
BestMdiagno2<-lrm(Survived~-1+Pclass+Age+SibSp, x=TRUE, y=TRUE, data=Datalogistic)

BestMdiagno2

cal2<-calibrate(BestMdiagno2, B=500)

plot(cal2)
#adjusted residual 
att.h <- lm.influence(bestmodel)$hat
att.h
att.bestmodel <- residuals(bestmodel, type="pearson")/sqrt( 1-att.h )
plot(att.bestmodel)
abline(h=c(-1.96,1.96))
plot(Datalogistic$Pclass,att.bestmodel)
abline(h=c(-1.96,1.96))
plot(Datalogistic$SibSp,att.bestmodel)
abline(h=c(-1.96,1.96))
plot(fitted.values(bestmodel),att.bestmodel)
abline(h=c(-1.96,1.96))

testlogistic<- read.csv("test.csv",sep=",")
testlogistic$Pclass<-as.factor(testlogistic$Pclass)
testlogistic$SibSp<-as.factor(testlogistic$SibSp) 
testlogistic$Suvived<-as.factor(testlogistic$Survived) 
testlogistic<-na.omit(testlogistic)
data.frame(predict(bestmodel,testlogistic, type = "response"))


is.factor(testlogistic$Pclass)

#K-Fold Cross Validation(http://www.r-bloggers.com/evaluating-logistic-regression-models/)

testlogistic<- read.csv("test.csv",sep=",")
testlogistic<-testlogistic[,c(1,2,4,5,6)]
testlogistic<-na.omit(testlogistic)

library(caret)
ctrl <- trainControl(method = "boot", number = 20, savePredictions = TRUE)
?trainControl
testlogistic$survived<-as.factor(testlogistic$survived)
mod_fit <- train(survived~-1+Pclass+Age+SibSp,  data=testlogistic, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit, newdata=testlogistic)
confusionMatrix(data=pred, testlogistic$survived)




#best model

Datalogistic$Pclass<-as.factor(Datalogistic$Pclass)
is.factor(Datalogistic$Pclass)
Datalogistic$Sex<-as.factor(Datalogistic$Sex) 
is.factor(Datalogistic$Sex)
Datalogistic$SibSp<-as.factor(Datalogistic$SibSp) 
is.factor(Datalogistic$SibSp)
Datalogistic$Parch<-as.factor(Datalogistic$Parch) 
Datalogistic$Embarked<-as.factor(Datalogistic$Embarked)

bestmodel<-glm(Survived~-1+Pclass+Age+SibSp, family=binomial, data=Datalogistic)
summary(bestmodel)

predi

#Use the model for prediction 

summary(Oppycountrymodel)
coefficients(Oppycountrymodel)

probability<-
function(inactive,noactive,china,HK,india,japan,korea,SEA,TW)
{exp(2.1549195-0.2416220*inactive-2.1573296*noactive-2.3317760*china-3.0935040*HK-4.2752600*india-0.9257586*japan-1.5750797*korea-3.0358558*SEA-0.4224906*TW)/
(1+exp(2.1549195-0.2416220*inactive-2.1573296*noactive-2.3317760*china-3.0935040*HK-4.2752600*india-0.9257586*japan-1.5750797*korea-3.0358558*SEA-0.4224906*TW))}



#sales got a new customer who is from ANZ and active of buying the product that the sales is trying to sell. now he wants know the "probability" 

probability(0,0,0,1,0,0,0,0,0)

#the intercept is for ANZ and Active 

#Model Lift

ANZ<-matrix(rep(c(0,0,0,0,0,0,0,0,0),81000),nrow=9000,ncol=9,byrow=TRUE)
china<-matrix(rep(c(0,0,1,0,0,0,0,0,0),22500),nrow=2500,ncol=9,byrow=TRUE)
HK<-matrix(rep(c(0,0,0,1,0,0,0,0,0),9000),nrow=1000,ncol=9,byrow=TRUE)
india<-matrix(rep(c(0,0,0,0,1,0,0,0,0),9000),nrow=1000,ncol=9,byrow=TRUE)
japan<-matrix(rep(c(0,0,0,0,0,1,0,0,0),81000),nrow=9000,ncol=9,byrow=TRUE)
korea<-matrix(rep(c(0,0,0,0,0,0,1,0,0),81000),nrow=9000,ncol=9,byrow=TRUE)
SEA<-matrix(rep(c(0,0,0,0,0,0,0,1,0),18000),nrow=2000,ncol=9,byrow=TRUE)
TW<-matrix(rep(c(0,0,0,0,0,0,0,0,1),81000),nrow=9000,ncol=9,byrow=TRUE)

TestD<-data.frame(rbind(ANZ,china,HK,india,japan,korea,SEA,TW))
Propesnity<-1-probability(TestD$X1,TestD$X2,TestD$X3,TestD$X4,TestD$X5,TestD$X6,TestD$X7,TestD$X8,TestD$X9)


P<-as.numeric(Propesnity)
Decile<-as.numeric(cut(Propesnity,seq(0,1,0.1)))
             #group the propensity into deciles


X<-data.frame(100*cumsum(prop.table(table(data.frame(Decile)))))
names(X)<-c("LIFT")

#calculate cumulative percentage of propensity and put the dataset to data.frame given LIFT variable name

Propensityplot<-data.frame(seq(0,10,by=1),c(0,0, 42.35294,63.52941, 84.70588, 84.70588, 90.58824, 90.58824,97.64706, 97.64706,100))

#there are not 1st and 2nd deciles for this model so create 1st and 2nd deciles with 0% to combine with X 

plot(Propensityplot, type="o",xlab="Deciles",ylim=c(0,100),xlim=c(0,10),ylab="LIFT", lwd=2.8,col=4,axes=FALSE)
abline(0,10, col=2, lwd=3)
axis(1, at=0:10)
axis(2,at=seq(0,100,by=10),lab=c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"))
legend(locator(1),c("Model Lift","Random Lift"),pch=c(1,1),lwd=0.8,col=c(4,2),cex=1)
title("LIFT CHART", lwd=0.8 , cex.main=1)


#Plot Lift Chart. Due to that the coefficients are all negative except intercept for anz and active in the demo data. the model lift goes up quite slow compared to random lift. 

