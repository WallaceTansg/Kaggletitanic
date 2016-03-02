#use iris defult dataset for modelling
#massage the dataset first to have train and test dataset 
#for predictive models including marching learning

data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/2), replace = FALSE, 
              prob = rep(1/150,m)) 
iris.train <- iris[val,]
iris.test<- iris[-val,]

#Model 1 Weighted k-Nearest Neighbor Classifier Model (maching learning)
library(kknn)
#remove variable Species #
iris.test$Species <- NULL
iris.kknn <- kknn(Species~., iris.train, iris.test, distance = 1,
                    kernel = "optimal")


??kknn 
summary(iris.kknn)
kknn.fit <- fitted(iris.kknn)
#the validation of test with train for the model
library(gmodels)
#produce prediction based on the model built#
CrossTable(iris.test$Species, kknn.fit)
#association test with p-value for further validation#
chisq.test(table(iris.test$Species,kknn.fit)) 

#Model 2 Generalized Boosted Regression Model(maching learning)

iris.gbm <- gbm.fit(iris.train[,1:4],iris.train$Species,distribution="multinomial",interaction.depth =10,shrinkage=0.3,bag.fraction = 0.8,n.trees=1200)
summary(iris.gbm )
#produce prediction based on the model built#
gbm.fit<- predict(iris.mod,iris.test[,1:4],n.trees=1200)
#to make gbm.fit as matrix for later data massage#
gbm.fit.matrix<-matrix(as.vector(gbm.fit),nrow=75,ncol=3,byrow=FALSE)
#select the max value with col name in matrix 
gbm.fit.2<-colnames(gbm.fit)[apply(gbm.fit.matrix, 1, which.max)]

library(gmodels)
#produce prediction based on the model built#
CrossTable(iris.train$Species, gbm.fit.2)
#association test with p-value for further validation#
chisq.test(table(iris.train$Species, gbm.fit.2)) 


#Model 3 Generlised Linear Model - outcome multinomial(statistical model) #

library(nnet)
# . add all main effects to the model#
iris.glm<- multinom(Species~.,iris)
summary(iris.glm)
#produce prediction based on the model built#
glm.fit<- predict(iris.glm)

library(gmodels)
#the validation of test with train for the model#
CrossTable(iris$Species, glm.fit)
#association test with p-value for further validation#
chisq.test(table(iris$Species, glm.fit)) 


