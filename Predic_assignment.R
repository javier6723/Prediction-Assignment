#Practical learning Machine Peer Assignment

#downloading the training data 
if (!file.exists("pml-training.csv")) {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                  destfile = "pml-training.csv", method = "wget")
}
#downloading the testing data 
if (!file.exists("pml-testing.csv.csv")) {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                  destfile = "pml-testing.csv", method = "wget")
}

#reading de training and testing data
pml_training<-read.csv("pml-training.csv")
pml_testing<-read.csv("pml-testing.csv")

#EXTRACTING ONLY THE MEASUREMENTS
names<-names(pml_training)
subsetnames<-grep("^roll_|^pitch|^yaw_|^total_accel|^gyros_|^accel_|^magnet|classe",names,value=T)
#creating a subset with the variables selected
pml_training2<-subset(pml_training,select=subsetnames)
names<-names(pml_testing)
subsetnames<-grep("^roll_|^pitch|^yaw_|^total_accel|^gyros_|^accel_|^magnet|classe",names,value=T)
pml_testing2<-subset(pml_testing,select=subsetnames)
###Preprocessing the data
library(caret)
#spliting pml_training2 into a trainig and a testing set 
set.seed(2014)
inTrain<-createDataPartition(y=pml_training2$classe,p=0.6,list=FALSE)

training<-pml_training2[inTrain,]
testing<-pml_training2[-inTrain,]

##initial model
##glm no funciona porque la 
#library(e1071)
#modelFit<-train(classe~.,data=training[,-1],method="glm")

#Removing zero covariates
nsv<-nearZeroVar(training,saveMetrics=TRUE)
table(nsv$nzv)
#training<-training[,-1]
#testing<-testing[,-1]
#correlated predictors
#M<-abs(cor(training[,c(-1,-54)]))
library(corrplot)
par(mar=c(4.1,2,2,1))
corrplot(M, type = "lower",method = "square",tl.cex=.6)

M<-abs(cor(training[,-53]))
diag(M)<-0
which(M>0.9,arr.ind=T)

#pre processing with pca
preProc <- preProcess(training[, -53], method = "pca", thresh = 0.95)
trainingPC <- predict(preProc, training[, -53])
testingPC <- predict(preProc, testing[, -53])
pml_testingPC<-predict(preProc,pml_testing2)
#random forest
set.seed(2014)
modelfit <- train(training$classe ~., method = "rf", data = trainingPC
                  ,trControl = trainControl(method = "cv",  number = 4)
                  ,importance = TRUE)
modelfit

predicted<-predict(modelfit,newdata=testingPC)
confusionMatrix(testing$classe,predict(modelfit,testingPC))
confusionMatrix(testing$classe,predict(modelfit,pml_testingPC))

#hay un outlier entre la variable 31, 33,46 de training[,c(-1,-54)]
#a<-training[,c(-1,-54)]
#c<-a[,33]!==317
#b<-a[,!c]

#PREDICTING WITH TREES
#library(rpart)
#set.seed(2014)
#modfit<-train(classe~.,method="rpart",data=training)
#print(modfit$finalModel)

#plot(modfit$finalModel,uniform=T,main="Classification Tree")
#text(modfit$finalModel,use.n=T,all=T,cex=.8)

#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")
library(rattle)
fancyRpartPlot(modfit$finalModel)

#preditions
#predicted<-predict(modfit,newdata=testing)
#confusionMatrix(testing$classe,predict(modfit,testing))

#RANDOM FOREST
#library(caret)
#library(randomForest)
#set.seed(2014)
#modfit<-train(classe~.,method="rf",data=training,prox=TRUE)
#modfit
#smalltraining<-training[,c(1,2,3,4,8,9,10,11,18,19,21,24,25,26,28,29,34,36)]
#smalltraining1<-training[,-c(1,2,3,4,8,9,10,11,18,19,21,24,25,26,28,29,34,36)]
#prComp<-prcomp(smalltraining)
#smtrainingPC<-prComp$x[,c(1,2,3)]
#smalltraining1<-cbind(smtrainingPC,smalltraining1) 

#library(rpart)
#set.seed(2014)
#modfit<-train(classe~.,method="rpart",data=smalltraining1)
#print(modfit$finalModel)

#plot(modfit$finalModel,uniform=T,main="Classification Tree")
#text(modfit$finalModel,use.n=T,all=T,cex=.8)

#smalltesting<-testing[,c(1,2,3,4,8,9,10,11,18,19,21,24,25,26,28,29,34,36)]
#smalltesting1<-testing[,-c(1,2,3,4,8,9,10,11,18,19,21,24,25,26,28,29,34,36)]
#smtestingPC<-predict(prComp,smalltesting)[,c(1,2,3)]
#smalltesting1<-cbind(smtestingPC,smalltesting1)

#predicted<-predict(modfit,newdata=smalltesting1)
#confusionMatrix(smalltesting1$classe,predict(modfit,smalltesting1))


#PREDICTING WITH TREES
library(rpart)
set.seed(2014)
modelfit<-train(training$classe~.,method="rpart",data=trainingPC)
print(modelfit$finalModel)

plot(modelfit$finalModel,uniform=T,main="Classification Tree")
text(modelfit$finalModel,use.n=T,all=T,cex=.8)
