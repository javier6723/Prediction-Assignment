Weight Lifting Exercise Predicting Model using data from accelerometers on the belt, forearm, arm, and dumbell
========================================================
*An study by Javier Rangel using the Weight Lifting Exercise Dataset available at [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har) as part of the Practical Machine Learning Coursera course.*

## Synopsis
This study was conducted using the Weight Lifting Exercise Dataset[1] in which are recorded the data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants who were asked to perform ten repetitions of barbell lifts in five (5) different ways: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). class A is correct manner and the others are incorrect.
the training data used in this analysis is avaliable at [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv) and the test data is avaliable at [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv) and originally come from this source [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har).
The analysis was perfom in an Intel® Pentium(R) CPU, Linux/Ubuntu 12.04 OS, R version 3.1.0.

## Data Processing
The firts step taken was to download the training and test data for this analysis come in the form of a comma-separated-value file, from the course web site. the method used was wget instead of curl or lynx. Then the data was read into R. 


```r
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
```
Following step was to select the variables to use in the analysis, in this case were select the variables: "classe", which is going to be predicted and all the variables which begin with "roll", "pitch", "yaw", "total_accel","gyros","accel" and "magnet".
 Our new training set now has 53 variables, which was 
 
 

```r
names<-names(pml_training)
subsetnames<-grep("^roll_|^pitch|^yaw_|^total_accel|^gyros_|^accel_|^magnet|classe",names,value=T)
#creating a subset with the variables selected
pml_training2<- subset(pml_training,select=subsetnames)

names<-names(pml_testing)
subsetnames<-grep("^roll_|^pitch|^yaw_|^total_accel|^gyros_|^accel_|^magnet|classe",names,value=T)
pml_testing2<-subset(pml_testing,select=subsetnames)
```

Our new training set now has 53 variables, which was splitted in two sets a training with 60% of the data in the original pml-training set and testing set with the other 40% to validate the results.


```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
#spliting pml_training2 into a trainig and a testing set 
set.seed(2014)
inTrain<-createDataPartition(y=pml_training2$classe,p=0.6,list=FALSE)

training<-pml_training2[inTrain,]
testing<-pml_training2[-inTrain,]
pml_testingPC<-predict(preProc,pml_testing2)
```

```
## Error: objeto 'preProc' no encontrado
```
in the next plot are shown the linear correlation between the variables, there are many which are highly correlated.


```r
library(corrplot)
par(mar=c(4.1,2,2,1))
corrplot(M, type = "lower",method = "square",tl.cex=.6)
```

```
## Error: objeto 'M' no encontrado
```

Due to the high dimensionality in the data it was performed a principal component pre-processing to reduce the dimension on it, with a new set of variables which retain the major variability. The the pre-processing was applied to our trainig and testing subsets of the pml-training set.


```r
#pre processing with pca
set.seed(2014)
preProc <- preProcess(training[, -53], method = "pca", thresh = 0.95)
trainingPC <- predict(preProc, training[, -53])
testingPC <- predict(preProc, testing[, -53])
pml_testingPC<-predict(preProc,pml_testing2)
```
The following step was to build the predictive model using the random forest method on the principal components obtained in the pre-processing step


```r
set.seed(2014)
modelfit <- train(training$classe ~., method = "rf", data = trainingPC
                  ,trControl = trainControl(method = "cv",  number = 4)
                  ,importance = TRUE)
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```


## Results
In  the following output can be seen the confusion matrix and  the statistics. the accuracy of the final model was of 97%, the sensitivity for every classe level was more than 90%.


```r
confusionMatrix(testing$classe,predict(modelfit,testingPC))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2206    7    5   11    3
##          B   24 1470   21    0    3
##          C    1   29 1312   22    4
##          D    3    2   58 1223    0
##          E    0   13   19    5 1405
## 
## Overall Statistics
##                                         
##                Accuracy : 0.971         
##                  95% CI : (0.967, 0.974)
##     No Information Rate : 0.285         
##     P-Value [Acc > NIR] : < 2e-16       
##                                         
##                   Kappa : 0.963         
##  Mcnemar's Test P-Value : 3.51e-09      
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.987    0.966    0.927    0.970    0.993
## Specificity             0.995    0.992    0.991    0.990    0.994
## Pos Pred Value          0.988    0.968    0.959    0.951    0.974
## Neg Pred Value          0.995    0.992    0.984    0.994    0.998
## Prevalence              0.285    0.194    0.180    0.161    0.180
## Detection Rate          0.281    0.187    0.167    0.156    0.179
## Detection Prevalence    0.284    0.193    0.174    0.164    0.184
## Balanced Accuracy       0.991    0.979    0.959    0.980    0.994
```

the predicted values in pml-testing set are the following

```r
predicted<-predict(modelfit,newdata=pml_testingPC)
predicted
```

```
##  [1] B A B A A B D B A A B C B A E E A B B B
## Levels: A B C D E
```


## References

[1] Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.
