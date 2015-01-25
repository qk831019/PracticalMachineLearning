
library(caret)
set.seed(825)

trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

### Load data

training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!","","."))
test <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!","","."))


### Clean the data by removing features have 80% or more missing values

noMissCol = colSums(is.na(training))/length(training[,1]) < 0.8
training1 = training[,noMissCol]
training1 = training1[,-1]

### check missing value after cleaning

colSums(is.na(training1))


## Fit model using rf method

### Create fit control using 10-fold cross validation

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,

  
  ### Fit model with random forest method

  rf_model<-train(classe~.,data=training1,method="rf",
                  trControl=trainControl(method="cv",number=5),
                  prox=TRUE,allowParallel=TRUE)

### Check final model fit
print(rf_model$finalModel)
###Check variable importance and plot
varImp(rf_model)
plot(varImp(rf_model))
  
### Predict using final model
predtrain = predict(rf_model,training)
  
### Provide error report.
confusionMatrix(predtrain,training1$classe)

## Conclusions and Test Data Submit
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
predtest = predict(rf_model,test1)
answer = as.character(predtest)
pml_write_files(answer)

  
  