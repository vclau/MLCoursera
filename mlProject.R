setwd('~/Dropbox/Coursera - R/Practical Machine Learning/Proy')
library(caret); library(kernlab); library (ISLR);
#Load CSV file with data 
trainingD<-read.csv("pml-training.csv" ,na.strings=c("","NA"), header=T)

trainingD<-trainingD[ , apply(trainingD, 2, function(x) !any(is.na(x)))]
#Remove identifier columns 
trainingD <- trainingD[,-c(1:7)]
table(trainingD$classe)

## GET SAMPLE 
size<-as.integer((as.character(nrow(trainingD))))

sample.size = function(c.lev, margin=.5,
                       c.interval=.05, population) {
  z.val = qnorm(.5+c.lev/200)
  ss = (z.val^2 * margin * (1-margin))/c.interval^2
  p.ss = round((ss/(1 + ((ss-1)/population))), digits=0)
  METHOD = paste("Recommended sample size for a population of ",
                 population, " at a ", c.lev,
                 "% confidence level", sep = "")
  structure(list(Population = population,
                 "Confidence level" = c.lev,
                 "Margin of error" = c.interval,
                 "Response distribution" = margin,
                 "Recommended sample size" = p.ss,
                 method = METHOD),
            class = "power.htest")
}

sampleSize<-sample.size(99.9, , , size)


ss<-as.integer(sampleSize[5])

set.seed(1)
trainS<-trainingD[sample (nrow(trainingD), ss),]




#SPLIT DATA 
inTrain<-createDataPartition(y=trainS$classe, p=.7, list=F)
training<-trainS[inTrain,]
testing<-trainS[-inTrain,]
dim(training); dim(testing)

modFit<-train(classe~.,method="gbm", data=training, verbose=F)
print(modFit)

pred<-predict(modFit,testing); 
qplot(pred, classe, data=testing)
table(pred, testing$classe)

## Submission 

testingD<-read.csv("pml-testing.csv" ,na.strings=c("","NA"), header=T)
testingD<-testingD[ , apply(testingD, 2, function(x) !any(is.na(x)))]
finalTesting <- testingD[,-c(1:7)]
finalPrediction<-predict(modFit,finalTesting); 
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(finalPrediction)