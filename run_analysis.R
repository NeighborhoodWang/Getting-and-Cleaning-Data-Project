library(dplyr)
library(tidyr)
x_train <- read.fwf(file="X_train.txt",skip=0,widths=rep(16,each=561))#load in x_train.txt
features <- read.table(file="features.txt",sep=" ")#load in features.txt
meanIndex <- grep("mean",as.character(features$V2))#filter the index whose value has "mean"
stdIndex <- grep("std",as.character(features$V2))#filter the index whose value has "std"
meanAndStdIndex <- c(meanIndex,stdIndex)#bind the "mean index" and "std index" together
meanAndStdFeatures <- as.character(features$V2[meanAndStdIndex])#fetch the descriptive(names) of every columns
descriptiveNames <- c(meanAndStdFeatures,"Activities","Subjects")#set up a vector full of descriptive(names)
x_trainMeanStd<-x_train[,meanAndStdIndex]#filter the dataset only the column is "mean" or "std"
x_test <- read.fwf(file="X_test.txt",skip=0,widths=rep(16,each=561))#load in x_test.txt
x_testMeanStd<-x_test[,meanAndStdIndex]#filter the dataset only the column is "mean" or "std"
y_train <- read.table(file="y_train.txt",sep="\n")#load in y_train.txt
y_test <- read.table(file="y_test.txt",sep="\n")#load in y_test.txt
subject_test <- read.table(file="subject_test.txt",sep="\n")#load in subject_test.txt
subject_train <- read.table(file="subject_train.txt",sep="\n")#load in subject_train.txt
activity_labels <- read.table(file="activity_labels.txt",sep=" ")#load in activity_labels.txt
train <- cbind(x_trainMeanStd,y_train,subject_train)#bind the xTrain,yTrain,subjuctTrain together
test <- cbind(x_testMeanStd,y_test,subject_test)#bind the xTest,yTest,subjuctTest together
dataset <- rbind(train,test)#bind trainSet and testSet
names(dataset) <- descriptiveNames#assign the descriptive to the columnNames
averageSub <- dataset[180,]#set up a Pseudo-Zero data.frame,init this data.frame for later use
for(j in 1:30)#the index of Subjects
{
    for(k in 1:6)#the index of Activities
  {
    aveRowIndex <- (j-1)*6 + k
    temp <- filter(dataset,Activities==k,Subjects==j)
    for(l in 1:79)#the index of columns
    {
      averageSub[aveRowIndex,l] <- mean(temp[,l])#get the mean of specified column,subject and activities
    }
    averageSub[aveRowIndex,80] <- k
    averageSub[aveRowIndex,81] <- j
  }
}
for(i in 1:6){
  dataset$Activities[dataset$Activities == i] <- as.character(activity_labels$V2[i])#make Activities easy to understand
                              #change numbers to explicity descriptive
}
for(i in 1:6){
  averageSub$Activities[averageSub$Activities == i] <- as.character(activity_labels$V2[i])
}
