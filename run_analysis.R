## This script is for tide UCI HAR Dataset and make it easy to use
## We only handle the column that is certained about mean and std

## Step 0:load nessary packages
library(dplyr)
library(tidyr)

## Step 1:Load All necessary data

x_train <- read.fwf(file=".\\UCI HAR Dataset\\train\\X_train.txt",skip=0,widths=rep(16,each=561))
y_train <- read.table(file=".\\UCI HAR Dataset\\train\\y_train.txt",sep="\n")#load in y_train.txt
subject_train <- read.table(file=".\\UCI HAR Dataset\\train\\subject_train.txt",sep="\n")

x_test <- read.fwf(file=".\\UCI HAR Dataset\\test\\X_test.txt",skip=0,widths=rep(16,each=561))
y_test <- read.table(file=".\\UCI HAR Dataset\\test\\y_test.txt",sep="\n")#load in y_test.txt
subject_test <- read.table(file=".\\UCI HAR Dataset\\test\\subject_test.txt",sep="\n")#load in subject_test.txt

features <- read.table(file=".\\UCI HAR Dataset\\features.txt",sep=" ")#load in features.txt
activity_labels <- read.table(file=".\\UCI HAR Dataset\\activity_labels.txt",sep=" ")#load in activity_labels.txt

## Step 2:Suset X_train.txt and X_test.txt with only “mean” and “std” columns
meanIndex <- grep("mean",as.character(features$V2))#filter the index whose value has "mean"
stdIndex <- grep("std",as.character(features$V2))#filter the index whose value has "std"
meanAndStdIndex <- c(meanIndex,stdIndex)#bind the "mean index" and "std index" together
x_trainMeanStd<-x_train[,meanAndStdIndex]#filter the dataset only the column is "mean" or "std"
x_testMeanStd<-x_test[,meanAndStdIndex]#filter the dataset only the column is "mean" or "std"

## Step 3:Get the descriptive(names) of every columns,Then tide the descriptive names.
meanAndStdFeatures <- as.character(features$V2[meanAndStdIndex])#fetch the descriptive(names) of every columns
descriptiveNames <- c(meanAndStdFeatures,"Activities","Subjects")#set up a vector full of descriptive(names)

## Step 4:Column bind train with activity and subject
##        Column bind test with activity and subject
train <- cbind(x_trainMeanStd,y_train,subject_train)#bind the xTrain,yTrain,subjuctTrain together
test <- cbind(x_testMeanStd,y_test,subject_test)#bind the xTest,yTest,subjuctTest together

## Step 5:Row bind train,test to “dataset”
dataset <- rbind(train,test)#bind trainSet and testSet

## Step6:Tide the data set
## Assign the descriptive names to the column names of dataset
names(dataset) <- descriptiveNames#assign the descriptive to the columnNames

## Step7:Aggregate the “dataset” and get the average dataset that this assignment asked for.
attach(dataset)
a <- aggregate(dataset, by = list(Subjects, Activities), FUN = mean)
averageSub <- subset(a, select=-c(Group.1, Group.2))

## Step8 Tide the data set
## Assign the activities numbers to explicitly descriptive.
for(i in 1:6){
  dataset$Activities[dataset$Activities == i] <- as.character(activity_labels$V2[i])#make Activities easy to understand
  #change numbers to explicity descriptive
}
for(i in 1:6){
  averageSub$Activities[averageSub$Activities == i] <- as.character(activity_labels$V2[i])#make Activities easy to understand
  #change numbers to explicity descriptive
}

##Step 9:## Export this dataset as averageSub.txt
write.table(averageSub, file = ".\\averageSub.txt", row.name=FALSE) 
