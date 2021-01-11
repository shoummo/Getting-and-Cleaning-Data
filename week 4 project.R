setwd("E:/Coursera Data cleaning with R/Data for Project")

# Unzip dataSet to /data directory
unzip(zipfile="dataset.zip",exdir="E:/Coursera Data cleaning with R/Data for Project")

#define the path where the new folder has been unziped
pathdata = file.path("E:/Coursera Data cleaning with R/Data for Project", "UCI HAR Dataset")

#create a file which has the 28 file names
files = list.files(pathdata, recursive=TRUE)
#show the files
files

### Data Analysis ###

#A. Making the Test and Training Set Data

### 1. Output Steps - Here we begin how to create the data set of training and test
#Reading training tables - xtrain / ytrain, subject train
xtrain = read.table(file.path(pathdata, "train", "X_train.txt"),header = FALSE)
ytrain = read.table(file.path(pathdata, "train", "y_train.txt"),header = FALSE)
subject_train = read.table(file.path(pathdata, "train", "subject_train.txt"),header = FALSE)
#Reading the testing tables
xtest = read.table(file.path(pathdata, "test", "X_test.txt"),header = FALSE)
ytest = read.table(file.path(pathdata, "test", "y_test.txt"),header = FALSE)
subject_test = read.table(file.path(pathdata, "test", "subject_test.txt"),header = FALSE)
#Read the features data
features = read.table(file.path(pathdata, "features.txt"),header = FALSE)
#Read activity labels data
activityLabels = read.table(file.path(pathdata, "activity_labels.txt"),header = FALSE)

#B. Tagging the test and train data sets now - Objective 3 of the Assignment
#Create Sanity and Column Values to the Train Data
colnames(xtrain) = features[,2]
colnames(ytrain) = "activityId"
colnames(subject_train) = "subjectId"
#Create Sanity and column values to the test data
colnames(xtest) = features[,2]
colnames(ytest) = "activityId"
colnames(subject_test) = "subjectId"
#Create sanity check for the activity labels value
colnames(activityLabels) <- c('activityId','activityType')

### The main objective was to merge the test and train data - Here are the R STEPS to ensure
#Merging the train and test data - important outcome of the project
mrg_train = cbind(ytrain, subject_train, xtrain)
mrg_test = cbind(ytest, subject_test, xtest)
#Create the main data table merging both table tables - this is the outcome of 1
setAllInOne = rbind(mrg_train, mrg_test)

### Objective of the merger is now complete ###

# 2. Extracting only the measurements on the mean and standard deviation for each measurement

# Need step is to read all the values that are available
colNames = colnames(setAllInOne)
#Need to get a subset of all the mean and standards and the correspondongin activityID and subjectID 
mean_and_std = (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))
#A subtset has to be created to get the required dataset
setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]

### 3. Use descriptive activity names to name the activities in the data set

setWithActivityNames = merge(setForMeanAndStd, activityLabels, by='activityId', all.x=TRUE)

## Label the data set with decriptive variable names

# New tidy set has to be created
secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

#The last step is to write the ouput to a text file 
write.table(secTidySet, "secTidySet.txt", row.name=FALSE)