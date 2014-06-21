##Getting and cleaning data assignment
##Dowload and unzip file into working directory

#######################################################################################
#This section creates the files used in the merge procedure below

##Each row in V2 of VariableLabels corresponds to the name of each of the 561 columns in the test and trial datasets
VariableLabels <- read.table("./features.txt")

##File linking numeric value to each of 6 measured activities (walking, walking_downstairs, etc)
ActivityLabels <- read.table("./activity_labels.txt")

##Test data consisting of 2947 observations of 561 variables
testData <- read.table("./test/X_test.txt")

##Numeric labels (1-6) for the activity (walking, walking_upstairs, etc) contained in each row of the test data
##File linking the number to its activity is found in ActivityLabels
testActivityLabels <- read.table("./test/Y_test.txt")

##List person who performed the activity represented by each row of the test data
##apparently only 9 subjects for test data
testSubjects <- read.table("./test/subject_test.txt")

##training data consisting of 7352 observations of 561 variables
trainData <- read.table("./train/X_train.txt")

##Numeric labels (1-6) for the activity (walking, walking_upstairs, etc) contained in each row of the train data
##File linking the number to its activity is found in ActivityLabels
trainActivityLabels <- read.table("./train/Y_train.txt")

##List person who performed the activity represented by each row of the training data
##apparently 21 subjects for training data
trainSubjects <- read.table("./train/subject_train.txt")


##############################################################################
#Merging and labeling data
#Each step required for the homework is labeled with its corresponding number

#Add testActivityLabels and testSubjects to testData
testData$Activity <- testActivityLabels$V1
testData$Subject <- testSubjects$V1

#Add trainActivityLabels and trainSubjects to trainData
trainData$Activity <- trainActivityLabels$V1
trainData$Subject <- trainSubjects$V1

#1. Merges the training and the test sets to create one data set
mergedData <- rbind(testData, trainData)
#4. Appropriately labels the data set with descriptive variable names. 
names(mergedData) <- VariableLabels[,2]
names(mergedData)[562] <- "Activity"
names(mergedData)[563] <- "Subject"

#3. Uses descriptive activity names to name the activities in the data set
#Label the activities by creating a factor variable based on ActivityLables
mergedData$ActivityFactor<-factor(mergedData$Activity, labels=ActivityLabels$V2)
 
###################################################################################
#2. Extracts only the measurements on the mean and standard deviation for each measurement
#Find columns with "mean()" or "std()" in the variable name
#Each of the following is a logical vector with TRUE in the position of each column with the necessary text
meanCols<-grepl("mean()", names(mergedData), fixed=TRUE)
stdCols<-grepl("std()", names(mergedData), fixed=TRUE)
#Select the columns indicated in the two steps above
mergedMeans<-mergedData[,meanCols]
mergedStds<-mergedData[,stdCols]
#Combine means and stds into a dataframe
data<-cbind(mergedMeans, mergedStds)
#Add the ActivityFactor and Subject Columns
data$Activity<-mergedData$ActivityFactor
data$Subject<-mergedData$Subject

#############################################################################
#5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#The result is a dataframe in which the first two columns represent each possible
  #comination of the activity and subject variables
#Following this are 66 columns, one for each mean or std variable
#The data in each row of those 66 columns is the mean observations for that subject for that activity
tidyDF<-aggregate(data, list(SubjectID=data$Subject, ActivityName=data$Activity), mean)

#Drop the columns I don't need
tidyDF$Activity<-NULL
tidyDF$Subject<-NULL

##Create the text file to submit with the homework
write.table(tidyDF, file="tidyDF.txt", sep=",", quote=FALSE)
