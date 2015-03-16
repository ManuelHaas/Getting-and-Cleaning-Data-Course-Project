library(plyr)

# load the Training set data.
trainingSet = read.table("./UCI HAR Dataset/train/X_train.txt")
# load the data that identifies the subject who performed the activity for each window sample.
trainingSubjects = read.table("./UCI HAR Dataset/train/subject_train.txt")
# load the Training labels.
trainingActivities = read.table("./UCI HAR Dataset/train/y_train.txt")

# load the Test set data.
testSet = read.table("./UCI HAR Dataset/test/X_test.txt")
# load the data that identifies the subject who performed the activity for each window sample.
testSubjects = read.table("./UCI HAR Dataset/test/subject_test.txt")
# load the Test labels.
testActivities = read.table("./UCI HAR Dataset/test/y_test.txt")

# load the list of all features.
features = read.table("./UCI HAR Dataset/features.txt")

# load the list that links the class labels with their activity name.
activityLabels = read.table("./UCI HAR Dataset/activity_labels.txt")

# merge the training set and the test set and create one data set out of it.
mergedSet = rbind(trainingSet, testSet)

# merge the lists of the  training subjects and the test subjects.
mergedSubjects = rbind(trainingSubjects, testSubjects)

# merge the lists of the  training labels and the test labels.
mergedActivities = rbind(trainingActivities, testActivities)

# label the data set with descriptive variable names.
names(mergedSet) = features[,2]
# label the subjects set with descriptive variable name.
names(mergedSubjects) = "Subject"
# label the labels set with descriptive variable name.
names(mergedActivities) = "Class"
# label the labels set with descriptive variable name.
names(activityLabels) = c("Class", "Activity")

# join the list of activities with the labels corresponding to that activity class.
mergedActivities = join(mergedActivities, activityLabels, by="Class")

# add a column with the corresponding activities to the mergedSet.
mergedSet = cbind(Activity = mergedActivities$Activity, mergedSet)

# add a column with the corresponding subject to the mergedSet.
mergedSet = cbind(Subject = mergedSubjects$Subject, mergedSet)

# reduce the dataset to columns that are regarding to the measurements on the mean and 
# standard deviation for each measurement. To do that use grep statements to search the
# column names for the patterns 'mean()' and 'std()' and generate a vector from the results.
# Add the first two columns (Subject and Activity) and sort it afterwards.
regardingColIdx = c(1, 2, grep("mean()", names(mergedSet), fixed=T), grep("std()", names(mergedSet), fixed=T))
mergedSet = mergedSet[, sort(regardingColIdx)]

# Create a second, independent tidy data set with the average of each variable for each activity and each subject
# group by subject

# create a nested list that is grouped by Subjects and Activities
nestedList = split(mergedSet, list(mergedSet$Subject, mergedSet$Activity))

tidySet = data.frame(stringsAsFactors = F)
n = length(nestedList)
numericColNum = ncol(mergedSet)
for (i in 1:n) {
    # calculate the column means of the feature columns
    averageValues = colMeans(nestedList[[i]][3:numericColNum])
    # add the current row to the tidy data set
    tidySet = rbind(tidySet, c(nestedList[[i]][[1]][1], nestedList[[i]][[2]][1], averageValues))
}

# label the tidy data set with descriptive variable names.
names(tidySet) = names(mergedSet)

# output the tidy data set
tidySet