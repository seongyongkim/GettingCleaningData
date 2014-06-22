library(reshape2)

# Read the Data file, the Subject file, the Activity file, and combine them.
# Extract only mean() and std() of each variable.
# Labels columns based on labels from Features file.
# Labels activity based on labels from activityLable file.
# Returns resulting data set. 
mergeUCIHarData <- function(subjectFile, activityFile, dataFile, featureFile, activityLabelFile) {
	# load Feature list which will be the variables (Column Labels)
	features <- read.delim(featureFile, header=FALSE, sep="", col.names=c("colno", "name"))

	# 4. Read data file and appropriately labels the data set with descriptive variable names. 
	data <- read.delim(dataFile, header=FALSE, sep="", col.names=features$name)

	# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
	colnames <- names(data)
	filtered.colnames <- colnames[grepl("\\.mean\\.|\\.std\\.", colnames)]
	data <- data[, filtered.colnames]

	# 3. Uses descriptive activity names to name the activities in the data set
	activity <- read.delim(activityFile, header=FALSE, sep="", col.names="activity")
	activity.labels <- read.delim(activityLabelFile, header=FALSE, sep="", col.names=c("level", "label"))
	activity$activity <- factor(activity$activity, labels=activity.labels$label)

	# Read subject data 
	subject <- read.delim(subjectFile, header=FALSE, sep="", col.names="subject.id")

	# Combine subject, activity, and data
	cbind(subject, activity, data)
}

data.test <- mergeUCIHarData("UCI HAR Dataset/test/subject_test.txt", 
				"UCI HAR Dataset/test/Y_test.txt", 
				"UCI HAR Dataset/test/X_test.txt", 
				"UCI HAR Dataset/features.txt", 
				"UCI HAR Dataset/activity_labels.txt")

data.train <- mergeUCIHarData("UCI HAR Dataset/train/subject_train.txt", 
				"UCI HAR Dataset/train/Y_train.txt", 
				"UCI HAR Dataset/train/X_train.txt", 
				"UCI HAR Dataset/features.txt", 
				"UCI HAR Dataset/activity_labels.txt")

# 1. Merges the training and the test sets to create one data set.
data <- rbind(data.test, data.train)

# Clean up unnecessary objects.
rm(data.test)
rm(data.train)

# Make a Narrow Tidy Dataset
dataMelt <- melt(data, id=c("subject.id", "activity"))

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
dataCast <- dcast(dataMelt, subject.id + activity ~ variable, mean)

options(width=10000)
print(dataCast)
# Write the tidy data set to a file
# write.table(dataCast, file = "UCI-HAR-TidyDataSetWithAvgOfMeanAndSd.txt")