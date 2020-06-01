library(dplyr)
#*
# Getting and Cleaning Data Course Project
#*

# 1. Merges the training and the test sets to create one data set.

# 1.1 Read the train Files

x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/Y_train.txt")
subject_train <- read.table("./train/subject_train.txt")

# 1.2 Read the test Files

x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/Y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

# 1.3 Read the features
features <- read.table("features.txt")

# 1.4 Read the activity labels
activityLabels <- read.table("activity_labels.txt")

# 1.5 Now that I have all the information in my variables, I will name the columns

colnames(x_train) <- features[,2]
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

# As the tables are ready lets merge them and complete step one

# 1.6 First merging Y and X 
mergeTrain <- cbind(y_train, subject_train, x_train)
mergeTest  <- cbind(y_test, subject_test, x_test)

# 1.7 Second merging Training and Test
bigMergedTable <- rbind(mergeTrain, mergeTest)

# 2 - Extracts only the measurements on the mean and standard deviation for each measurement.

# 2.1 Reading and determining which columns to use:

mean_and_std <- grepl("subject|activity|mean|std", colnames(bigMergedTable))

# 2.2 only use the data selected
bigMergedTable <- bigMergedTable[,mean_and_std] 

# 3. Uses descriptive activity names to name the activities in the data set

setActivityNames <- merge(bigMergedTable, activityLabels, by='activityId', all.x=TRUE)


# 4. Appropriately labels the data set with descriptive variable names.
  # Done in 1.5

# 5. From the data set in step 4, creates a second, independent tidy data set with the
#   average of each variable for each activity and each subject. 

BigMergedTableMeans <- bigMergedTable %>% 
  group_by(subjectId, activityId) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(BigMergedTableMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)


