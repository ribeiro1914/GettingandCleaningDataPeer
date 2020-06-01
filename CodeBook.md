#Getting and Cleaning Data - peer assessment project

##The original data was transformed by
 1. Merging the training and the test sets to create one data set.
 2. Extracting only the measurements on the mean and standard deviation for each measurement.
 3. Using descriptive activity names to name the activities in the data set
 4. Appropriately labeling the data set with descriptive activity names.
 5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject.

##About R script

File with R code "run_analysis.R" perform 5 following steps (in accordance assigned task of course work)

##About variables:
x_train, y_train, x_test, y_test, subject_train, subject_test, feature, and activityLabels contain the data from the downloaded files.

 - mergeTrain -> contains the merge between x_train, subject_train, y_train.
 - mergeTest -> contains the merge between x_test, subject_test, y_test.
 - bigMergedTable -> contains the merge between mergeTrain and mergeTest.
 - mean_and_std -> a matrix checking if the column correspond to the measurements asked for(mean and standard deviation).
 - bigMergedTable in 2.2 -> a clean table with only TRUE columns from mean_and_std.
 - BigMergedTableMeans -> second independent tidy data set with the average of each variable for each activity and each subjec.
 