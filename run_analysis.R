# run_analysis.R for Cousera Course "Getting and Cleaning Data" 
# Author: Dongli Liu

# Make sure package dplyr installed

library(dplyr)

# ------------------------------------------------------------------------
# Step O: Download "Human Activity Recognition Using Smartphones Data Set"
# ------------------------------------------------------------------------

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("UCI HAR Dataset")) {
  download.file(fileUrl, destfile="UCI.zip") 
  unzip("UCI.zip")
}

# --------------------------------------------------------------------
# Step 1: Merges the training and the test sets to create one data set
# --------------------------------------------------------------------

# Load features and labels
features.names <- read.table("UCI HAR Dataset/features.txt", 
                             col.names = c("id", "features"))
activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt", 
                              col.names = c("activity","label"))

# Load test and train files
x.test <- read.table("UCI HAR Dataset/test/X_test.txt", 
                     col.names = features.names$features)
x.train <- read.table("UCI HAR Dataset/train/X_train.txt", 
                      col.names = features.names$features)
subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt", 
                           col.names = "subject")
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt", 
                            col.names = c("subject"))
y.test <- read.table("UCI HAR Dataset/test/y_test.txt", 
                     col.names = c("activity"))
y.train <- read.table("UCI HAR Dataset/train/y_train.txt", 
                      col.names = c("activity"))

# bind them
x.all <- bind_rows(x.test, x.train)
y.all <- bind_rows(y.test, y.train)
xy.all <- bind_cols(y.all, x.all)
subject.all <- bind_rows(subject.test, subject.train)
data.all <- bind_cols(subject.all, xy.all)


# -----------------------------------------------------------------------------
# Step 2: Extracts only the measurements on the mean and standard deviation for 
# each measurement 
# -----------------------------------------------------------------------------

# Here we set ignore.case = TRUE to make sure both mean & Mean could be included; 
# indeed default is TRUE
data.extract <- select(data.all, subject, activity, 
                       contains("mean", ignore.case = TRUE), contains("std"))

# ------------------------------------------------------------------------------
# Step 3: Uses descriptive activity names to name the activities in the data set
# ------------------------------------------------------------------------------

# Combine extracted data with activity labels, then drop activity column leave the label
data.joined <- left_join(activity.labels, data.extract, by = "activity")
data.desc <- select(data.joined, -activity)

# -------------------------------------------------------------------------
# Step 4: Appropriately labels the data set with descriptive variable names
# -------------------------------------------------------------------------

# Drop unnecesary "." and correct "Mean" to "mean"
temp <- names(data.desc)
temp1 <- gsub("\\.\\.\\.", ".", temp)
temp2 <- gsub("\\.\\.$", "", temp1)
temp3 <- gsub("\\.$", "", temp2)
temp4 <- gsub("Mean", "mean", temp3)
names(data.desc) <- temp4


# ----------------------------------------------------------------------------
# Step 5: Creates a second, independent tidy data set with the average of each
# variable for each activity and each subject.
# ----------------------------------------------------------------------------

data.mean <-
  data.desc %>%
  group_by(subject, label) %>%
  summarise_each(funs(mean)) 

# Output the data
write.table(data.mean, file="data.mean.txt", row.name=FALSE)
