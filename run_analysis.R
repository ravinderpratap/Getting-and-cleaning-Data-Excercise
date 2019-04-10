# Loading required Packages 
library(dplyr)

#Set Working Directory
setwd("C:/Users/r.pratap.singh/Desktop/JohnHopkins")

#string variables for file download
dwfile <- "UCIHDataset.zip"
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
repos <- "UCI HAR Dataset"

# File download verification. If file does not exist, download to working directory.
if(!file.exists(dwfile)){
  download.file(url,dwfile, method = "curl") 
}

# File unzip verification. If the directory does not exist, unzip the downloaded file.
if(!file.exists(repos)){
  unzip(dwfile)
}

# Reading the text files (data) and assigning all data frames

features <- read.table("UCI HAR Dataset/features.txt")
colnames(features) <- c("n","functions")

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
colnames(activities) <- c("code", "activity")

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
colnames(subject_test) <- "subject"

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
colnames(subject_train) <- "subject"

x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
colnames(x_test) <- features$functions
colnames(y_test) <- "code"

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
colnames(x_train) <- features$functions
colnames(y_train) <- "code"

# 1. Merges the training and the test sets to create one data set.
# a. rbind -> binding the x_train and x_test in sequence on rows to form final x_data dataset.
# b. rbind -> binding the y_train and y_test in sequence on rows to form final y_data dataset.
# c. rbind -> binding the subject_train and subject_test in sequence.
# d. cbind -> Binding the x_data, y_data and Subject on column to produce single data set.
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)
  
final_data <- cbind(subject_data, x_data, y_data)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Create a vector of only mean and std, use the vector to subset.
result_col <- grep("mean()|std()", colnames(final_data)) 

# Create a vector of only subject and code, use the vector to subset
result_col1 <- grep("subject|code",colnames(final_data))

# result data will have subject, code from result_col1 and 
# columns containing mean and std from result_col and pass the column list
result_data <- final_data[,c(result_col1, result_col)]

#3. Uses descriptive activity names to name the activities in the data set.

result_data$code <- activities[result_data$code, 2]

#4. Appropriately labels the data set with descriptive variable names.
# Taking out the column names of result_data and assign to vector for operations
# at end assign the clean_names vector to colnames of result_data 

clean_names <- gsub("[()]","", colnames(result_data))
clean_names[2] <- "Activity"
clean_names <- gsub("Acc", "Accelerometer", clean_names)
clean_names <- gsub("Gyro", "Gyroscope", clean_names)
clean_names <- gsub("BodyBody", "Body", clean_names)
clean_names <- gsub("Mag", "Magnitude", clean_names)
clean_names <- gsub("^t", "Time", clean_names)
clean_names <- gsub("^f", "Frequency", clean_names)
clean_names <- gsub("tBody", "TimeBody", clean_names)
clean_names <- gsub("-mean", "Mean", clean_names, ignore.case = TRUE)
clean_names <- gsub("-std", "STD", clean_names, ignore.case = TRUE)
clean_names <- gsub("-freq", "Frequency", clean_names, ignore.case = TRUE)
clean_names <- gsub("angle", "Angle", clean_names)
clean_names <- gsub("gravity", "Gravity", clean_names)

colnames(result_data) <- clean_names

#5. From the data set in step 4, creates a second, independent tidy data set 
#  with the average of each variable for each activity and each subject.
# grouping done on activity and subject then summarise the mean for that grouping.

avg_activity_subject <- result_data %>%
            group_by(subject, Activity) %>%
            summarise_all(funs(mean))

# Writing the file.
write.table(avg_activity_subject,file = "Mean_on_subject_n_activity.txt",row.names = F, sep = ",")
