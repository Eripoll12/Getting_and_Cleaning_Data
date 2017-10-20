# The script...
run_analysis <- function() {
      
      library(dplyr)
      
      # 0. read data
      x_test <- read.table("Assignment_course_3/test/X_test.txt")
      y_test <- read.table("Assignment_course_3/test/y_test.txt")
      x_train <- read.table("Assignment_course_3/train/X_train.txt")
      y_train <- read.table("Assignment_course_3/train/y_train.txt")
      subject_test <- read.table("Assignment_course_3/test/subject_test.txt")
      subject_train <- read.table("Assignment_course_3/train/subject_train.txt")
      features <- read.table("Assignment_course_3/features.txt")
      
      
      # 1. Merge the sets to create one data set.
      x <- merge(x_test, x_train, all=TRUE)
      colnames (x) <- features[,2] # Labels the data set columns with descriptive variable names
      y <- rbind(y_test, y_train)
      y <- rename_(y, activity = "V1") #rename the column name to "activity"
      subject <- rbind(subject_test, subject_train)
      subject <- rename_(subject, id = "V1") # rename the column name to "id"
      
      z <- cbind(subject, y, x) # One data set appropiately labeled
      
      # 2. Extracts the measurements on the mean and standard deviation for each measurement
      
      mean_std_vector <- grep("mean|std", names(z))
      extracted_mean_std <- cbind(subject, y, subset(z, select = mean_std_vector))
      
      # 3. Uses descriptive activity names to name the activities in the data set
      
      activity <- c("walking", "walking_upstairs", "walking_downstairs", "sitting", "standing", "laying")
      for (i in 1:6) {
            extracted_mean_std$activity <- gsub(i, activity[i], extracted_mean_std$activity)
            z$activity <- gsub(i, activity[i], z$activity)
      }
      
      # 4. Labels the data set with descriptive variable names
      
      # remove parentheses
      names(extracted_mean_std) <- gsub("\\(|\\)", "", names(extracted_mean_std), perl  = TRUE)
      
      # correct syntax
      names(extracted_mean_std) <- make.names(names(extracted_mean_std))
      
      #add descriptive names
      
      names(extracted_mean_std) <- gsub("mean", "Mean", names(extracted_mean_std))
      names(extracted_mean_std) <- gsub("std", "Std", names(extracted_mean_std))
      names(extracted_mean_std) <- gsub("Acc", "Acceleration", names(extracted_mean_std))
      names(extracted_mean_std) <- gsub("^t", "Time", names(extracted_mean_std))
      names(extracted_mean_std) <- gsub("^f", "Frequency", names(extracted_mean_std))
      names(extracted_mean_std) <- gsub("Freq", "Frequency", names(extracted_mean_std))

   
      # 5. creates a second, independent tidy data set with the average of each 
      # variable for each activity and each subject
      
      library(plyr)
      set_average <- ddply(extracted_mean_std, c("id","activity"), numcolwise(mean))
      write.table(set_average,file="tidydata.txt", row.name = FALSE)
      set_average
}
