# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.

run_analysis <- function(){
  
  # Load used packages
  library(data.table)
  library(reshape2)
  
  # Load the subject files
  fSubjectTest <- file.path(getwd(), "UCI HAR Dataset/test/subject_test.txt")
  df_subject_test <- read.table(fSubjectTest) ##reads as data.frame
  dt_subject_test <- data.table(df_subject_test) ##converts into a data.table
  
  fSubjectTrain <- file.path(getwd(), "UCI HAR Dataset/train/subject_train.txt")
  df_subject_train <- read.table(fSubjectTrain) ##reads as data.frame
  dt_subject_train <- data.table(df_subject_train) ##converts into a data.table
  
  # Load the actitivies files
  fActivityTest <- file.path(getwd(), "UCI HAR Dataset/test/y_test.txt")
  df_activity_test <- read.table(fActivityTest)
  dt_activity_test <- data.table(df_activity_test)
  
  fActivityTrain <- file.path(getwd(), "UCI HAR Dataset/train/y_train.txt")
  df_activity_train <- read.table(fActivityTrain)
  dt_activity_train <- data.table(df_activity_train)
  
  # Load test and train data into R
  ftest <- file.path(getwd(), "UCI HAR Dataset/test/X_test.txt")
  df_test <- read.table(ftest) 
  dt_test <- data.table(df_test) 
  
  ftrain <- file.path(getwd(), "UCI HAR Dataset/train/X_train.txt")
  df_train <- read.table(ftrain)
  dt_train <- data.table(df_train)
  
  # Merge the training and the tests set
  ## Concatenate data labels - use function rbind()
  dt_subject <- rbind(dt_subject_test, dt_subject_train)
  setnames(dt_subject, "V1", "subject")
  
  dt_activity <- rbind(dt_activity_test, dt_activity_train)
  setnames(dt_activity, "V1", "activityID")
  
  dt <- rbind(dt_test, dt_train)
  
  # Merge columns - use function cbind():
  # Merge subject and activity data
  # Merge the result with test and train data 
  
  dt_subject <- cbind(dt_subject, dt_activity)
  dt <- cbind(dt_subject, dt)
  
  ## Set key
  setkey(dt, subject, activityID)
  
  #Extract only mean and standard deviation
  
  ## Read the features.txt file. This tells which variables in dt are measurements 
  ## for the mean and standard deviation.
  ffeatures <- file.path(getwd(), "UCI HAR Dataset/features.txt")
  df_features <- read.table(ffeatures, col.names = c("featureID", "featureName"))
  dt_features <- data.table(df_features)
  
  ## Subset only measurements for the mean and standard deviation.
  dt_features <- dt_features[grepl("mean\\(\\)|std\\(\\)", featureName)]
  
  ## Convert the column numbers to a vector of variable names matching columns in dt.
  dt_features$featureCode <- dt_features[, paste0("V", featureID)]
  head(dt_features)
  dt_features$featureCode
  
  ## Subset these variables using variable names
  select <- c(key(dt), dt_features$featureCode)
  dt <- dt[, select, with=FALSE]
  
  # Add descriptive Activity Name
  ## Read the actitivities.txt file.
  ## This will be used to add descriptive names to the activities.
  factivitiesnames <- file.path(getwd(), "UCI HAR Dataset/activity_labels.txt")
  df_activities_names <- read.table(factivitiesnames, col.names = c("activityID", "activityName"))
  dt_activities_names <- data.table(df_activities_names)
  
  ## Merge activity labels
  dt <- merge(dt, dt_activities_names, by = "activityID", all.x = TRUE)
  
  ## Add activity name as a Key
  setkey(dt, subject, activityID, activityName)
  
  ## Melt the data table to reshape it from a short and wide format to a tall and narrow format.
  dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
  
  ## Merge activity name.
  ## Merge dt with dt_features by column featuresCode
  dt <- merge(dt, dt_features[, list(featureID, featureCode, featureName)], by="featureCode", all.x=TRUE)
  
  ## Create a new variable, activity that is equivalent to activityName as a factor class. 
  ## Create a new variable, feature that is equivalent to featureName as a factor class.
  dt$activity <- factor(dt$activityName)
  dt$feature <- factor(dt$featureName)
  
  ## Seperate features from featureName using the helper function grepthis
  grepthis <- function (regex) {
    grepl(regex, dt$feature)
  }
  
  ## Features with 2 categories
  n <- 2
  y <- matrix(seq(1, n), nrow=n)
  x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
  dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
  x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
  dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
  x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
  dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
  x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
  dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
  
  ## Features with 1 category
  dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
  dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
  
  ## Features with 3 categories
  n <- 3
  y <- matrix(seq(1, n), nrow=n)
  x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
  dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
  
  ## Check to make sure all possible combinations of feature are accounted for by all possible 
  ## combinations of the factor class variables.
  r1 <- nrow(dt[, .N, by=c("feature")])
  r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
  r1 == r2
  ## Yes. All possible combination of feature are accounted.
  
  # Create a tidy dataset
  ## Create a data set with the average of each variable for each activity and each subject.
  setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
  dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]
  
  write.table(dt, "dt.txt")
  write.table(dtTidy, "dt_tidy.txt")
  
  
}

