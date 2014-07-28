library(data.table)

runAnalysis <- function() {
  if (!file.exists("UCI HAR Dataset")) {
    stop("UCI HAR Dataset folder (raw data) must be in the current folder")
  }
  setwd("UCI HAR Dataset")
  
  print("Reading labels...")
  # Read activity labels
  if (!file.exists("activity_labels.txt")) {
    stop("activity_labels.txt")
  }
  labels <- fread("activity_labels.txt", header = FALSE)
  setnames(labels, c("LABEL_LEVEL", "LABEL_NAME"))
  print(labels)
  
  print("Reading features...")
  # Read features
  if (!file.exists("features.txt")) {
    stop("Couldn't find features.txt")
  }
  features <- fread("features.txt", header = FALSE)
  setnames(features,  c("FEATURE_COL", "FEATURE_NAME"))
  
  # Find required indexes
  requiredFeatures <- features[grep("-mean\\(\\)|-std\\(\\)", 
                           features[, features$FEATURE_NAME], )]
  requiredFeatures$FEATURE_NAME <- gsub("-mean\\(\\)", "_MEAN", 
                                        requiredFeatures$FEATURE_NAME)
  requiredFeatures$FEATURE_NAME <- gsub("-std\\(\\)", "_STD", 
                                        requiredFeatures$FEATURE_NAME)
  testData <- loadData("test", requiredFeatures)
  trainData <- loadData("train", requiredFeatures)
  
  # Merges the training and the test sets to create one data set.
  mergedData <<- rbind(testData, trainData)
  
  #Uses descriptive activity names to name the activities in the data set
  mergedData$ACTIVITY <<- factor(mergedData$ACTIVITY, 
                                levels = labels$LABEL_LEVEL, 
                                labels = labels$LABEL_NAME)
  
  # Creates a second, independent tidy data set with the average of each 
  # variable for each activity and each subject. 
  summaryData <<- aggregate(mergedData[, 3:ncol(mergedData), with = FALSE], by = 
                              list(ACTIVITY = mergedData$ACTIVITY, 
                                   SUBJECT = mergedData$SUBJECT), 
                            FUN = mean)
    
  setwd("..")
  
  #Write files
  
  if(!file.exists("tidyData")) {
    warning("tidyData folder not found, creating it now")
    dir.create("tidyData")
  }
  
  write.csv(mergedData, "tidyData/tidy-set.csv", row.names = FALSE)
  write.csv(summaryData, "tidyData/summary-set.csv", row.names = FALSE)
 
}

loadData <- function(type, requiredFeatures) {
  print(paste("Reading data from", type, "folder"))
  if (!file.exists(type)) {
    stop(paste("Couldn't find", type, "folder"))
  }
  setwd(type)
  
  # Path to the the data file
  xFile <- paste0("X_",type,".txt")
  # Path to the activity file
  yFile <- paste0("y_",type,".txt")  
  # Path to the subject file
  subjectFile  <- paste0("subject_", type ,".txt")
  
  if (!file.exists(yFile)) {
    stop(paste("Couldn't find", subjectFile, "in raw data"))
  }
  if (!file.exists(xFile)) {
    stop(paste("Couldn't find", xFile, "in raw data"))
  }
  if (!file.exists(yFile)) {
    stop(paste("Couldn't find", yFile, "in raw data"))
  }
  
  # Read the subject file
  subjectData <- fread(subjectFile, header = FALSE)
  setnames(subjectData, "SUBJECT")
  dataSize <- nrow(subjectData)
  
  # Read the Activity file
  yData <- fread(yFile, header = FALSE, nrows = dataSize)
  setnames(yData, "ACTIVITY")
  
  # Read the data file
  # Coudln't read the file with fscan because it starts with an empty space
  # see http://stackoverflow.com/questions/23833294/data-tablefread-doesnt-like-missing-values-in-first-column
  xData <- read.table(xFile, header = FALSE, colClasses = rep("numeric", 561), 
                      quote="", stringsAsFactors = FALSE, comment.char="", 
                      nrows = dataSize)
  
  # Extracts only the measurements on the mean and standard deviation for each measurement. 
  xData <- xData[, requiredFeatures$FEATURE_COL]
  setnames(xData, requiredFeatures$FEATURE_NAME)
  
  # Up one level
  setwd("..")
  
  cbind(subjectData, yData, xData)

}

## Auto-run the analisys function
runAnalysis()