# merging of data sets (training and test) to create one data set
  var1 <- read.table("train/X_train.txt")
  var2 <- read.table("test/X_test.txt")
      X <- rbind(var1, var2)
  var1 <- read.table("train/subject_train.txt")
  var2 <- read.table("test/subject_test.txt")
      S <- rbind(var1, var2)
  var1 <- read.table("train/y_train.txt")
  var2 <- read.table("test/y_test.txt")
      Y <- rbind(var1, var2)
# extraction of measurements based on mean and standard deviation
  feat <- read.table("features.txt")
  ind_feat <- grep("-mean\\(\\)|-std\\(\\)", feat[, 2])
      X <- X[, ind_feat]
  names(X) <- feat[ind_feat, 2]
  names(X) <- gsub("\\(|\\)", "", names(X))
  names(X) <- tolower(names(X))
# naming activities
  activities <- read.table("activity_labels.txt")
  activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
      Y[,1] = activities[Y[,1], 2]
  names(Y) <- "activity"
# label data with activity names
  names(S) <- "subject"
  cleanse <- cbind(S, Y, X)
  write.table(cleanse, "cleaneddata.txt")
# tidy data set with avgs of each variable and sub
  uniqueSubjects = unique(S)[,1]
  numSubjects = length(unique(S)[,1])
  numActivities = length(activities[,1])
  numCols = dim(cleanse)[2]
  result = cleanse[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleanse[cleanse$subject==s & cleanse$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "dataaverages.txt")