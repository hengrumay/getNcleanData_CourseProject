## Assignment for Getting & Cleaning Data
# run_analysis.R -- coded & commented by h-rm_tan 22-24Oct2015

## NOTES:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# https://www.youtube.com/watch?v=XOEN9W05_4A

####

## --- LIBRARIES --------------------------------------------------------------
library(dplyr)

## --- SET PATHS --------------------------------------------------------------
# datapath <- "~/Documents/OwnCloud/Coursera_DataScienceTrack_JH/03_Getting&CleaningData/Assignment/UCI HAR Dataset/"
# setwd(datapath)


## --- READ FILES & ADD VARIABLE NAMES --------------------------------------------------------------
## --- variables
activityL <- read.table("activity_labels.txt")   # dim(activityL)  # [1] 6 2

activityF <- read.table("features.txt")   # dim(activityF)  # [1] 561   2

## ** resolving problem with "duplicate-VarNames" for subsequent use of select() --> make.names
FeatureNames <- make.names(activityF[,2], unique = TRUE, allow_ = TRUE)


## --- features -- activityF
trainX <- read.table("train/X_train.txt")   # dim(trainX)     # [1] 7352  561
names(trainX) <- FeatureNames #activityF[,2]

testX <- read.table("test/X_test.txt")   # dim(testX)      # [1] 2947  561
names(testX) <- FeatureNames #activityF[,2]


## --- activity -- activityL || activityL[trainY$activity,]
trainY <- read.table("train/y_train.txt")   # dim(trainY)     # [1] 7352    1
names(trainY) <- "activity"

testY <- read.table("test/y_test.txt")   # dim(testY)      # [1] 2947    1
names(testY) <- "activity"


## --- subjs
trainSubj <- read.table("train/subject_train.txt")   # dim(trainSubj)  # [1] 7352    1
names(trainSubj) <- "subjID"
trainSubj <- trainSubj %>% mutate(datatype = "train")   # dim(trainSubj)  # [1] 7352    2

testSubj <- read.table("test/subject_test.txt")   # dim(testSubj)   # [1] 2947    1
names(testSubj) <- "subjID"
testSubj <- testSubj %>% mutate(datatype = "test")   # dim(testSubj)   # [1] 2947    2


## --- COMBINE DATA --------------------------------------------------------------
## --- combineTRAINdata
trainD <- tbl_df(cbind(trainSubj,trainY,trainX))

## --- combineTRAINdata
testD <- tbl_df(cbind(testSubj,testY,testX))


## --- MERGE trainD & testD --------------------------------------------------------------
#1# Merges the training and the test sets to create one data set.
#4# Appropriately labels the data set with descriptive variable names. -- (see ** FeatureNames above)
D <- rbind(trainD,testD)

# # --- clear workingMEM...
# rm(list = c( ls(pattern = "^test"), ls(pattern = "^train") ) )

## --- EXTRACT mean & std values & LABEL activity with descripts --------------------------------------------------------------
#2# Extracts only the measurements on the mean and standard deviation for each measurement.
#3# Uses descriptive activity names to name the activities in the data set
#4# Appropriately labels the data set with descriptive variable names. -- (see ** FeatureNames above)

D1 <- D %>% select(subjID, datatype, activity, contains("mean"), contains("std") ) # dim: [10,299 x 86 +3]
D1$activity <- activityL[D1$activity,2] #%>% print


## --- MAKE TidyMeans --------------------------------------------------------------
#5# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

D2_tidymeans <- D1 %>% group_by(subjID, datatype, activity) %>% summarise_each(funs(mean), -subjID:-activity )

# --- write D2_tidymeans to working-directory...
write.table(D2_tidymeans, file = "outputD2_tidymeans.txt", row.name=FALSE, col.names = names(D2_tidymeans), sep = ",")

# # Save the file as "outputD2_tidymeans.txt" and read the file from its directory:
# outputD2_tidymeans <- read.csv("outputD2_tidymeans.txt")
# View(outputD2_tidymeans)

