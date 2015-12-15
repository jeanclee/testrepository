# You should create one R script called run_analysis.R that does the following.
# 
# Merges the training and the test sets to create one data set.
# Extracts columns containing mean and standard deviation for each measurement 
## (Hint: Since some feature/column names are repeated, you may need to use the 'make.names()' function in R)
# Creates variables called 'ActivityLabel' and 'ActivityName' that label all observations with the corresponding activity labels and names respectively
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


###### set up 

library(dplyr)
library(reshape2)

# import test and training datasets, activity labels, and subjects
features <- read.table("har_data/features.txt")
activities <- read.table ("har_data/activity_labels.txt")
testdata <- read.table("har_data/test/X_test.txt")
testactivity <- read.table("har_data/test/y_test.txt")
testsubject <-read.table("har_data/test/subject_test.txt")
traindata <- read.table("har_data/train/X_train.txt")
trainactivity <- read.table("har_data/train/y_train.txt")
trainsubject <-read.table("har_data/train/subject_train.txt")
ls()

# convert testdata and traindata to local dataframes
testdata <- tbl_df(testdata)
traindata <- tbl_df(traindata)

View(features)

##########################

### 1) combine test and training datasets; verify with record counts

nrow(testdata) + nrow(traindata)

all.data <- rbind(testdata,traindata)
all.activity <- rbind(testactivity, trainactivity)
all.subject <- rbind(testsubject, trainsubject)

nrow(all.data) == nrow(testdata) + nrow(traindata)
nrow(all.activity) == nrow(testactivity) + nrow(trainactivity)
nrow(all.subject) == nrow(testsubject)+ nrow(trainsubject)

head(all.data)
#########################


### 2) Extracts columns containing mean and standard deviation for each measurement


# make syntactically valid variable names
varnames <- gsub('mean\\(\\)','Meanvar',features$V2)
varnames <- gsub('std\\(\\)','Stdvar',varnames)
varnames <- make.names(varnames)
varnames <- make.unique(varnames)
varnames

# remove extra periods in variable names

varnames <- gsub('\\.','', varnames)

# rename all.data with new column names
colnames(all.data) <- varnames
str(all.data)

# extract mean and standard deviation columns into meansd.data dataframe
meansd.data <- all.data %>% select (matches('Meanvar|Stdvar'))
str(meansd.data)

############################

### 3) Creates variables called 'ActivityLabel' and 'ActivityName' that label all observations with the corresponding activity labels and names respectively
# join activities to all.activity data
activities.labeled <- left_join(all.activity, activities, by = "V1")
# rename columns
colnames(activities.labeled) <- c("ActivityLabel","ActivityName")
head(activities.labeled)
# cbind activity labels and names to all.data
nrow(meansd.data)
nrow(activities.labeled)
meansd.data <- cbind(meansd.data, activities.labeled)

str(meansd.data)

#############################

### 4) creates a second, independent tidy data set with the 
### average of each variable for each activity and each subject.

#add subject ID to alldata.labeled
colnames(all.subject) <- c("subjectID")
meansd.data <-cbind(meansd.data,all.subject)
meansd.data
str(meansd.data)

# average of each mean/standard deviation variable grouped by subject and activity 
summary <- meansd.data %>% group_by(ActivityLabel,ActivityName,subjectID) %>% summarize_each(funs(mean))

# melt data and create tidy dataset
tidy_data <- melt(summary, id=c("ActivityLabel","ActivityName","subjectID"))
str(tidy_data)
write.table(tidy_data, "tidy_data.CSV", sep=",", row.names=FALSE)


                       