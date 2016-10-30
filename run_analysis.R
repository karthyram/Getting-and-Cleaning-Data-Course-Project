# libary
library(dplyr)
library(data.table)
library(tidyr)
# Download of data-----------------------------------------------
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "20Dataset.zip")
download.file(url, f)
unzip(f)

# Fetching the activiy labels------------------------------------
actlabels <- read.table("UCI HAR Dataset/activity_labels.txt",header = FALSE, stringsAsFactors = FALSE)

# Fetching the feature labels------------------------------------
featurelabels <- read.table("UCI HAR Dataset/features.txt",header = FALSE, stringsAsFactors = FALSE)
 

##Loading the train data sets
#Loading Xtrain data with feature labels as column names
Xtrain <- read.table("UCI HAR Dataset/train/X_train.txt",header = FALSE)
names(Xtrain) <- featurelabels[,2]

#Loading ytrain data 
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt",header = FALSE)

#Loading subject train data
subtrain <- read.table("UCI HAR Dataset/train/subject_train.txt",header = FALSE)

#Combine data with subject and activity
traindata<-bind_cols(Xtrain,ytrain)
names(traindata)[562] <- "Activity"
traindata$Activity <- factor(traindata$Activity,labels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
traindata<-bind_cols(traindata,subtrain)
names(traindata)[563] <- "subject"

traincolNames <- colnames(traindata)
trainmean_and_std <- (grepl("Activity" , traincolNames) | 
                   grepl("subject" , traincolNames) | 
                   grepl("mean.." , traincolNames) | 
                   grepl("std.." , traincolNames) 
)

traindata <- traindata[ ,trainmean_and_std == TRUE]

##Loading the test data sets
#Loading Xtest data with feature labels as column names
Xtest <- read.table("UCI HAR Dataset/test/X_test.txt",header = FALSE)
names(Xtest) <- featurelabels[,2]

#Loading ytest data 
ytest <- read.table("UCI HAR Dataset/test/y_test.txt",header = FALSE)

#Loading subject train data
subtest <- read.table("UCI HAR Dataset/test/subject_test.txt",header = FALSE)

#Combine data with subject and activity
testdata<-bind_cols(Xtest,ytest)
names(testdata)[562] <- "Activity"
testdata$Activity <- as.factor(testdata$Activity)
testdata$Activity <- factor(testdata$Activity,labels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
testdata<-bind_cols(testdata,subtest)
names(testdata)[563] <- "subject"

# combining test and train data

testcolNames <- colnames(testdata)
testmean_and_std <- (grepl("Activity" , testcolNames) | 
                   grepl("subject" , testcolNames) | 
                   grepl("mean.." , testcolNames) | 
                   grepl("std.." , testcolNames) 
)

testdata <- testdata[ ,testmean_and_std == TRUE]

## Combining Train and Test data
Finaldata <- bind_rows(traindata,testdata)

### Making tody data
secTidySet <- Finaldata %>% 
              group_by(subject,Activity) %>%
              summarise_each(funs(mean)) %>%
              arrange(subject,Activity)

#### Writing the table
write.table(secTidySet, "tidy.txt", row.names = FALSE, quote = FALSE)

