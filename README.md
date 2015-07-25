---
title: "README.md"
author: "Mudit Maheshwari"
date: "July 22, 2015"
output: html_document
---

This section of the code loads dplyr and tidyr packages and sets the working directory
on my computer where all the datasets for the course project are stored
```{r}
library(dplyr)
library(tidyr)
getwd()
setwd("C:/R/Coursera/03_Getting and Cleaning Data/UCI HAR Dataset")
```

####STEP 1 Merges the training and the test sets to create one data set.

step 1a: In this section test and training data is imported from my computer into R. 

X_test is saved as test_measurement, Y_test as test_activity and subject_test as test_subject. 
The test_measurement represents 2,947 test records for 561 observations 
collected for 9 volunteers (30%), 
The test_activity values range from 1 to 6 and represent (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) activities for 2,947 test records. 
The test_subject values range from 1 to 30 and represent 9 test volunteers for 2947 test records

X_train is saved as train_measurement, Y_train as train_activity and subject_train as train_subject. 
The train_measurement represents 7,352 training records for 561 observations collected for 21 volunteers (70%).
The train_Activity values range from 1 to 6 and represent (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) activities for 7,352 training records. 
The train_subject values range from 1 to 30 for 21 training volunteers representing 7,352 records


```{r}

test_measurement <- read.table("C:/R/Coursera/03_Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt")
test_activity <- read.table("C:/R/Coursera/03_Getting and Cleaning Data/UCI HAR Dataset/test/Y_test.txt")
test_subject <- read.table("C:/R/Coursera/03_Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt")


train_measurement <- read.table("C:/R/Coursera/03_Getting and Cleaning Data/UCI HAR Dataset/train/X_train.txt")
train_activity <- read.table("C:/R/Coursera/03_Getting and Cleaning Data/UCI HAR Dataset/train/Y_train.txt")
train_subject <- read.table("C:/R/Coursera/03_Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt")

```

step 1b

####STEP 3 Uses descriptive activity names to name the activities in the data set. 
The step 3 (applying activity labels) is performed along with STEP 1 before STEP 2 (i.e. limiting the dataset to mean and sd measurements). 


Importing activity labels (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING,         STANDING, LAYING) into R

```r{}
activity_labels <- read.table("C:/R/Coursera/03_Getting and Cleaning Data/UCI HAR Dataset/activity_labels.txt")
```

Applying activity labels (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) on test_activity and train_activity datasets

```r{}
test_activity <- test_activity %>% inner_join (activity_labels, by = c("V1" = "V1"))  %>%
                    rename(ActivityID = V1, Activitylabel = V2) 

train_activity <- train_activity %>% inner_join (activity_labels, by = c("V1" = "V1"))  %>%
                    rename(ActivityID = V1, Activitylabel = V2) 
```

step 1c
Renaming V1 column name on test and train subject datasets to SubjectID (range 1 to 30)

```r{}
test_subject <- rename(test_subject, SubjectID = V1)
train_subject <- rename(train_subject, SubjectID = V1)
```
step 1d
Column Binding/Combining measurement, activity and subject datasets test and train separately

```r{}
test <- cbind(test_measurement, test_activity, test_subject)
train <- cbind(train_measurement, train_activity, train_subject)
```
step 1e
adding SubjectType column to identify test and training records

```r{}
test <- mutate(test, SubjectType = "Test")
train <- mutate(train, SubjectType = "Train")
```
step 1f
Row Binding/combining test and train datasets into one dataset.
COMPLETED STEP 1 and STEP 3 (merging datasets and applying activity labels)

```r{}
test_train <- rbind(test,train)
```



####STEP 2 Extracts only the measurements on the mean and standard deviation for each measurement. 


Convert measurement columns 1 thru 561 into rows

```r{}
test_train_gather <- gather(test_train, "measurement", "n", 1:561)
```

Importing features (1 to 561 variable names) into R

```r{}
features <- read.table("C:/R/Coursera/03_Getting and Cleaning Data/UCI HAR Dataset/features.txt")
```

adding V in front of numbers 1 to 561 of features table so that we can join with the 
test_train_gather dataset using values V1 thru V561
```r{}
features <- mutate(features, measure = "V")
features <- unite(features, measure1, measure, V1, sep ="")
```
extract the column numbers for mean and standard deviation columns
```r{}
mean_sd_cols <- rbind(features[grep("std\\(\\)",features$V2 ),], 
      features[grep("mean\\(\\)",features$V2 ),])
```

limit test_train combined dataset to mean and standard deviation on each measurement
```r{}
test_train_gather <- test_train_gather %>% 
                      inner_join(mean_sd_cols, by = c("measurement" = "measure1"))
``` 



####STEP 3 (application of activity labels was completed in Step 1b)
```r{}
The code was completed as part of STEP 1 test and train activity datasets
```

####STEP 4 Appropriately labels the data set with descriptive variable names.

Renames column V2 with features and removes the column with V1 thru V561 labels. 
Reordered columns to have SubjectID, SubjectType, ActivityID, Activitylabel, features
and measurement

```r{}
test_train_measurement <- test_train_gather %>% 
                          select(-measurement) %>%
                          rename (features = V2, measurement = n) %>%
                          select (SubjectID, SubjectType, ActivityID,Activitylabel,
                                  features, measurement)
                          
```


####STEP 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Create an average of each measurement for each subject and activity
```r{}
Subject_Activity_Features_means <- test_train_measurement %>% 
                          group_by(SubjectID, SubjectType, ActivityID, 
                                    Activitylabel, features) %>%
                          summarise(Measurement_Average = mean(measurement))


write.table(Subject_Activity_means, "Subject_Activity_Features_means.txt", 
            row.names = FALSE)
```
