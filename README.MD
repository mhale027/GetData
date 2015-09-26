#Getting and Cleaning Data Course Project README

#Author
Matthew Hale mhale027@gmail.com

##Data
In this project, I will be using the data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

##Source Files
The files used in creating the tidy data set are as follows        
        * 'activity_labels.txt': Lists the activity labels.
        * 'features_info.txt': Lists the variables and give features and descriptions.
        * 'features.txt': List the individual features.
        * 'train/X_train.txt': X raining set.
        * 'train/y_train.txt': Y training labels.
        * 'test/X_test.txt': X test set.
        * 'test/y_test.txt': Y test labels.
        * 'test/subject_test.txt': Lists the subjects of the test set.
        * 'train/subject_train.txt': Lists the subjects of the training set.
        
##Script Breakdown
        * All files should be cloned from the github repository and run in the root directory.
        * First the source files are loaded, and read out to variables.
        * Labels are loaded and merged with the data.
        * Features are loaded and used to properly label the variables.
        * Test and training sets are merged into one data frame.
        * The data frame is refined to only contain the variables associated with the stdev and mean.
        * Two for loops are used to select first the subject, and then the activity.
        * The mean of each individuals activities are computed and stored in a vector.
        * The vector is bound over the loop to produce a data frame of a tidy data set.