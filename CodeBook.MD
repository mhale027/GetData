#Course Project for Getting and Cleaning Data

## Overview:
In this project I will be using the data from the UCI machine learning repository titled, "Human Activity Recognition Using Smartphones Data Set" to demonstrate my ability to effectively clean a data frame to create a tidy data set.

## Experiment Details
In this data set, the experimentors took 30 subjects and gave them Samsung Galaxy S IIs to wear around the waist during the six different activities. The smartphones are manufactured with gyroscopes and accelerometers which allow for the accurate detection of both linear and rotational acceleration. The data was transmitted by the smartphones where 30% of the subjects were used for the test data, and 70% were used for the training data. Components were combined into magnitudes, and calculus was used to determine the jerk, which is the time rate of change in the acceleration. Means and standard deviations were also taken for each measurement.


## Source Files
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
        
## Variables 
The activity variables are: 
        1. 'WALKING'
        2. 'WALKING_UPSTAIRS'
        3. 'WALKING_DOWNSTAIRS'
        4. 'sITTING'
        5. 'STANDING'
        6. 'LAYING'
        
All 30 subjects have individual subject ids, from 1 to 30.

The feature variables are considerably more numerous than the activities, but can be broken down by a few markers in the names of the variables.
Features that begin with "t", like tBodyAcc_mean_X, are time domain variables, where as the features that begin with "f", like fBodyGyro_std_Y, have had a fast fourier transform applied and are frequency domain variables.
Linear accelerations in the X, Y, and Z directions are calculated by gyroscopes inside the smartphones, and are noted in the variables with the "Acc" marker. Gravity must be subtracted from the measurements so gravity related variables have the marker "Gravity". 
Jerk is the rate of change in acceleration, and has variable marker "Jerk".
Rotational acceleration is measured in the variables with marker "Gyro".
When measurements from the X, Y and Z components are converted into a magnitude, the variables have the marker "Mag"
The standard deviation and mean of each component are taken and their variables have the markers "std", and "mean" respectively. With all these measurements and errors, a simple application of Newtonian physics can give very accurate indicators, with errors, of velocity, linear acceleration and rotational acceleration. 


## Code Transformations
First the source files are loaded, and read out to variables.
Labels are loaded and merged with the data.
Features are loaded and used to properly label the variables.
Test and training sets are merged into one data frame.
The data frame is refined to only contain the variables associated with the stdev and mean.
Two for loops are used to select first the subject, and then the activity.
The mean of each individuals activities are computed and stored in a vector.
The vector is bound over the loop to produce a data frame of a tidy data set.
