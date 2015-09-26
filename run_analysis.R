run_analysis <- function(){
        
        #set working directory to renamed data file
        setwd("./UCI")
        
        #load libraries needed
        
        library(dplyr)
        
        #load test data
        
        test.x <- read.csv("test/X_test.txt",sep="",header=FALSE)
        test.y <- read.csv("test/Y_test.txt",sep="",header=FALSE)
        test.sub <- read.csv("test/subject_test.txt",sep="",header=FALSE)
        
        #load training data
        
        train.x <- read.csv("train/X_train.txt",sep="",header=FALSE)
        train.y <- read.csv("train/Y_train.txt",sep="",header=FALSE)
        train.sub <- read.csv("train/subject_train.txt",sep="",header=FALSE)
        
        #load the labels
        
        features <- read.csv("features.txt", sep=" ", header=FALSE)
        activities <- read.csv("activity_labels.txt", sep=" ", header=FALSE)
        
        #clean up the variable names and activities
        
        features <- gsub("[()]|-", " ", features[,2])
        activities <- gsub("_", " ", activities[,2])
        
        #merge the X files and label
        
        tt.x <- rbind(test.x, train.x)
        names(tt.x) <- features
        
        #merge the y files and label
        
        tt.y <- rbind(test.y, train.y)
        names(tt.y) <- "Activity"
        
        #merge the subject files and label
        
        tt.sub <- rbind(test.sub, train.sub)
        names(tt.sub) <- "Subject"
        
        #merge the data sets into one 
        
        data <- cbind(tt.sub, tt.y, tt.x)
        
        #refine the data frame to the columns containing mean and std
        
        std.mean.columns <- grep("mean|std|Activity|Subject", names(data))
        data <- data[,std.mean.columns]
        
        #change the activity labels
        
        data$Activity <- activities[data$Activity]
        
        #create new data frame with the averages from each subject in each activity
        
        data.averages <- NULL
        for (i in levels(factor(data$Subject))) {
                data.i <- filter(data, Subject==i)
                for (j in levels(factor(data$Activity))) {
                        data.i.j <- filter(data.i, Activity==j)
                        avgs <- apply(data.i.j[3:81],2,mean)
                        data.averages.i.j <- c(data.i.j[1,1:2], avgs)
                        data.averages <- rbind(data.averages, data.averages.i.j)
                }
        }
        
        write.table(data.averages, file="tidydata.txt", row.names=FALSE)
       
        
}

