CodeBook for average subset.

==========================================
One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 
==========================================================================================
This average subset privide:
a)180 objects with 81 variables
b)The last two columns is describled for activities and subjects.
  Activities are as follow WALKING WALKING_UPSTAIRS WALKING_DOWNSTAIRS SITTING STANDING LAYING
  Subjects are describled as numbers from 1 to 30.
c)The others columns are the average of each items describled by activities and subjects in the original dataset.
  The items are as follows:
  "tBodyAcc.mean...X"               "tBodyAcc.mean...Y"               "tBodyAcc.mean...Z"              
  "tGravityAcc.mean...X"            "tGravityAcc.mean...Y"            "tGravityAcc.mean...Z"           
  "tBodyAccJerk.mean...X"           "tBodyAccJerk.mean...Y"           "tBodyAccJerk.mean...Z"          
  "tBodyGyro.mean...X"              "tBodyGyro.mean...Y"              "tBodyGyro.mean...Z"             
  "tBodyGyroJerk.mean...X"          "tBodyGyroJerk.mean...Y"          "tBodyGyroJerk.mean...Z"         
  "tBodyAccMag.mean.."              "tGravityAccMag.mean.."           "tBodyAccJerkMag.mean.."         
  "tBodyGyroMag.mean.."             "tBodyGyroJerkMag.mean.."         "fBodyAcc.mean...X"              
  "fBodyAcc.mean...Y"               "fBodyAcc.mean...Z"               "fBodyAcc.meanFreq...X"          
  "fBodyAcc.meanFreq...Y"           "fBodyAcc.meanFreq...Z"           "fBodyAccJerk.mean...X"          
  "fBodyAccJerk.mean...Y"           "fBodyAccJerk.mean...Z"           "fBodyAccJerk.meanFreq...X"      
  "fBodyAccJerk.meanFreq...Y"       "fBodyAccJerk.meanFreq...Z"       "fBodyGyro.mean...X"             
  "fBodyGyro.mean...Y"              "fBodyGyro.mean...Z"              "fBodyGyro.meanFreq...X"         
  "fBodyGyro.meanFreq...Y"          "fBodyGyro.meanFreq...Z"          "fBodyAccMag.mean.."             
  "fBodyAccMag.meanFreq.."          "fBodyBodyAccJerkMag.mean.."      "fBodyBodyAccJerkMag.meanFreq.." 
  "fBodyBodyGyroMag.mean.."         "fBodyBodyGyroMag.meanFreq.."     "fBodyBodyGyroJerkMag.mean.."    
  "fBodyBodyGyroJerkMag.meanFreq.." "tBodyAcc.std...X"                "tBodyAcc.std...Y"               
  "tBodyAcc.std...Z"                "tGravityAcc.std...X"             "tGravityAcc.std...Y"            
  "tGravityAcc.std...Z"             "tBodyAccJerk.std...X"            "tBodyAccJerk.std...Y"           
  "tBodyAccJerk.std...Z"            "tBodyGyro.std...X"               "tBodyGyro.std...Y"              
  "tBodyGyro.std...Z"               "tBodyGyroJerk.std...X"           "tBodyGyroJerk.std...Y"          
  "tBodyGyroJerk.std...Z"           "tBodyAccMag.std.."               "tGravityAccMag.std.."           
  "tBodyAccJerkMag.std.."           "tBodyGyroMag.std.."              "tBodyGyroJerkMag.std.."         
  "fBodyAcc.std...X"                "fBodyAcc.std...Y"                "fBodyAcc.std...Z"               
  "fBodyAccJerk.std...X"            "fBodyAccJerk.std...Y"            "fBodyAccJerk.std...Z"           
  "fBodyGyro.std...X"               "fBodyGyro.std...Y"               "fBodyGyro.std...Z"              
  "fBodyAccMag.std.."               "fBodyBodyAccJerkMag.std.."       "fBodyBodyGyroMag.std.."         
  "fBodyBodyGyroJerkMag.std.."
  
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

========================================================================
To know how to get this result refer to https://github.com/NeighborhoodWang/Getting-and-Cleaning-Data-Project
