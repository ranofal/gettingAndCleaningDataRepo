#How to run the scripts:
set the variable defaultWoringArea <- <path_to_your_work_area> 
then load the script run_analysis.R to your R Studio and run it after changing the above variable to your work area (line 40)

Here are the steps the project:
#This project requires the following packages 
	require("data.table",character.only = TRUE, quietly = TRUE)
	require("reshape2",character.only = TRUE, quietly = TRUE) 
	require("knitr",character.only = TRUE, quietly = TRUE)
	require("markdown",character.only = TRUE, quietly = TRUE)
##Gobal Variables 
###YMAT2 <- matrix(seq(1,2),nrow=2)
###YMAT3 <- matrix(seq(1,3),nrow=3)
###My Functions 
Function to  convert a file to Data Table 
There is a problem while using fread function, found a solution in the internet
	convertFileToDataTable <- function (fileName)
Function return True if a reqular expression regularExp in the column name feature
otherwise returns False
	myGrep <- function(regularExp)
Create Matrix 
createMatrix <- function(vList,numOfCat,labelList)

#constant values: Data URL and name of the directory after unzip the zip files 

URL <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	zipDirName <- "UCI HAR Dataset"
	initDataSetName <- "InitDataSet.zip"
	harusDir <-"Human_Activity_Recognition_Using_Smartphones"
	harusOutFile <- "Human_Activity_Recognition_Using_Smartphones.txt"

This the value that need to be changed:
#This is the line that needs to be changed to point to the user working area
defaultWoringArea <- "C:/ramziPri/R_workarea/getting_and_cleaning_data/course_project"

#Prepare 1 for Step 1: download the the zip file that contains all the data, then try to unzip it.
create a new folder under workArea the name of the folder will be "Human_Activity_Recognition_Using_Smartphones"
check if the folder exist and if not create it
First check if the zip file is already downloaded and if not download the zip file. This to save some time
#check if the initDataSetName is unzipped already, if not unzip it. 
#Now setting the new working area 
#Prepare 2 for Step 1: after extracting all the data file, need to read in all the data.
#Prepare 3 for Step 1: Reading the following subject files and the activity files For Y and X 
	train/subject_train.txt
	test/subject_test.txt

#Step 1: Merge the training and test sets

Merge columns of the Subject and Acitivey then merge this data to the DataTableX, then setting the kyes 
#Setting the key
	setkey(DataTableX,subject,activityNumber)
#Step 2: Extract only the Mean and Standard Deviation 
First read features.txt file which contains the name of all columns,then find the name of the features 
#Subset the measurement for the mean and standard deviation
Looking for any name that contains the following expression .*mean().* or .*std().* 
where .* is any string then string "mean()" then any string after that e.g.: tBodyAcc-mean()-X
In Table DataTableX the columns names are V<num> where num is a value from 1 to 561
In Table dataTableFeatures the columns names are simply <num> where num is a value from 1 to 561
#This is to add another to match DataTableX
dataTableFeatures$featureCode <- dataTableFeatures[,paste0("V",featureNumber)]
Create a vector of all names which include the keys of DataTableX and featureCodes from the list[V1, V2, V3.....V561]
setnames(dataTableActivityNames,names(dataTableActivityNames),c("activityNumber","activityName")) 
head(dataTableActivityNames)

   activityNumber       activityName
1:              1            WALKING
2:              2   WALKING_UPSTAIRS
3:              3 WALKING_DOWNSTAIRS
4:              4            SITTING
5:              5           STANDING
6:              6             LAYING
#Step 3: Extract Desciptive Activity Names and to name the activities in the DataSet.
Convert the default names to "activityNumber" and "activityName" instead of V1 and V2 respectively

#Step 4 Label the data set with a descriptive variables names
head(dataTableActivityNames,2)
activityNumber       activityName
1:              1            WALKING
2:              2   WALKING_UPSTAIRS
3:              3 WALKING_DOWNSTAIRS
head(DataTableX,2)
subject activityNumber        V1          V2          V3         V4          V5         V6       V41        V42
1:       1              1 0.2820216 -0.03769622 -0.13489730 -0.3282802 -0.13715339 -0.1890859 0.9453028 -0.2459414
2:       1              1 0.2558408 -0.06455003................

Merge the 2 datatable by activityNumber
Setting the keys subject, activityNumber, and activityName,create a new table from short_wide to tall_narrow
DataTableX <- merge(DataTableX,dataTableActivityNames,by="activityNumber",all.x=TRUE)

Now merge activity name to the new DataTable 
DataTableX <- merge(DataTableX,dataTableFeatures[,list(featureNumber,featureCode,featureName)],by="featureCode", all.x=TRUE)
Add 2 new variables activity and feature equivalent to activityName and featureName respectively
head(DataTableX,2)
featureCode subject activityNumber activityName     value featureNumber       featureName activity           feature
1:          V1       1              1      WALKING 0.2820216             1 tBodyAcc-mean()-X  WALKING tBodyAcc-mean()-X
2:          V1       1              1      WALKING 0.2558408             1 tBodyAcc-mean()-X  WALKING tBodyAcc-mean()-X



# Features with 1 category
DataTableX$featJerk <- factor(myGrep("Jerk"), labels=c(NA, "Jerk"))
DataTableX$featMagnitude <- factor(myGrep("Mag"), labels=c(NA, "Magnitude"))

# Features with 2 category
DataTableX$featJerk <- factor(myGrep("Jerk"), labels=c(NA, "Jerk"))
DataTableX$featMagnitude <- factor(myGrep("Mag"), labels=c(NA, "Magnitude"))
DataTableX$featDomain <- createMatrix(c(myGrep("^t"),myGrep("^f")),numCategories,c("Time", "Freq"))
DataTableX$featInstrument <- createMatrix(c(myGrep("Acc"),myGrep("Gyro")),numCategories,c("Accelerometer", "Gyroscope"))
DataTableX$featAcceleration <- createMatrix(c(myGrep("BodyAcc"),myGrep("GravityAcc")),numCategories,c(NA,"Body", "Gravity"))
DataTableX$featVariable <- createMatrix(c(myGrep("mean\\(\\)"),myGrep("std\\(\\)")),numCategories,c("Mean", "SD"))

# Features with 2 category
DataTableX$featAxis <- createMatrix(c(myGrep("\\-X"),myGrep("\\-Y"),myGrep("\\-Z")),numCategories,c(NA,"X", "Y","Z"))

#Step 5 creating DataTableXTidy this is independent tidy data 
##printing sturcture and summary and some combination
#Saving the Tidy data 
After saving the data the following the structure and Summary of the Tidy Data.
> str(DataTableXTidy)
Classes ‘data.table’ and 'data.frame':	11880 obs. of  11 variables:
 $ subject         : int  1 1 1 1 1 1 1 1 1 1 ...
 $ activity        : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ featDomain      : Factor w/ 2 levels "Time","Freq": 1 1 1 1 1 1 1 1 1 1 ...
 $ featAcceleration: Factor w/ 3 levels NA,"Body","Gravity": 1 1 1 1 1 1 1 1 1 1 ...
 $ featInstrument  : Factor w/ 2 levels "Accelerometer",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ featJerk        : Factor w/ 2 levels NA,"Jerk": 1 1 1 1 1 1 1 1 2 2 ...
 $ featMagnitude   : Factor w/ 2 levels NA,"Magnitude": 1 1 1 1 1 1 2 2 1 1 ...
 $ featVariable    : Factor w/ 2 levels "Mean","SD": 1 1 1 2 2 2 1 2 1 1 ...
 $ featAxis        : Factor w/ 4 levels NA,"X","Y","Z": 2 3 4 2 3 4 1 1 2 3 ...
 $ count           : int  50 50 50 50 50 50 50 50 50 50 ...
 $ average         : num  -0.0166 -0.0645 0.1487 -0.8735 -0.9511 ...
 - attr(*, "sorted")= chr  "subject" "activity" "featDomain" "featAcceleration" ...
 - attr(*, ".internal.selfref")=<externalptr> 
> 
> summary(DataTableXTidy)
    subject                   activity    featDomain  featAcceleration       featInstrument featJerk      featMagnitude  featVariable
 Min.   : 1.0   LAYING            :1980   Time:7200   NA     :4680     Accelerometer:7200   NA  :7200   NA       :8640   Mean:5940   
 1st Qu.: 8.0   SITTING           :1980   Freq:4680   Body   :5760     Gyroscope    :4680   Jerk:4680   Magnitude:3240   SD  :5940   
 Median :15.5   STANDING          :1980               Gravity:1440                                                                   
 Mean   :15.5   WALKING           :1980                                                                                              
 3rd Qu.:23.0   WALKING_DOWNSTAIRS:1980                                                                                              
 Max.   :30.0   WALKING_UPSTAIRS  :1980                                                                                              
 featAxis      count          average        
 NA:3240   Min.   :36.00   Min.   :-0.99767  
 X :2880   1st Qu.:49.00   1st Qu.:-0.96205  
 Y :2880   Median :54.50   Median :-0.46989  
 Z :2880   Mean   :57.22   Mean   :-0.48436  
           3rd Qu.:63.25   3rd Qu.:-0.07836  
           Max.   :95.00   Max.   : 0.97451 
		   
#Code Book		   
#Variable list and descriptions

Variable name	Description
subject	ID the subject who performed the activity for each window sample. Its range is from 1 to 30.
activity	Activity name
featDomain	Feature: Time domain signal or frequency domain signal (Time or Freq)
featInstrument	Feature: Measuring instrument (Accelerometer or Gyroscope)
featAcceleration	Feature: Acceleration signal (Body or Gravity)
featVariable	Feature: Variable (Mean or SD)
featJerk	Feature: Jerk signal
featMagnitude	Feature: Magnitude of the signals calculated using the Euclidean norm
featAxis	Feature: 3-axial signals in the X, Y and Z directions (X, Y, or Z)
featCount	Feature: Count of data points used to compute average
featAverage	Feature: Average of each variable for each activity and each subject
