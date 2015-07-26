#This project requires the following packages 
require("data.table",character.only = TRUE, quietly = TRUE)
require("reshape2",character.only = TRUE, quietly = TRUE) 
require("knitr",character.only = TRUE, quietly = TRUE)
require("markdown",character.only = TRUE, quietly = TRUE)

###Gobal Variables 
YMAT2 <- matrix(seq(1,2),nrow=2)
YMAT3 <- matrix(seq(1,3),nrow=3)
###My Functions 
#Function to  convert a file to Data Table 
#There is a problem while using fread function, found a solution in the internet
convertFileToDataTable <- function (fileName) {
    dataFile <- read.table(fileName)
    dataTable <- data.table(dataFile) 
}

#Function return True if a reqular expression regularExp in the column name feature
#otherwise returns False
myGrep <- function(regularExp) {
    grepl(regularExp,DataTableX$feature) 
}
#Create Matrix 
createMatrix <- function(vList,numOfCat,labelList) {
    if (numOfCat == 2) {
        YMAT <- YMAT2
    } else { YMAT <- YMAT3}
    mat <- matrix(vList,ncol=nrow(YMAT)) 
    factor(mat %*% YMAT,labels=labelList) 
}

#constant values: Data URL and name of the directory after unzip the zip files 
#URL Name of the original data.

URL <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipDirName <- "UCI HAR Dataset"
 
#Prepare 1 for Step 1: download the the zip file that contains all the data, then try to unzip it.

defaultWoringArea <- "C:/ramziPri/R_workarea/getting_and_cleaning_data/course_project"
workArea <- getwd() #get the working area 
if (workArea != defaultWoringArea) {
    setwd(defaultWoringArea)
    workArea = getwd()
}
message("The current working area is -> ",workArea)
#create a new folder under workArea the name of the folder will be "Human_Activity_Recognition_Using_Smartphones"

harusDir <-"Human_Activity_Recognition_Using_Smartphones"
harusOutFile <- "Human_Activity_Recognition_Using_Smartphones.txt"

#check if the folder exist and if not create 

if (!file.exists(harusDir)) {
    dir.create(harusDir)
} else {
    msg <- paste("Directory: ",file.path(workArea,harusDir)," Already exists, NO need to create")
    message(msg)
}

initDataSetName <- "InitDataSet.zip" #name of the init data set 

#first check if the zip file is already downloaded and if not download the zip file
#This to save someo time
if (!file.exists(file.path(workArea,harusDir,initDataSetName))) {
    #download the Initial data to the file initDataSetName under newF
    download.file(URL,file.path(harusDir,initDataSetName))
} else {
    msg <- paste("zipFile: ",file.path(workArea,harusDir,initDataSetName), " Already exists, No need to download")
    message(msg)
}

#check if the initDataSetName is unzip already 
if (!file.exists(file.path(workArea,harusDir,zipDirName))) {
    #download the Initial data to the file initDataSetName under newF
    msg <- paste("Directory: ",file.path(workArea,harusDir,zipDirName)," Not found , need to unzip ",initDataSetName)
    message(msg)
    msg <- paste("Switching working directory to: " , file.path(workArea,harusDir))
    message(msg)
    setwd(file.path(workArea,harusDir)) 
    unzip(initDataSetName)
    
} else {
    msg <- paste("Directory: ",file.path(workArea,harusDir,zipDirName), " Already exists, No need to unzip")
    message(msg)
    
}
#Now setting the new working area 
msg <- paste("Switching working directory to: " , file.path(workArea,harusDir,zipDirName))
message(msg)
setwd(file.path(workArea,harusDir,zipDirName)) 
zWorkArea <-getwd() #Work area the contains all the ddata

#Prepare 2 for Step 1: after extracting all the data file, need to fill
fList <- list.files(zWorkArea,recursive=T)

print ("done")

#Prepare 3 for Step 1: Reading the following subject files and the activity files For Y and X 
# train/subject_train.txt
# test/subject_test.txt

dataTableSubjectTrain <- convertFileToDataTable(file.path(zWorkArea,"train","subject_train.txt"))
dataTableSubjectTest  <- convertFileToDataTable(file.path(zWorkArea,"test" ,"subject_test.txt"))

dataTableActivity_Y_Train <- convertFileToDataTable(file.path(zWorkArea,"train","Y_train.txt"))
dataTableActivity_Y_Test  <- convertFileToDataTable(file.path(zWorkArea,"test" ,"Y_test.txt"))

dataTableActivity_X_Train <- convertFileToDataTable(file.path(zWorkArea,"train","X_train.txt"))
dataTableActivity_X_Test  <- convertFileToDataTable(file.path(zWorkArea,"test" ,"X_test.txt"))


#Step 1: Merge the training and test sets
#########################################

dataTableSubject <- rbind(dataTableSubjectTrain,dataTableSubjectTest) #bind Subject and Train into one table 
setnames(dataTableSubject, "V1","subject") #names the columns V1 and subject 

dataTableActivity <- rbind(dataTableActivity_Y_Train,dataTableActivity_Y_Test)
setnames(dataTableActivity, "V1","activityNumber") #names the columns V1 and activityNumber

DataTableX <- rbind(dataTableActivity_X_Train,dataTableActivity_X_Test)

#merge columns of the Subject and Acitivey 
dataTableSubject <-cbind(dataTableSubject,dataTableActivity)
#then merge this data to the DataTableX 
DataTableX <- cbind(dataTableSubject,DataTableX)

#now need to set the key
setkey(DataTableX,subject,activityNumber)

#Step 2: Extract only the Mean and Standard Deviation 
#first read features.txt file which contains the name of all columns 

dataTableFeatures <- convertFileToDataTable(file.path(zWorkArea,"features.txt"))
#now find the name of the features 
FeaturesNames <- names(dataTableFeatures)
setnames(dataTableFeatures, FeaturesNames,c("featureNumber","featureName"))

#Subset the measurement for the mean and standard deviation
#looking for any name that contains the following expression .*mean().* or .*std().* 
#where .* is any string then string "mean()" then any string after that e.g.: tBodyAcc-mean()-X

dataTableFeatures <- dataTableFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

#In Table DataTableX the columns names are V<num> where num is a value from 1 to 561
#In Table dataTableFeatures the columns names are simply <num> where num is a value from 1 to 561

head(dataTableFeatures,2)
head(DataTableX,2)
#This is to add another to match DataTableX
dataTableFeatures$featureCode <- dataTableFeatures[,paste0("V",featureNumber)]
head(dataTableFeatures,2)

#Create a vector of all names which include the keys of DataTableX and featureCodes from the list[V1, V2, V3.....V561]
names <- c(key(DataTableX),dataTableFeatures$featureCode) #
DataTableX <- DataTableX[,names,with=FALSE] 

#Step 3: Extract Desciptive Activity Names and to name the activities in the DataSet.
dataTableActivityNames <- convertFileToDataTable(file.path(zWorkArea,"activity_labels.txt"))
head(dataTableActivityNames) #The default names of the table is V1 and V2
#convert the names to "activityNumber and activityName 
setnames(dataTableActivityNames,names(dataTableActivityNames),c("activityNumber","activityName")) 
head(dataTableActivityNames) 

#Step 4 label the data set with a descriptive variables names
#head(dataTableActivityNames,2)
#activityNumber       activityName
#1:              1            WALKING
#2:              2   WALKING_UPSTAIRS
#3:              3 WALKING_DOWNSTAIRS
#head(DataTableX,2)
#subject activityNumber        V1          V2          V3         V4          V5         V6       V41        V42
#1:       1              1 0.2820216 -0.03769622 -0.13489730 -0.3282802 -0.13715339 -0.1890859 0.9453028 -0.2459414
#2:       1              1 0.2558408 -0.06455003........
#........

##merge the 2 datatable by activityNumber
DataTableX <- merge(DataTableX,dataTableActivityNames,by="activityNumber",all.x=TRUE)
head(DataTableX,2)

#Setting the keys subject, activityNumber, and activityName 
setkey(DataTableX,subject,activityNumber,activityName)
#create a new table from short_wide to tall_narrow
DataTableX <- data.table(melt(DataTableX,key(DataTableX),variable.name="featureCode")) 

#now merge activity name to the new DataTable 

DataTableX <- merge(DataTableX,dataTableFeatures[,list(featureNumber,featureCode,featureName)],by="featureCode", all.x=TRUE)

#add 2 new variables activity and feature equivalent to activityName and featureName respectively
DataTableX$activity <- factor(DataTableX$activityName)
DataTableX$feature  <- factor(DataTableX$featureName) 

head(DataTableX,2)
#featureCode subject activityNumber activityName     value featureNumber       featureName activity           feature
#1:          V1       1              1      WALKING 0.2820216             1 tBodyAcc-mean()-X  WALKING tBodyAcc-mean()-X
#2:          V1       1              1      WALKING 0.2558408             1 tBodyAcc-mean()-X  WALKING tBodyAcc-mean()-X

numCategories <- 2 #This is number of Categories in feature to look for 


## Features with 1 category
DataTableX$featJerk <- factor(myGrep("Jerk"), labels=c(NA, "Jerk"))
DataTableX$featMagnitude <- factor(myGrep("Mag"), labels=c(NA, "Magnitude"))

DataTableX$featDomain <- createMatrix(c(myGrep("^t"),myGrep("^f")),numCategories,c("Time", "Freq"))

DataTableX$featInstrument <- createMatrix(c(myGrep("Acc"),myGrep("Gyro")),numCategories,c("Accelerometer", "Gyroscope"))

DataTableX$featAcceleration <- createMatrix(c(myGrep("BodyAcc"),myGrep("GravityAcc")),numCategories,c(NA,"Body", "Gravity"))

DataTableX$featVariable <- createMatrix(c(myGrep("mean\\(\\)"),myGrep("std\\(\\)")),numCategories,c("Mean", "SD"))
## Features with 1 category
DataTableX$featJerk <- factor(myGrep("Jerk"), labels=c(NA, "Jerk"))
DataTableX$featMagnitude <- factor(myGrep("Mag"), labels=c(NA, "Magnitude"))

numCategories <- 3 #This is number of Categories in feature to look for
DataTableX$featAxis <- createMatrix(c(myGrep("\\-X"),myGrep("\\-Y"),myGrep("\\-Z")),numCategories,c(NA,"X", "Y","Z"))


#r1 <- nrow(DataTableX[, .N, by=c("feature")])
#r2 <- nrow(DataTableX[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
#r1 == r2

##Step 5 creating DataTableXTidy this is independent tidy data 
setkey(DataTableX, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
DataTableXTidy <- DataTableX[, list(count = .N, average = mean(value)), by=key(DataTableX)]

##printing sturcture and summary and some combination
str(DataTableXTidy)

summary(DataTableXTidy)

DataTableXTidy[, .N, by = c(names(DataTableXTidy)[grep("^feat", names(DataTableXTidy))])]
#Saving the Tidy data 


foutput <- file.path(workArea,harusOutFile)
write.table(DataTableXTidy,foutput,quote = FALSE, sep = "\t", row.names = FALSE)

