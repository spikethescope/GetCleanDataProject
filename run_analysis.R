# Run analysis file for Smartphone data from the web
# Read in the training and test data
#First clear the environment
rm(list=ls())

setwd("E:\\Archives\\Personal\\Teaching\\data_setsBackup\\Samsung_phones_datasets\\UCI HAR Dataset");
library(dplyr)
# Read in the data from files
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
x_train       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
y_train       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
subjectTest  = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
x_test        = read.table('./test/x_test.txt', header = FALSE); #imports x_test data
y_test        = read.table('./test/y_test.txt', header = FALSE); #imports 

#Now rename the columns for each data stored in training set
colnames(features) = c('Signal_id', 'Domain_signal_type')
colnames(activityType) = c('Activity_id', 'Activity_name')
colnames(subjectTrain) = c('Subject_id')
colnames(x_train) = features[,"Domain_signal_type"]
colnames(y_train) = c('Activity_id')

#Do this for test data as well.
colnames(subjectTest) = c('Subject_id')
colnames(x_test) = features[,"Domain_signal_type"]
colnames(y_test) = c('Activity_id')

#Now create a single data for training set by binding columns
x_train = cbind(x_train, subjectTrain, y_train)

#Now create a single data for test set by binding columns
x_test  = cbind(x_test, subjectTest, y_test)

#Now combine both train and test set together
combinData = rbind(x_train, x_test)

#Now create a tbldf for using dplyr commands
trainData = tbl_df(combinData)

#now clean the memory used by other variables 

#Colnames contain duplicates. Unable to use select from dplyr.
#So, using logical indices for selection
isNum = grepl("mean|std|id",colnames(trainData))
#Select measurements containing name strings mean or std or id
metrics_trainData = trainData[,which(isNum)]

#Now join activity_names with selected data
metrics_trainData = inner_join(metrics_trainData,activityType, by = "Activity_id")


#Now store column names for cleaning
datColNames = colnames(metrics_trainData)

#Clean the column names 
for (i in 1: length(datColNames))
{
  datColNames[i] =  gsub("\\()","", datColNames[i])
  datColNames[i] =  gsub("tBody","Time_Body", datColNames[i])
  datColNames[i] =  gsub("tGravity", "Time_Gravity", datColNames[i])
  datColNames[i] =  gsub("fBody", "Freq_Body", datColNames[i])
  datColNames[i] =  gsub("-std", "StdDeviation", datColNames[i])
  datColNames[i] =  gsub("-mean", "Mean", datColNames[i])
  datColNames[i] =  gsub("-meanFreq", "Mean_Frequency", datColNames[i])
  datColNames[i] =  gsub("BodyBody", "Body", datColNames[i])
  datColNames[i] =  gsub("Mag", "Magnitude", datColNames[i])
}  
#Replace column names in tbl_df with new ones
colnames(metrics_trainData) = datColNames

#Now derive the tidyData from the modified tbl_df
tidyData = metrics_trainData %>% group_by(Subject_id, Activity_name) %>% summarise_each(funs(mean), c(1:(length(datColNames)-3)))

#Output the tidy data to a file
write.table(tidyData, './tidyData.txt', row.names = FALSE, sep='\t')

#Now clear the R environment 
rm(list=ls())
