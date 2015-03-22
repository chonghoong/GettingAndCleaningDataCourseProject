require(plyr)

# define file path
feature_file <- "./UCI HAR Dataset/features.txt"
activity_labels_file <- "./UCI HAR Dataset/activity_labels.txt"
x_train_file <- "./UCI HAR Dataset/train/X_train.txt"
y_train_file <- "./UCI HAR Dataset/train/y_train.txt"
x_test_file  <- "./UCI HAR Dataset/test/X_test.txt"
y_test_file  <- "./UCI HAR Dataset/test/y_test.txt"
subject_test_file <- "./UCI HAR Dataset/test/subject_test.txt"
subject_train_file <- "./UCI HAR Dataset/train/subject_train.txt"


# Load raw data from the files
features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)


##################################################################
# 1. Merges the training and the test sets to create one data set.
##################################################################

# Binding sensor data - column binds for labels, dataset and subjects
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)

# Binding test and training sensor data - row binds
sensor_data <- rbind(training_sensor_data, test_sensor_data)

# Label the columns. rbind the additional 2 columns "Subject" and "ActivityId"
# columnn #2 are the labels
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels


############################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################

# grepl returns the indices of the specified search pattern
sensor_data_mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]


###########################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################

sensor_data_mean_std <- join(sensor_data_mean_std, activity_labels, by = "ActivityId", match = "first")

## drop the first element - "Activity Id"
sensor_data_mean_std <- sensor_data_mean_std[,-1]




##############################################################
# 4. Appropriately labels the data set with descriptive names.
##############################################################

# Remove parentheses ()
names(sensor_data_mean_std) <- gsub('\\(|\\)',"",names(sensor_data_mean_std), perl = TRUE)

# Make syntactically valid names - "put "."
names(sensor_data_mean_std) <- make.names(names(sensor_data_mean_std))

# Make clearer names - find pattern and replace with the clearer names
names(sensor_data_mean_std) <- gsub('Acc',"Acceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Mag',"Magnitude",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.mean',".Mean",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.std',".StandardDeviation",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq$',"Frequency",names(sensor_data_mean_std))



######################################################################################################################
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################

# ddply - Split data frame by Subject and Activity and then apply a function, and return results in a data frame.
# numcolwise is a column-wise funcion that apply to every column e.g. the mean of each variable is calculated.
sensor_avg_by_act_sub = ddply(sensor_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "sensor_avg_by_act_sub.txt")





