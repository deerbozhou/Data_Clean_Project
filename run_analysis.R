## Create a Run_Analysis.R to do the following functions:
## Merges the training and the test sets to create a first data set.
## Extracts only the measurements on the mean and standard deviation for each measurement. 
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive activity names. 
## Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

## Use gsub function to reformat the lables and make the names readable from the features.txt 
clean.names <- function(names) {
  ## Get rid of parentheses 
  clean <- gsub('*[()]', '', names)
  
  ## Convert prefixes "t" and "F" to convert to readable names
  clean <- gsub('^t', 'time.', clean)
  clean <- gsub('^f', 'freq.', clean)
  
  ## Convert abbreviated "Acc" and "Mag" to convert to readable names
  clean <- gsub('Acc', 'Acceleration', clean)
  clean <- gsub('Mag', 'Magnitude', clean)
  
  clean
}

## Create a Functions to load the activity labels and description, and convert to common descriptive names
get_factor_info <- function(fileLoc='UCI HAR Dataset/activity_labels.txt') {
  df <- read.table(fileLoc, stringsAsFactors=FALSE)
  lvls <- df[,1]
  lbls <- df[,2]
  list(levels=lvls, labels=lbls)
}

## Download and extract the data files
if(!file.exists('UCI HAR Dataset')){
  url <- paste0('https://d396qusza40orc.cloudfront.net/','getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip')
  download.file(url, destfile = 'dataset.zip', method='curl')
  unzip('dataset.zip')
}

## Load and process the data per project requirements
load_data <- function(set='test') {
  ## Load X_test.txt and X_train.txt 
  stats <- read.table(paste0('UCI HAR Dataset/', set, '/X_', set, '.txt'))
  
  ## Load features.txt
  features <- read.table('UCI HAR Dataset/features.txt', stringsAsFactors=FALSE)
  names <- features[,2]
  better.names <- clean.names(names)
  
  ## Get means and Standard deviation
  chosen <- grepl('mean|std', names) & !grepl('meanFreq', names)
  
  ## Extract only the measurements on the mean and standard deviation 
  stats <- stats[,chosen]
  colnames(stats) <- better.names[chosen]
  
  ## Load subject and test data
  subjects <- read.table(paste0('UCI HAR Dataset/', set, '/subject_', set, 
                                '.txt'))
  tests <- read.table(paste0('UCI HAR Dataset/', set, '/y_', set, '.txt'))
  
  ## Call the function get_factor_info to get readable, descriptive names for the lables
  f <- get_factor_info()
  tests <- factor(tests[,1], levels=f$levels, labels=f$labels)
  subjects <- factor(subjects[,1], levels=1:30)
  
  ## Merge all the data sets and add additional fiels for output
  stats <- cbind(subjects, tests, stats)
  colnames(stats)[1:2] <- c('Subject', 'Test')
  stats$Set <- rep(set, dim(stats)[1])   
  stats
}

## Create a first clean data set called clean_data.txt to merge the training and test sets for further analysis
if(!file.exists('clean_data.txt')){   
  ## Load the data sets
  test <- load_data('test')
  train <- load_data('train')
  
  ## Combine the sets together
  combine <- rbind(test, train)
  
  ## Write results to a table
  write.table(combine, file='clean_data.txt')
}

## Create a second data set called clean_data_analysis.txt to extracts only the measurements on the mean and standard deviation for each measurement
if(!file.exists('clean_data_analysis.txt')){  
  
  ## Read in the data from Clean_data.txt file
  library(data.table) 
  df <- read.table('clean_data.txt')
  dt <- data.table(df)
  
  ## Label the columns with descriptive activity names
  keys <- c('Subject', 'Test')
  setkeyv(dt, keys)
  
  ## Analysis data for the mean and Standard devisation for each measurement
  n.dt <- dt[, lapply(.SD, mean), by=key(dt)]
  
  ## Write results to a table
  write.table(n.dt, file='clean_data_analysis.txt')
}



