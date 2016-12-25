run_analysis <- funciont(){
    # Initialize
    rm(list=ls())
    
    ## ------ Load data ------------
    
    # Read test data
    X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
    subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
    y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
    
    # Read training data
    X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
    subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
    y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
    
    # Read feature
    feature_names <- read.table("./data/UCI HAR Dataset/features.txt")
    activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
    names(activity_labels) <- c("activity_id", 'activity names')
    
    
    ## ------ Merge Test and Train Data Set -------------
    X_all <- rbind(X_train, X_test)
    names(X_all) <- feature_names[,2]
    
    # Extact mean and std only
    col_inds <- grep("*-mean\\(\\)-]*|*-std\\(\\)-*",names(X_all))
    X_sliced <- X_all[,col_inds]
    
    y_all <- rbind(y_train, y_test)
    names(y_all) <- "target lables"
    
    subject_all <- rbind(subject_train, subject_test)
    names(subject_all) <- "subject_id"
    
    consolidated_db <- cbind(y_all, subject_all, X_sliced)
    consolidated_db <- merge(consolidated_db, activity_labels, 
                             by.x = "target lables", 
                             by.y = "activity_id")
    
    
    # Create the ticker for each subject AND activity
    consolidated_db$unique_ticker <- paste(consolidated_db$subject_id, consolidated_db$`activity names`)
    
    # Calculate the average of every var for each subject AND activity
    var_names <- names(X_sliced)
    avg <- matrix(data=NA, nrow = length(var_names), ncol = length(unique(consolidated_db$unique_ticker)))
    for (i in seq_along(var_names)){
        avg[i,] <- tapply(X_sliced[,i], consolidated_db$unique_ticker, mean)
    }
    
    avg <- data.frame(avg) # convert matrix into data frame
    names(avg) <- names(tapply(X_sliced[,1], consolidated_db$unique_ticker, mean)) # rename the column
    
    # Save the table
    write.table(avg, file = "./data/UCI HAR Dataset/avg.txt", row.names = FALSE)
    return(avg)
}