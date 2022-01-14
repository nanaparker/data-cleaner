###############################################################################
###                                                                       #####
###   Author:  Nana Parker                                                #####
###   Date:    23rd December, 2021                                        #####
###   Purpose: This program seeks to provide analysts with a means to     ##### 
###            detect empty rows and columns in datasets and separate     #####
###            those values.                                              #####
###                                                                       #####
###############################################################################

cleaner <- function(dir, dataset, rowRemove) {

  # Switch statement to select between directories and setting it as a working directory
  setwd(dir)
  
  # If the data set exists the function will proceed else it would fail
  if(file.exists(dataset) == TRUE) {
    library(dplyr)
    
    # Variables
    index <- c()
    emptyRows_identity <- c()
    emptyRows_List <- list()
    filename_var <- paste("empty columns of ", dataset, sep="")
    filename_var2 <- paste("empty rows of ", dataset, sep="")
    filename_var3 <- paste("clean", dataset, sep="_")
    
    # Turns the data frame into a tibble and replaced any empty space with NA
    new_table <- tbl_df(read.csv(dataset))
    new_table[new_table == "" | new_table == "N/A"] <- NA
    
    if (rowRemove == 1)
      new_table <- new_table[-1, ]
    
    # Here empty columns are detected, placed in a text file and deleted from the dataset
    allmissedcols <- sapply(new_table, function(x) all(is.na(x) | x == "" ))
    
    if(sum(allmissedcols) > 0){
      empty_cols <- names(allmissedcols[allmissedcols != FALSE])
      new_table <- select(new_table, -empty_cols)
      write.csv(empty_cols, filename_var, row.names = FALSE)
    }else {
      write.csv("No empty columns", filename_var, row.names = FALSE)
    }
    
    # Here empty rows are detected, placed in a text file and deleted from the dataset
    for(i in 1:nrow(new_table)){
      new_count <- 0
      missing <- c()
      
      for (p in 1:ncol(new_table)) {
        if(is.na(new_table[i, p])) {
          missing <- append(missing, names(new_table[i, p]))
          new_count <- new_count + 1
        }
      }
      
      # If a particular row has half of its columns empty it will be removed
      if(new_count > (ncol(new_table) / 2) - 1){
        index <- append(index, i)
        
        emptyRows_identity <- append(emptyRows_identity, new_table[[i, 1]])
        maxLength <- length(names(new_table))
        emptyRows <- c(missing, rep("", maxLength - length(missing)))
        emptyRows_List[[length(emptyRows_List) + 1]] <- emptyRows
      }
    }
    
    names(emptyRows_List) <- emptyRows_identity
    emptyRowsData <- data.frame(emptyRows_List, check.names = FALSE)
    
    if(length(emptyRows_List) <= 1){
      write.csv("No empty rows", filename_var2)
    } else{
      write.csv(emptyRowsData, filename_var2)
      new_table <- new_table[-index, ]
      
      if(!("ID" %in% colnames(new_table)) | !("id" %in% colnames(new_table))){
        id <- c(seq(nrow(new_table)))
        new_table <- new_table %>% mutate(ID = id) %>% select(ID, everything())
      }
    }

    # Finale: Empty columns are removed and the final data set conversion to csv takes place
    write.csv(new_table, filename_var3, row.names = FALSE)
  }
  else {
    print("This file does not exist in this directory.")
  }
}