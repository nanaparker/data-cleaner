###############################################################################
###                                                                       #####
###   Author:  Nana Parker                                                #####
###   Date:    10th March, 2022                                           #####
###   Purpose: This program seeks to provide analysts with a means to     ##### 
###            detect empty rows and columns in datasets and separate     #####
###            those values.                                              #####
###                                                                       #####
###############################################################################

# Libraries required
library(shiny)
library(readr)
library(dplyr)
library(shinyjs)


#-----------------------------------------UI SECTION------------------------------------------------
ui <- tags$html(
  useShinyjs(),
  
  tags$head(
    tags$title("Data Cleaner"),
    tags$link(`rel`="stylesheet", `type`="text/css", `href` ="dataClean.css")
  ),
  
  tags$body(
    class="bkg",
    
    #---------------------------Front End for Main Menu Section----------------------------
    
    tags$div(class="mainMenu", id="mainMenu",
             tags$img(class="main-logo", src="img/logo.png"),
             actionButton("crossDuplicate","Two File Duplicates", class="main-Duplicates"),
             actionButton("dataCleaner", "Data Cleaner", class="main-DataCleaner"),
             
    ),
    
    #------------------------Front End for Cross Duplicates Section------------------------
    tags$div(
      class="CrossDuplicates", id="CrossDuplicates",
      
      tags$div(class="cross_sidebar", id="cross_sidebar",
               tags$img(class="logo", src="img/logo.png"),
               tags$p(class="cc", "by Cross Multiply"),
               tags$div(class="line"),
               numericInput(inputId = "ColumnRemovalA", "File 1 Columns", 1, min = 1, max=10),
               numericInput(inputId = "ColumnRemovalB", "File 1 Columns", 1, min = 1, max=10),
               numericInput(inputId = "ColumnRemovalC", "File 2 Columns", 1, min = 1, max=10),
               numericInput(inputId = "ColumnRemovalD", "File 2 Columns", 1, min = 1, max=10),
               uiOutput("results"),
               actionButton("cross_submitButton", "Select", class="submit-btn")
      ),
      
      tags$div(class="cross_grid", id="cross_grid",
               tags$img(class="up-logo", src="img/cloud.png"),
               tags$p(class="txt1", "Drag and drop file here"),
               tags$p(class="txt2", "or"),
               fileInput('datafiles',label = "", buttonLabel = "Browse",
                         accept=c('text/csv', 'text/comma-separated-values,text/plain'), multiple=TRUE)              
      ),
      
      tags$div(class="cross_finalGrid", id="cross_finalGrid",
               downloadButton("cross_button1", "One Item with Two IDs"),
               downloadButton("cross_button2", "Two Items with One ID")
      )
    ),
    
    
    #---------------------------Front End for Data Cleaner Section------------------------
    tags$div(
      class="DataCleaner", id="DataCleaner",
      uiOutput("page2"),
      tags$div(
        class="sidebar", id="sidebar",
        tags$img(class="logo", src="img/logo.png"),
        tags$p(class="cc", "by Nana Parker"),
        tags$div(class="line"),
        radioButtons(inputId = 'select', label = 'Select the row to be removed', choices = c(None='0',First='1'), selected = '0'),
        numericInput(inputId = "ColumnRemoval1", "Columns to scan", 1, min = 1, max=10),
        numericInput(inputId = "ColumnRemoval2", "Columns to scan", 1, min = 1, max=10),
        uiOutput("tables"),
        actionButton("submitButton", "Select", class="submit-btn")
      ),
      
      tags$div(
        class="grid", id="grid",
        tags$img(class="up-logo", src="img/cloud.png"),
        tags$p(class="txt1", "Drag and drop file here"),
        tags$p(class="txt2", "or"),
        fileInput('datafile',label = "", buttonLabel = "Browse",
                  accept=c('text/csv', 'text/comma-separated-values,text/plain'))
      ),
      
      fluidRow(
        class="finalGrid", id="finalGrid",
        downloadButton("emptyRows", "Empty Rows"),
        downloadButton("emptyColumns", "Empty Columns"),
        downloadButton("cleanData", "Clean Data")
      ),
      
      fluidRow(
        id="DuplicatedGrid", class="DuplicatedGrid",
        downloadButton("repIDs", "Repeated IDs"),
        downloadButton("repComps", "Repeated Items"),
        downloadButton("repCompsDiffIds", "Repeated Items with Different IDs")
      ),
      tags$script(src="draganddrop.js")
    ) # End of Data Cleaner Section
  ) # End of Body
) # End of UI




#---------------------------------------SERVER SECTION---------------------------------------------
server <- function(input, output, session) {
  shinyjs::hide(id="DataCleaner")
  shinyjs::hide(id="CrossDuplicates")
  shinyjs::hide(id="cross_finalGrid")
  shinyjs::hide(id="finalGrid")
  shinyjs::hide(id="DuplicatedGrid")
  
  #---------------------------Cross Duplicates Back End Section------------------------
observeEvent(input$crossDuplicate, {
  shinyjs::hide(id="mainMenu")
  shinyjs::show(id="CrossDuplicates")
  
  observeEvent(input$datafiles, {
    continue <- eventReactive(input$cross_submitButton, {
      if(is.null(input$datafiles)){return ()}
      else {
        filename1 <- input$datafiles$name[1]
        filename2 <- input$datafiles$name[2]
        set1 <- tbl_df(read_csv(input$datafiles$datapath[1], locale = locale(encoding = "Latin1")))
        set2 <- tbl_df(read_csv(input$datafiles$datapath[2], locale = locale(encoding = "Latin1")))
        
        finalDuplicatedList <- list()
        secondDuplicatedList <- list()
        
        rpID1 <- c()
        rpID2 <- c()
        IDName <- c()
        com1 <- c()
        com2 <- c()
        repeatedID <- c()
        
        if(nrow(set1) > nrow(set2))
          maxLength <- nrow(set1)
        else 
          maxLength <- nrow(set2)
        
        mainTable <- data.frame(c(set1[[input$ColumnRemovalA]], rep("", maxLength - length(set1[[input$ColumnRemovalA]]))), 
                                c(set1[[input$ColumnRemovalB]], rep("", maxLength - length(set1[[input$ColumnRemovalB]]))),
                                c(set2[[input$ColumnRemovalC]], rep("", maxLength - length(set2[[input$ColumnRemovalC]]))),
                                c(set2[[input$ColumnRemovalD]], rep("", maxLength - length(set2[[input$ColumnRemovalD]])))
        )
        
        names(mainTable) <- c("Set_A_Col_A", "Set_A_Col_B", "Set_B_Col_A", "Set_B_Col_B")
        mainTable[is.na(mainTable)] <- 0
        
        mainTable <- tbl_df(mainTable)
        
        for (i in 1:nrow(mainTable)){
          for (p in 1:nrow(mainTable)){
            
            # Checking if one companies has two different IDs
            if (tolower(mainTable[[2]][[i]]) == tolower(mainTable[[4]][[p]])) {
              
              if (mainTable[[1]][[i]] != mainTable[[3]][[p]]) {
                duplicatedValues <- c(mainTable[i, 2],  mainTable[p, 3], mainTable[i, 1])
                rpID1 <- append(rpID1, mainTable[[1]][[i]])
                rpID2 <- append(rpID2, mainTable[[3]][[p]])
                IDName <- append(IDName, mainTable[[2]][[i]])  
              }
            }
            
            # Checking if one ID is used by two different companies
            if (mainTable[[1]][[i]] == mainTable[[3]][[p]]) {
              
              if (tolower(mainTable[[2]][[i]]) != tolower(mainTable[[4]][[p]])) {
                com1 <- append(com1, mainTable[[2]][[i]])
                com2 <- append(com2, mainTable[[4]][[p]])
                repeatedID <- append(repeatedID, mainTable[[1]][[i]])  
                
              }
            }
            
          }   # End of inner for loop
        }   # End of outer for loop
        
        finalDuplicatedList[[length(finalDuplicatedList)+1]] <- IDName
        finalDuplicatedList[[length(finalDuplicatedList)+1]] <- rpID1
        finalDuplicatedList[[length(finalDuplicatedList)+1]] <- rpID2
        
        secondDuplicatedList[[length(secondDuplicatedList)+1]] <- repeatedID
        secondDuplicatedList[[length(secondDuplicatedList)+1]] <- com1
        secondDuplicatedList[[length(secondDuplicatedList)+1]] <- com2
        
        shinyjs::hide(id="cross_grid")
        shinyjs::show(id="cross_finalGrid")
        
        
        # One Company, Two IDs
        if(length(finalDuplicatedList) >= 1){
          emptyRowsData <- data.frame(finalDuplicatedList, check.names = FALSE)
          names(emptyRowsData) <- c("Repeated Company", "ID 1", "ID 2")
          emptyRowsData <- tbl_df(emptyRowsData)
          
          output$cross_button1 <- downloadHandler(
            filename = function() {
              paste("One Item with Two IDs in ", filename1," and ", filename2, sep = "")
            },
            content = function(file) {
              write.csv(emptyRowsData, file, row.names = FALSE)
            }
          )
          #write.csv(emptyRowsData, "RepeatedData.csv")
        } else {
          shinyjs::hide(id="cross_button1")
        }
        
        # One ID, Two Companies
        if(length(secondDuplicatedList) >= 1){
          emptyRowsData2 <- data.frame(secondDuplicatedList, check.names = FALSE)
          names(emptyRowsData2) <- c("Repeated ID", "Item 1", "Item 2")
          emptyRowsData2 <- tbl_df(emptyRowsData2)
          
          output$cross_button2 <- downloadHandler(
            filename = function() {
              paste("Two Items with One ID in ", filename1," and ", filename2, sep = "")
            },
            content = function(file) {
              write.csv(emptyRowsData2, file, row.names = FALSE)
            }
          )
          #write.csv(emptyRowsData2, "DuplicateID.csv")
        } else {
          shinyjs::hide(id="cross_button2")
        }
      }
    })
    
    output$results <- renderUI({
      continue()
    })
  })
})


  
  #---------------------------Data Cleaner Back End Section------------------------
  
  # Waiting until a file has been uploaded and the 'Submit' button has been 
  # pressed to take action
  observeEvent(input$dataCleaner, {
    
    shinyjs::hide(id="mainMenu")
    shinyjs::show(id="DataCleaner")
    
    observeEvent(input$datafile, {
      start <- eventReactive(input$submitButton, {
        if(is.null(input$datafile)){return ()}    # If no file has been uploaded, nothing happens
        else {
          filename<- input$datafile$name
          dataset <- tbl_df(read_csv(input$datafile$datapath, locale = locale(encoding = "Latin1")))
          index <- c()
          emptyRows_identity <- c()
          emptyRows_List <- list()
          
          # Replace empty spaces with 'NA' values 
          dataset[dataset == "" | dataset == "N/A"] <- NA  
          
          
          # --------------------- Duplicate Checker ---------------------- #
          # Checking for Repeating IDs
          dupli_range <- (duplicated(dataset[, 1]) | duplicated(dataset[, 1], fromLast = TRUE))
          
          print(sum(dupli_range))
          if (sum(dupli_range) > 0){
            repeatedRows <- dataset[dupli_range, ]
            
            output$repIDs <- downloadHandler(
              filename = function() {
                paste("Repeated IDs in ", filename, sep="")
              },
              content = function(file) {
                write.csv(repeatedRows, file, row.names=FALSE)
              }
            )
          } else {
            shinyjs::hide("repIDs")
          }
          
          # Comparing the Columns selected plus the 4th
          dupli_range1 <- (duplicated(dataset[, input$ColumnRemoval1]) | duplicated(dataset[, input$ColumnRemoval1], fromLast = TRUE)) & (duplicated(dataset[, input$ColumnRemoval2]) | duplicated(dataset[, input$ColumnRemoval2], fromLast = TRUE)) & (duplicated(dataset[, 4]) | duplicated(dataset[, 4], fromLast = TRUE))
          
          print(sum(dupli_range1))
          if (sum(dupli_range1) > 0){
            
            repeatedRows1 <- dataset[dupli_range1, ]
            
            output$repComps <- downloadHandler(
              filename = function() {
                paste("Repeated Items in ", filename, sep = "")
              },
              content = function(file) {
                write.csv(repeatedRows1, file, row.names = FALSE)
              }
            )
          } else {
            shinyjs::hide("repComps")
          }
          
          # Comparing the Columns selected
          dupli_range2 <- (duplicated(dataset[, input$ColumnRemoval2]) | duplicated(dataset[, input$ColumnRemoval2], fromLast = TRUE)) & !(duplicated(dataset[, input$ColumnRemoval1]) | duplicated(dataset[ , input$ColumnRemoval1], fromLast = TRUE))
          print(sum(dupli_range2))
          if (sum(dupli_range2) > 0){
            
            repeatedRows2 <- dataset[dupli_range2, ]
            
            output$repCompsDiffIds <- downloadHandler(
              filename = function() {
                paste("Repeated Items with different IDs in ", filename, sep = "")
              },
              content = function(file) {
                write.csv(repeatedRows2, file, row.names = FALSE)
              }
            )
            print(filename)
          } else {
            shinyjs::hide("repCompsDiffIds")
          }
          
          
          
          # --------------------- Empty Column Removal ---------------------- #
          
          # User giving the option to remove the first row or not
          if(input$select == "1")
            dataset <- dataset[-1, ]
          
          shinyjs::hide(id="grid")
          shinyjs::show(id="finalGrid")
          shinyjs::show(id="DuplicatedGrid")
          
          # Finding all columns with NA's or empty strings
          allmissedcols <- sapply(dataset, function(x) all(is.na(x) | x == "" ))
          
          if(sum(allmissedcols) > 0){
            empty_cols <- names(allmissedcols[allmissedcols != FALSE])
            dataset <- select(dataset, -empty_cols)
            
            # Write to 'txt' for columns
            output$emptyColumns <- downloadHandler(
              filename = function() {
                paste("empty columns of ", filename, sep="")
              },
              content = function(file) {
                write.csv(empty_cols, file, row.names = FALSE)
              }
            )
          } else {
            shinyjs::hide(id="emptyColumns")
          }
          
          
          
          # --------------------- Empty Row Removal ---------------------- #
          
          # Finding rows with half or more of its columns being unfilled
          for(i in 1:nrow(dataset)){
            new_count <- 0
            missing <- c()
            emptyRows <- c()
            
            for (p in 1:ncol(dataset)) {
              if(is.na(dataset[i, p])) {
                missing <- append(missing, names(dataset[i, p]))
                new_count <- new_count + 1
              }
            }
            
            # If a particular row has half of its columns empty it will be removed
            if(new_count > (ncol(dataset) / 2) - 1){
              index <- append(index, i)
              
              emptyRows_identity <- append(emptyRows_identity, dataset[[i, 1]])
              maxLength <- length(names(dataset))
              emptyRows <- c(missing, rep("", maxLength - length(missing)))
              emptyRows_List[[length(emptyRows_List) + 1]] <- emptyRows
            }
          }
          
          names(emptyRows_List) <- emptyRows_identity
          emptyRowsData <- data.frame(emptyRows_List, check.names = FALSE)
          
          if (length(emptyRows_List) <= 1){
            shinyjs::hide("emptyRows")
          }else {
            # Write txt for rows
            output$emptyRows <- downloadHandler(
              filename = function() {
                paste("empty rows of ", filename, sep="")
              },
              content = function(file) {
                write.csv(emptyRowsData, file, row.names = FALSE)
              }
            )
            
            dataset <- dataset[-index, ]
            
            
            
            # --------------------- Clean Data Acquisition ---------------------- #
            
            # If dataset does not have an existing ID structure, one is provided
            
            if(!("ID" %in% colnames(dataset)) | !("id" %in% colnames(dataset))){
              id <- c(seq(nrow(dataset)))
              dataset <- dataset %>% mutate(ID = id) %>% select(ID, everything())
            }
          }
          
          # Write clean data to csv
          output$cleanData <- downloadHandler(
            filename = function() {
              paste("clean", filename, sep="_")
            },
            content = function(file) {
              write.csv(dataset, file, row.names = FALSE)
            }
          )
        }
        
      })
      
      output$tables <- renderUI({
        start()
      })
      
    }) 

    
  }) # End of Data Cleaner Server Side
  
} # End of Server

shinyApp(ui = ui, server = server)