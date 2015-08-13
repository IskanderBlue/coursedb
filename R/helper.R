#' Ensure that correctly formatted dates are stored as numeric values.
#' 
#' @family helper functions
#' @param x A date -- converted into a date class object, then converted into a numeric value.  
#' @examples numDate(Sys.Date())
#' numDate("2015-07-29")
numDate <- function(x) as.numeric(as.Date(x))

#' Returns a numeric value if as.numeric() can successfully coerce it.
#' 
#' Not expected to be accessed directly by users.
#' @family helper functions
#' @param maybeANumber A variable of unknown type, possibly coercible to a numeric value.
checkNumeric <- function(maybeANumber) {
      # Avoiding warning when assigning as.numeric(maybeANumber) to num
      storeWarn <- getOption("warn")
      options(warn = -1)
      asNum <- as.numeric(maybeANumber)
      options(warn = storeWarn)
      # Testing 
      if (!is.na(asNum)) {
            return(as.numeric(maybeANumber))
      } else {
            return(maybeANumber)
      }
}      

#' A helper function for the all of the Updater-() functions.
#' 
#' Not expected to be accessed directly by users.
#' Used by the \code{\link{data entry functions}}.
#' 
#' @family helper functions
#' @param df A data.frame to be used to update the table.
#' @param sql1 A string, the initial sql statement to find rows matching the criteria for an existing row.
#' @param ifsql A string, the sql statement to be used if there are no such matching rows 
#'    (tells Updater-() to add new rows to table).
#' @param elsesql A string, the sql statement to be used if no rows match; 
#'    (tells Updater-() to update matching rows).

rowUpdater <- function(df, sql1, ifsql, elsesql, nextRow, updateCounter) {
      conn <- DBconn()
      query <- dbGetPreparedQuery(conn, sql1, bind.data = df)      
      if (nrow(query) == 0) {
            newRow <- 1
            sql <- ifsql
      } else {
            newRow <- 0
            sql <- elsesql
      }
      dbGetPreparedQuery(conn, statement = sql, bind.data = df)      
      return(newRow)
}

#' Edit tables directly.
#' 
#' Not used in package; used to develop package.
#' Can mess things up; not really for user use.  
#' @param table A database table to be edited.
#' @param conn An SQL connection.
DBedit <- function(table, conn = DBconn()) {
      tab <- dbReadTable(conn, table)
      tab <- edit(tab)
      dbWriteTable(conn, table, tab, overwrite=TRUE, row.names = FALSE)
}



#' Split a names column into givenNames and lastNames columns.
#' 
#' Must assign output to (the parameter data.frame or a new) data.frame
#' 
#' @param dframe A data.frame with a column containing first and last names in a single string.
#' @param nameColumn The name of the column containing the names.  
#' @examples 
#' df <- data.frame(names = c("James Earl Jones", "Chow Yun Fat", "Julia-Louise Dreyfus"), stringsAsFactors = FALSE)
#' namesColumn <- "names"
#' df <- nameSplitter(df, namesColumn)
#' df

nameSplitter <- function(dframe, namesColumn) {
      splitNames <- strsplit(x = as.character(dframe[[namesColumn]]), split = " ")
      givenNames <- list()
      lastName <- list()
      length(lastName) <- length(splitNames)
      length(givenNames) <- length(splitNames)
      
      for (i in 1:length(splitNames)) {
            for (j in 1:length(splitNames[[i]])-1) {
                  givenNames[[i]] <- paste(givenNames[[i]], splitNames[[i]][j])
            }
            givenNames[[i]] <- substring(givenNames[[i]], 2)
            lastName[[i]] <- splitNames[[i]][length(splitNames[[i]])]
      }
      return(data.frame(dframe, givenNames = t(t(givenNames)), lastName = t(t(lastName))))
}


#' Function to remove rows erroneously entered into database.

#' @param tableName A string, the name of the table to edit.
#' @param rows A vector of rows to remove.
#' @param conn A connection to an SQL database.
#' @examples
#' removeRow("students", 2)

removeRow <- function(tableName, rows, conn = DBconn()) {
      rows <- data.frame(rows = rows)
      sql <- paste("DELETE FROM ", tableName, " WHERE rowNumber = :rows", sep = "")
      dbGetPreparedQuery(conn, statement = sql, bind.data = rows)      
}

#' Determine which rows to remove.
#' 
#' @param table The table from which you wish to delete rows.
#' @param badRowsDF A data.frame containing the characteristics of the rows you wish to delete.
#' @param columns The specific columns in badRowsDF containing characteristics of the rows you wish to delete.


# asCha <- c(T, F, F, T, T, T)
#if (is.null(badRowsDF$date) == FALSE) badRowsDF$date <- as.numeric(badRowsDF$date)

deleteWhichRows <- function(table, badRowsDF, columns, asCha) {
      
      # Cutting 'newDF' data.frame down to only those columns listed in 'columns'
      badRowsDF <- badRowsDF[columns]
      # Ensuring that appropriate columns are read as characters, not numerics.
      badRowsDF[asCha] <- lapply(badRowsDF[asCha], as.character)
      
      # Ensuring that columns have names.
      if (is.null(names(columns))) {
            names(columns) <- columns
      } else {
            for (i in 1:length(columns)) (if (names(columns)[i] == "") (names(columns)[i] <- columns[i]))
      }
      
      # Renaming columns of badRowsDF appropriately.
      names(badRowsDF) <- names(columns)
      
      # Changing elements of columns to their names for simplicity.
      columns <- names(columns)
      
      # findsql, finding which rows match user's description.
      t.columns <- paste("t.", columns[1], " = :", columns[1], sep = "")
      for (i in columns[-1]) {
            (t.columns <- paste(t.columns, " AND t.", i, " = :", i, sep = ""))
      }
      if (table == "students") {
            selection <- "SELECT ID, email, givenNames, lastname, program, notes FROM students"
      } else if (table == "assignments") {
            selection <- "SELECT ID, date, grade, assignmentName FROM assignments"
      } else if (table == "mcAnswers") {
            selection <- "SELECT ID, date, answer, questionName, questionValue, examName, examCode FROM mcAnswers"
      } else if (table == "longformGrades") {
            selection <- "SELECT ID, date, grade, questionName, examName, examCode FROM longformGrades"
      } else if (table == "classParticipation") {
            selection <- "SELECT ID, date, attended, questionAnswered, questionAsked, participationNotes FROM classParticipation"
      } else {
            print("Invalid table name.")
      }
      findsql <- paste(selection, " AS t WHERE ", t.columns, sep = "")
      
      # Finding rows described in badRowsDF.
      query <- dbGetPreparedQuery(conn = DBconn(), findsql, bind.data = badRowsDF)      
      # If none found, return.
      if (nrow(query) == 0) {
            print("No rows matching your description were found. ")
      } else {
            # If some found, show which, ask which row numbers should be deleted.
            print("We found the following records matching your description:")
            print(query)
            d <- readline("Which rows would you like to delete?  (Enter a non-numeric character for none.) ")
            
            # Suppressing warnings while testing whether 'd' is numeric.
            storeWarn <- getOption("warn")
            options(warn = -1)
            if (is.na(as.numeric(d))) {
                  print("I'm assuming that's 'None of the above'. ")
            } else {
                  removeRow(table, d, conn = DBconn())
            }
            # Resetting default warnings.
            options(warn = storeWarn)
      }
}