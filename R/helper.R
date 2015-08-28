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
#' Used by the data entry functions.
#' 
#' @family helper functions
#' @family data entry functions
#' @param df A data.frame to be used to update the table.
#' @param sql1 A string, the initial sql statement to find rows matching the criteria for an existing row.
#' @param ifsql A string, the sql statement to be used if there are no such matching rows 
#'    (tells Updater-() to add new rows to table).
#' @param elsesql A string, the sql statement to be used if no rows match; 
#'    (tells Updater-() to update matching rows).

rowUpdater <- function(df, sql1, ifsql, elsesql) {
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
#' @param namesColumn The name of the column containing the names.  
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


#' Function to ghost or unghost rows erroneously entered into database.
#' 
#' Meant to be internal.  

#' @param tableName A string, the name of the table to edit.
#' @param rows A vector of rows to remove.
#' @param delete A flag setting whether to delete or undelete.
#' @param conn A connection to an SQL database.
#' @examples
#' ghostRow("students", 2)

ghostRow <- function(tableName, rows, delete, conn = DBconn()) {
      rows <- data.frame(rows = rows)
      if (delete == TRUE) {
            sql <- paste("UPDATE ", tableName, " SET del = 1 WHERE rowNumber = :rows", sep = "")
      } else {
            sql <- paste("UPDATE ", tableName, " SET del = 0 WHERE rowNumber = :rows", sep = "")
      }
      dbGetPreparedQuery(conn, statement = sql, bind.data = rows)      
}

#' Determine which rows to remove by description.
#' 
#' @param table The table from which you wish to delete or undelete rows.
#' @param description A named vector describing the rows you wish to delete or undelete.
#' @param delete A flag setting whether to delete or undelete.
#' @examples 
#' description <- c(ID = "111111111", date = as.Date("2015-05-21")) 
#' deleteRows("classParticipation", description) # Can enter 1 or 2 or "1 2" as prompted.
#' description <- c(ID = "333333333", date = as.Date("2015-05-22"))
#' deleteRows("classParticipation", description) # Can enter any combination of 1, 2, 3, and 4.

deleteRows <- function(table, description, delete = TRUE) {
      
      # Cobbling together SQL to find which rows match user's description.
      # Putting user information into usersql, 
      # row numbers matching rows into hiddenRNsql.
      t.columns <- paste("t.", names(description)[1], " = :", names(description)[1], sep = "")
      for (i in names(description)[-1]) {
            (t.columns <- paste(t.columns, " AND t.", i, " = :", i, sep = ""))
      }
      if (delete == TRUE) {
            t.columns <- paste(t.columns, "AND t.del = 0")
      } else {
            t.columns <- paste(t.columns, "AND t.del = 1")
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
      usersql <- paste(selection, " AS t WHERE ", t.columns, sep = "")
      hiddenRNsql <- paste("SELECT rowNumber from ", table, " AS t WHERE ", t.columns, sep = "")
      
      # Creating df
      df <- data.frame(t(description))
      # Finding rows described.
      conn = DBconn()
      userSees <- dbGetPreparedQuery(conn, usersql, bind.data = df)
      
      # If none found, return.
      if (nrow(userSees) == 0) {
            print("No rows matching your description were found. ")
      } else {
            # If some found, show which, ask which row numbers should be deleted.
            print("We found the following records matching your description:")
            print(userSees)
            if (delete == TRUE) {
                  print("Which rows would you like to delete?")
            } else {
                  print("Which rows would you like to undelete?")
            }
            d <- readline("(Enter row numbers separated by spaces; other inputs interpreted as 'none'.) \n")

            # Suppressing warnings while testing whether 'd' is numeric.
            storeWarn <- getOption("warn")
            options(warn = -1)
            if (is.na(as.integer(strsplit(d, " ")[[1]]))) {
                  print("I'm assuming that's 'None of the above'. ")
            } else {
                  d <- as.integer(strsplit(d, " ")[[1]])
                  hiddenRN <- dbGetPreparedQuery(conn, hiddenRNsql, bind.data = df)
                  rNumbers <- hiddenRN[d, ]
                  ghostRow(table, rNumbers, delete)
            }
            # Resetting default warnings.
            options(warn = storeWarn)
      }
}

#' Determine which rows to remove by formula
#' 
#' @param table The table from which you wish to delete or undelete rows.
#' @param formula A formula or vector of formulas describing the rows you wish to delete or undelete.
#' @param delete A flag setting whether to delete or undelete.
#' @examples 
#' formula <- c(~ID == "222222222", ~grade < 10) 
#' deleteFormula("assignments", formula) 
#' formula <- c(~ID == "333333333", ~date == as.Date("2015-05-22"))
#' deleteFormula("classParticipation", formula) # Can enter any combination of 1, 2, 3, and 4.

deleteFormula <- function(table, formula, delete = TRUE) {
      # Create enviroment within which to evaluate formula
      df <- showTable(table)
      env <- list2env(df)
      # If formula is a list, go through every entry; otherwise just evaluate.
      if (is.list(formula)) {
            deleteVector <- rep(TRUE, nrow(df))
            for (i in 1:length(formula)) {
                  deleteVector <- deleteVector & eval(formula[[i]][[2]], envir = env)
            }
      } else {
            deleteVector <- eval(formula[[2]], envir = env)
            if (sum(deleteVector) == 0) stop("No rows fit your formula.")
      }
      # Cutting df down to rows that fit formula.  Renaming rows for user clarity.
      df <- df[deleteVector, ]
      rownames(df) <- 1:nrow(df)
      # Acquiring hidden rown numbers, cutting them to fit formula too.
      hiddenRNsql <- paste("SELECT rowNumber from ", table, sep = "")
      hiddenRN <- dbGetPreparedQuery(conn = DBconn(), hiddenRNsql, bind.data = df)
      hiddenRN <- hiddenRN[deleteVector, ]
      # Verifying with user which rows to cut.
      cat("These are the rows returned by your formula.\n")
      print(df)
      if (delete == TRUE) {
            cat("Which rows would you like to delete?\n")
      } else {
            cat("Which rows would you like to undelete?\n")
      }
      d <- readline("Enter row numbers separated by spaces or 'all'; other inputs interpreted as 'none'. \n")
      # Suppressing warnings while testing whether 'd' is numeric.
      storeWarn <- getOption("warn")
      options(warn = -1)
      if (is.na(as.integer(strsplit(d, " ")[[1]]))) {
            if (d == 'all') {
                  rNumbers <- hiddenRN
                  ghostRow(table, rNumbers, delete)
            } else {      
                  cat("I'm assuming that's 'None of the above'. \n")
            }
      } else {
            d <- as.integer(strsplit(d, " ")[[1]])
            rNumbers <- hiddenRN[d]
            ghostRow(table, rNumbers, delete)
      }
      # Resetting default warnings.
      options(warn = storeWarn)
}