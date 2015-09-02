# Data entry functions.
# 
# A series of functions to update the course.db database.  


#' Generic function to update tables.
#' 
#' @family data entry functions
#' @param table A string containing the name of the table to be updated.
#' @param newDF A data.frame containing the info to be added to the table (or updated)
#' @param columns A vector of strings, the names of the specific columns to be added to the table; set names(columns) to the appropriate database column names.  eg. c(ID = "student.ID", email = "student.email", ...).
#' @param vitalColumns A vector of strings, the names of the columns by which UpdateTables() is to recognize whether a row is already in the database and needs updating or is new and is to be appended.  Set names(vitalColumns) to the appropriate database column names.  eg. c(ID = "student.ID").
#' @param asCha A logical vector, TRUE where the columns should be entered as characters rather than numerics.  Keeps R from deciding that IDs or names made only of digits are actually numeric values.
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' table <- "students"
#' sDF <- data.frame(ID = c("asd", 222222229, 122222229), email = c(NA, "222semail@@address.com", "abcd@@email.com"), lastName = c("Abelard", "Semekovic", "Kovacs"), givenNames = c("Eugene", "Juliana", "Takeshi"), program = c("statistics", "financial modelling", ""), notes = c("", "", "Terrifying.") ) 
#' columns <- c("ID", "email", "givenNames", "lastName")
#' vitalColumns <- c("ID")
#' asCha <- c(TRUE, TRUE, TRUE, TRUE)
#' UpdateTable(table, sDF, columns, vitalColumns, asCha, conn = DBconn(tmpcoursedb)) # Adds three lines to the students table.
#' sDF <- data.frame(ID = c("asd", 222222229, 122222229), email = c(NA, "amended.email@@address.com", "abcd@@email.com"), lastName = c("Abelard", "Semekovic", "Kovacs"), givenNames = c("Eugene", "Juliana", "Takeshi"), program = c("statistics", "financial modelling", ""), notes = c("", "Amended line.", "Terrifying.") ) 
#' columns <- c("ID", "email", "givenNames", "lastName", "notes")
#' UpdateTable(table, sDF, columns, vitalColumns, asCha, conn = DBconn(tmpcoursedb)) # Amends student 222222229's entry.


#' table <- "assignments"
#' aDF <- data.frame(ID = c(993456888, 222222229, 222222229), assignmentName = c("1", "1", "three"), grade = c(4, 5.5, 10), date = rep(Sys.Date(), 3) ) 
#' columns <- c("ID", "date", "grade", "assignmentName")
#' vitalColumns <- c("ID", "assignmentName")
#' asCha <- c(TRUE, FALSE, FALSE, TRUE)
#' UpdateTable(table, aDF, columns, vitalColumns, asCha, conn = DBconn(tmpcoursedb)) # Creates 3 new rows in 'assignments' table.
#' aDF <- data.frame(ID = c(993456888, 222222229, 222222229), assignmentName = c("1", "1", "three"), grade = c(4, 5.5, 9), date = rep(Sys.Date(), 3) ) 
#' UpdateTable(table, aDF, columns, vitalColumns, asCha, conn = DBconn(tmpcoursedb)) # Reduces grade in last entry from 10 to 9.


#' table <- "mcAnswers"
#' mcDF <- data.frame(ID = rep(c(567567567, 678678678, 999999999), each = 3), date = rep(Sys.Date(), 9), answer = c(1, 1, 1, 2, 3, 4, 3, 2, 3), questionName = rep(1:3, 3), questionValue = rep(1, 9), examName = rep("exm1.1(makeup)", 9), examCode = rep("101", 9))
#' columns <- c("ID", "date", "answer", "questionName", "questionValue", "examName", "examCode")
#' vitalColumns <- c("ID", "questionName", "examName")
#' asCha <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
#' UpdateTable(table, mcDF, columns, vitalColumns, asCha, conn = DBconn(tmpcoursedb)) # Enter answers to 3 MC questions for two students as well as the correct answers.
#' mcDF$questionValue <- rep(c(2, 2, 1), 3)
#' UpdateTable(table, mcDF, columns, vitalColumns, asCha, conn = DBconn(tmpcoursedb)) # Amend the question values from c(1, 1, 1) to c(2, 2, 1).

#' table <- "longformGrades"
#' ID = rep(c(567567567, 678678678, 999999999), each = 3)
#' date = rep(Sys.Date(), length(ID))
#' grade = c(8, 5, 2, 9, 4, 4, 10, 5, 5)
#' questionName = rep(1:3, 3)
#' examName = rep("exm1.1(makeup)", length(grade))
#' examCode = rep("101", length(grade))
#' lfDF <- data.frame(ID, date, grade, questionName, examName, examCode)
#' columns <- c("ID", "date", "grade", "questionName", "examName", "examCode")
#' vitalColumns <- c("ID", "questionName", "examName")
#' asCha <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
#' UpdateTable(table, lfDF, columns, vitalColumns, asCha, conn = DBconn(tmpcoursedb)) # Enter grades to 3 questions for two students as well as the maximum grades.
#' lfDF$grade <- c(8, 3, 2, 9, 4, 5, 10, 5, 5)
#' UpdateTable(table, lfDF, columns, vitalColumns, asCha, conn = DBconn(tmpcoursedb)) # Correct mistaken grade entries.

#' table <- "classParticipation"
#' cpDF <- data.frame(ID = c(993456889, 222222229, 222222229), date = rep(Sys.Date(), 3), attended = c(FALSE, TRUE, TRUE), questionAnswered = c("", "Q3", ""), questionAsked = c("", "", "Why is option pricing so complicated?"), participationNotes = c("", "", "Came in late. Again.") ) 
#' columns <- c("ID", "date", "attended", "questionAnswered", "questionAsked", "participationNotes")
#' vitalColumns <- c("ID", "date", "questionAnswered", "questionAsked", "participationNotes")
#' asCha <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
#' UpdateTable(table, cpDF, columns, vitalColumns, asCha, conn = DBconn(tmpcoursedb)) # Enters three new participation records.
#' cpDF$attended <- c(TRUE, TRUE, TRUE)
#' UpdateTable(table, cpDF, columns, vitalColumns, asCha, conn = DBconn(tmpcoursedb)) # Amends attendance for 1st student.

UpdateTable <- function(table, newDF, columns, vitalColumns, asCha = rep(TRUE, length(columns)), conn = DBconn()) {
      
      # Cutting 'newDF' data.frame down to only those columns listed in 'columns'
      newDF <- newDF[columns]
      # Ensuring that appropriate columns are read as characters, not numerics.
      newDF[asCha] <- lapply(newDF[asCha], as.character)
      
      # Ensuring that columns and vitalColumns have names.
      if (is.null(names(columns))) {
            names(columns) <- columns
      } else {
            for (i in 1:length(columns)) (if (names(columns)[i] == "") (names(columns)[i] <- columns[i]))
      }
      if (is.null(names(vitalColumns))) {
            names(vitalColumns) <- vitalColumns
      } else {
            for (i in 1:length(vitalColumns)) (if (names(vitalColumns)[i] == "") (names(vitalColumns)[i] <- vitalColumns[i]))
      }
      
      # Renaming columns of newDF appropriately.
      names(newDF) <- names(columns)
      
      # Changing elements of columns, vitalColumns to their names for simplicity.
      columns <- names(columns)
      vitalColumns <- names(vitalColumns)
      
      # Cobbling the sql statements together from 'columns' and 'vitalColumns'.  
      
      # sql1
      t.vitalVar <- paste("t.", vitalColumns[1], " = :", vitalColumns[1], sep = "")
      for (i in vitalColumns[-1]) {
            (t.vitalVar <- paste(t.vitalVar, " AND t.", i, " = :", i, sep = ""))
      }
      sql1 <- paste("SELECT * FROM ", table, " AS t WHERE ", t.vitalVar, " AND t.del = 0", sep = "")
      
      # ifsql
      specificInsert <- "rowNumber, del"
      for (i in columns) (specificInsert <- paste(specificInsert, i, sep = ", "))
      ifVarNames <- "rowNumber, 0"
      for (i in columns) (ifVarNames <- paste(ifVarNames, ", :", i, sep = ""))
      ifsql <- paste("INSERT INTO ", table, " (", specificInsert, ") VALUES (:", ifVarNames, ")", sep = "")
      
      # elsesql
      # Suppressing mysterious warnings:  
      #      1: In columns != vitalColumns :
      #            longer object length is not a multiple of shorter object length
      storeWarn <- getOption("warn")
      options(warn = -1)
      
      vitalVar <- paste(vitalColumns[1], " = :", vitalColumns[1], sep = "")      
      for (i in vitalColumns[-1]) (vitalVar <- paste(vitalVar, " AND ", i, " = :", i, sep = ""))
      nonVitalVar <- paste(columns[columns != vitalColumns][1], " = :", columns[columns != vitalColumns][1], sep = "")
      for (i in columns[columns != vitalColumns][-1]) (nonVitalVar <- paste(nonVitalVar, ", ", i, " = :", i, sep = ""))
      elsesql <- paste("UPDATE ", table, " SET ", nonVitalVar, " WHERE ", vitalVar, sep = "")
      
      # Resetting default warnings.
      options(warn = storeWarn)
      
      # Determining row number for next new row in table.
      maxsql <- paste("SELECT MAX(rowNumber) FROM ", table, sep = "")
      nextRow <- dbGetQuery(conn = conn, maxsql)[[1]]
      if (is.na(nextRow)) (nextRow <- 0)
      nextRow <- nextRow + 1
      
      # Setting up counter (counts how many rows are updated as opposed to created).
      updateCounter <- 0
      
      # Loop through variables, calling rowUpdater() to update each 
      # database row as well as nextRow and updateCounter appropriately.
      for (i in 1:nrow(newDF)) {
            df <- newDF[i, ]
            df$rowNumber <- nextRow
            # rowUpdater() returns 1 if new row was made, 0 if existing row was updated.
            newRow <- rowUpdater(df, sql1, ifsql, elsesql, conn)
            nextRow <- nextRow + newRow
            updateCounter <- updateCounter + (1 - newRow)
      }
      
      # Notify user that they have overwritten existing rows.
      if (updateCounter == 1) {
            print(paste("You have overwritten 1 existing row in the", table, "table."))
      } else if (updateCounter > 1) {
            print(paste("You have overwritten", updateCounter, "existing rows in the", table, "table."))
      }
}




#' Update "students" table with new (or corrected) data.
#' 
#' One function call can update an arbitrary number of student entries.  
#' The key variable identifying a given table row as unique is ID.  
#'    If a row with that ID value does not exist (eg. if ID was entered wrong initially), 
#'          UpdateMCAnswers() will assume that a new row should be added to the table.  
#'          See function removeRow() to fix such occurrances.  
#'    If a row with a given value for ID already exists, 
#'          UpdateStudents() assumes that the row should be corrected.  
#' @family data entry functions
#' @param newStudentDataFrame A dataframe with at least a column named "ID"; also possible: email, lastName, givenNames, program, notes.
#' \describe{
#'    \item{ID}{Strings, the students' IDs (typically 9-digit integers). Note that 999999999 is used as the ID for correct responses.}
#'    \item{email}{Strings, the email addresses.}
#'    \item{givenNames}{Strings, the students' first (and possibly middle) names.}
#'    \item{lastName}{Strings, the students' last names.}
#'    \item{program}{Strings, the program a student is enrolled in.}
#'    \item{notes}{Strings, any notes you would like to include.}
#' }
#' @param columns A vector containing the names of the columns you wish to include.  Set names(columns) to the appropriate database column names.  eg. c(ID = "student.ID", email = "student.email", ...).
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @examples
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' sDF <- data.frame(ID = c("", 222222229, 122222229), email = c(NA, "222semail@@address.com", "abcd@@email.com"), lastName = c("Abelard", "Semekovic", "Kovacs"), givenNames = c("Eugene", "Juliana", "Takeshi"), program = c("statistics", "financial modelling", ""), notes = c("", "", "Terrifying.") ) 
#' UpdateStudents(sDF, conn = DBconn(tmpcoursedb)) # Adds three lines to the students table.
#' sDF$email <- c(NA, "amended.email@@address.com", "abcd@@email.com") 
#' sDF$notes <- c("", "Amended line.", "Terrifying.") 
#' UpdateStudents(sDF, conn = DBconn(tmpcoursedb)) # Amends student 222222229's entry.

UpdateStudents <- function(newStudentDataFrame, columns = c("ID", "email", "givenNames", "lastName", "program", "notes"), conn = DBconn()) {
      # Ensuring that columns has names.
      if (is.null(names(columns))) {
            names(columns) <- columns
      } else {
            for (i in 1:length(columns)) (if (names(columns)[i] == "") (names(columns)[i] <- columns[i]))
      }
      
      UpdateTable(table = "students", newDF = newStudentDataFrame, columns = columns, vitalColumns = c(ID = columns[["ID"]]), asCha = rep(TRUE, length(columns)), conn = conn)
      rsID <- readStudents()$ID[]
      badIDs <- sum(is.na(rsID) | grepl("^$", rsID))
      if (sum(badIDs) == 1) {
            print("It seems that 1 ID was not read in correctly.")
      } else if (sum(badIDs) > 1) {
            print(paste("It seems that", badIDs, "IDs were not read in correctly."))
      }
}


#' Update "assignments" table with new (or corrected) data.
#' 
#' If in a given row of the data.frame parameter (aDF) ID and assignmentName are the same as a row in the assignments table, 
#' that row is assumed to be a duplicate and the date and grade values of the existing row in 
#' the table are updated rather than a new row created.  
#' @family data entry functions
#' @param newAssignmentDataFrame A dataframe with between two and four columns.  
#' \describe{
#'    \item{ID}{The students' IDs (typically 9-digit integers).  Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{date}{\code{\link{date}} class objects.  Do not set asCha equal to TRUE for this column.}
#'    \item{grade}{Numeric values, the grades recieved on assignments.  Do not set asCha equal to TRUE for this column.}
#'    \item{assignmentName}{The names or numbers of the assignments.  Should be entered using as.character() (set asCha equal to TRUE for this column).}
#' }
#' @param columns A vector containing the names of the columns you wish to include. Set names(columns) to the appropriate database column names.  eg. c(ID = "student.ID", date = "date.of.assignment", ...).
#' @param asCha A logical vector specifiying which columns to deliberately enter as characters (as opposed to numerics, date objects, etc.).
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @seealso \code{\link{date}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' aDF <- data.frame(ID = c(993456888, 222222229, 222222229), assignmentName = c("1", "1", "three"), grade = c(4, 5.5, 10), date = rep(Sys.Date(), 3) ) 
#' UpdateAssignments(aDF, conn = DBconn(tmpcoursedb)) # Creates 3 new rows in 'assignments' table.
#' aDF$grade <- c(4, 5.5, 9)
#' UpdateAssignments(aDF, conn = DBconn(tmpcoursedb)) # Reduces grade in last entry from 10 to 9.

UpdateAssignments <- function(newAssignmentDataFrame, columns = c("ID", "date", "grade", "assignmentName"), asCha = c(TRUE, FALSE, FALSE, TRUE), conn = DBconn()) {
      
      # Ensuring that columns has names.
      if (is.null(names(columns))) {
            names(columns) <- columns
      } else {
            for (i in 1:length(columns)) (if (names(columns)[i] == "") (names(columns)[i] <- columns[i]))
      }
      
      UpdateTable(table = "assignments", newDF = newAssignmentDataFrame, columns = columns, vitalColumns = c(ID = columns[["ID"]], assignmentName = columns[["assignmentName"]]), asCha = asCha, conn = conn)
}


#' Update "mcAnswers" table with new (or corrected) data.
#' 
#' Use one function call per exam.  
#' Key variables identifying a given table row as unique: ID, questionNumber, examName. 
#'    If a row with those values does not exist (eg. if ID or examName were entered wrong initially), 
#'          UpdateMCAnswers() will assume that a new row should be added to the table.  
#'          See function removeRow() to fix such occurrances.  
#'    If a row with given values for those parameters already exists, 
#'          UpdateMCAnswers() assumes that the line should be corrected.  
#' @family data entry functions
#' @param newMCAnswersDataFrame A data.frame with between 3 and 7 columns.
#' \describe{
#'    \item{ID}{The students' IDs (typically 9-digit integers).  Note that 999999999, is used as the ID for correct responses.  Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{date}{\code{\link{date}} class objects.  Do not set asCha equal to TRUE for this column.}
#'    \item{answer}{The multiple choice answers to be entered.  Note that a set of correct answers should be entered with the ID: 999999999. Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{questionName}{The names or numbers of the questions in a given test.  Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{questionValue}{Numerics, the number of marks to assign to a correct answer.  Do not set asCha equal to TRUE for this column.}
#'    \item{examName}{The name (or number) assigned to an exam. Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{examCode}{Distiguish between different versions of the same exam.  Typically 3-digit integers. Should be entered using as.character() (set asCha equal to TRUE for this column).}
#' }
#' @param columns A vector containing the names of the columns you wish to include.  Set names(columns) to the appropriate database column names.  eg. c(ID = "student.ID", date = "date.of.test", ...).
#' @param asCha A logical vector specifiying which columns to deliberately enter as characters (as opposed to numerics, date objects, etc.).
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' mcDF <- data.frame(ID = rep(c(567567567, 678678678, 999999999), each = 3), date = rep(Sys.Date(), 9), answer = c(1, 1, 1, 2, 3, 4, 3, 2, 3), questionName = rep(1:3, 3), questionValue = rep(1, 9), examName = rep("exm1.1(makeup)", 9), examCode = rep("101", 9))
#' UpdateMCAnswers(mcDF, conn = DBconn(tmpcoursedb)) # Enter answers to 3 MC questions for two students as well as the correct answers.
#' mcDF$questionValue <- rep(c(2, 2, 1), 3)
#' UpdateMCAnswers(mcDF, conn = DBconn(tmpcoursedb)) # Amend the question values from c(1, 1, 1) to c(2, 2, 1).

UpdateMCAnswers <- function(newMCAnswersDataFrame, columns = c("ID", "date", "answer", "questionName", "questionValue", "examName", "examCode"), asCha = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE), conn = DBconn()) {
      
      # Ensuring that columns has names.
      if (is.null(names(columns))) {
            names(columns) <- columns
      } else {
            for (i in 1:length(columns)) (if (names(columns)[i] == "") (names(columns)[i] <- columns[i]))
      }
      
      UpdateTable(table = "mcAnswers", newDF = newMCAnswersDataFrame, columns = columns, vitalColumns = c(ID = columns[["ID"]], questionName = columns[["questionName"]], examName = columns[["examName"]]), asCha = asCha, conn = conn)
}



#' Update "longformGrades" table with new (or corrected) data.
#' 
#' Use one function call per exam.  
#' Key variables identifying a given table row as unique: ID, questionNumber, examName. 
#'    If a row with those values does not exist (eg. if ID or examName were entered wrong initially), 
#'          UpdateLFGrades() will assume that a new row should be added to the table.  
#'          See function removeRow() to fix such occurrances.  
#'    If a row with given values for those parameters already exists, 
#'          UpdateLFGrades() assumes that the line should be corrected.  
#' @family data entry functions
#' @param newLFGradesDataFrame A data.frame with between 3 and 6 columns.
#' \describe{
#'    \item{ID}{The students' IDs (typically 9-digit integers).  Note that 999999999, is used as the ID for correct responses.  Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{date}{\code{\link{date}} class objects.  Do not set asCha equal to TRUE for this column.}
#'    \item{grade}{Numeric values, the grades to be entered.  Note that a set of correct answers should be entered with the ID: 999999999. Do not set asCha equal to TRUE for this column.}
#'    \item{questionName}{The names or numbers of the questions in a given test.  Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{examName}{The name (or number) assigned to an exam. Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{examCode}{Distiguish between different versions of the same exam.  Typically 3-digit integers. Should be entered using as.character() (set asCha equal to TRUE for this column).}
#' }
#' @param columns A vector containing the names of the columns you wish to include.  Set names(columns) to the appropriate database column names.  eg. c(ID = "student.ID", date = "date.of.test", ...).
#' @param asCha A logical vector specifiying which columns to deliberately enter as characters (as opposed to numerics, date objects, etc.).
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' ID = rep(c(567567567, 678678678, 999999999), each = 3)
#' date = rep(Sys.Date(), length(ID))
#' grade = c(8, 5, 2, 9, 4, 4, 10, 5, 5)
#' questionName = rep(1:3, 3)
#' examName = rep("exm1.1(makeup)", length(grade))
#' examCode = rep("101", length(grade))
#' lfDF <- data.frame(ID, date, grade, questionName, examName, examCode)
#' UpdateLFGrades(lfDF, conn = DBconn(tmpcoursedb)) # Enter grades to 3 questions for two students as well as the maximum grades.
#' lfDF$grade <- c(8, 3, 2, 9, 4, 5, 10, 5, 5)
#' UpdateLFGrades(lfDF, conn = DBconn(tmpcoursedb)) # Correct mistaken grade entries.

UpdateLFGrades <- function(newLFGradesDataFrame, columns = c("ID", "date", "grade", "questionName", "examName", "examCode"), asCha = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE), conn = DBconn()) {
      
      # Ensuring that columns has names.
      if (is.null(names(columns))) {
            names(columns) <- columns
      } else {
            for (i in 1:length(columns)) (if (names(columns)[i] == "") (names(columns)[i] <- columns[i]))
      }
      
      UpdateTable(table = "longformGrades", newDF = newLFGradesDataFrame, columns = columns, vitalColumns = c(ID = columns[["ID"]], questionName = columns[["questionName"]], examName = columns[["examName"]]), asCha = asCha, conn = conn)
}

#' Update "classParticipation" table with new (or corrected) data.
#' 
#' The default setting is to assume that all columns other than 'attended' are 
#' necessary to correctly identify whether a row is new (and needs initial 
#' entry) or old (and needs updating).  
#' You, however, may not use all columns.  
#' For example, maybe you do not use 'questionAnswered', 'questionAsked', or 
#' 'participationNotes' at all.  In that case, set 
#' column = c("ID", "date", "attended") and 
#' vitalColumn = c("ID, "date").  In that case, any rows with the same date 
#' and ID are considered the same and new 'attended' values overwrite the old 
#' values for that row.  
#' To delete an existing row, see removeRow().  
#' @family data entry functions
#' @param newCPDataFrame A dataframe with up to six columns.  
#' \describe{
#'    \item{ID}{The students' IDs (typically 9-digit integers).  Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{date}{\code{\link{date}} class objects.  Do not set asCha equal to TRUE for this column.}
#'    \item{attended}{Boolean/ logical values, whether students attended on that date.  Do not set asCha equal to TRUE for this column.}
#'    \item{questionAnswered}{Strings, defaulting to ""; the questions the student answered.  Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{questionAsked}{Strings, defaulting to ""; the questions the student asked.  Should be entered using as.character() (set asCha equal to TRUE for this column).}
#'    \item{participationNotes}{Strings, any notes you wish to include.  Should be entered using as.character() (set asCha equal to TRUE for this column).}
#' }
#' @param columns A vector containing the names of the columns you wish to include.  Set names(columns) to the appropriate database column names.  eg. c(ID = "student.ID", date, attended = "there.today", ...).
#' @param vitalColumns A vector containing the names of the columns by which UpdateClassParticipation() is to recognize whether a row is already in the database and needs updating or is new and is to be appended.  Set names(vitalColumns) to the appropriate database column names.  eg. c(ID = "student.ID", date = "DATE", questionAnswered = "qA", ...).
#' @param asCha A logical vector specifiying which columns to deliberately enter as characters (as opposed to numerics, date objects, etc.).
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @seealso \code{\link{date}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' cpDF <- data.frame(ID = c(993456888, 222222229, 222222229), attended = c(FALSE, TRUE, TRUE), questionAnswered = c("", "Q3", ""), questionAsked = c("", "", "Why is option pricing so complicated?"), participationNotes = c("", "", "Came in late. Again."), date = rep(Sys.Date(), 3) ) 
#' UpdateClassParticipation(cpDF, conn = DBconn(tmpcoursedb)) # Enters three new participation records.
#' cpDF$attended <- c(TRUE, TRUE, TRUE)
#' UpdateClassParticipation(cpDF, conn = DBconn(tmpcoursedb)) # Amends attendance for 1st student.

UpdateClassParticipation <- function(newCPDataFrame, columns = c("ID", "date", "attended", "questionAnswered", "questionAsked", "participationNotes"), vitalColumns = c("ID", "date", "questionAnswered", "questionAsked", "participationNotes"), asCha = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE), conn = DBconn()) {
      
      # Ensuring that columns and vitalColumns have names.
      if (is.null(names(columns))) {
            names(columns) <- columns
      } else {
            for (i in 1:length(columns)) (if (names(columns)[i] == "") (names(columns)[i] <- columns[i]))
      }
      if (is.null(names(vitalColumns))) {
            names(vitalColumns) <- vitalColumns
      } else {
            for (i in 1:length(vitalColumns)) (if (names(vitalColumns)[i] == "") (names(vitalColumns)[i] <- vitalColumns[i]))
      }
      
      UpdateTable(table = "classParticipation", newDF = newCPDataFrame, columns = columns, vitalColumns = vitalColumns, asCha = asCha, conn = conn)
}