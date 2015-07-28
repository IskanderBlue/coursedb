#' Ensure that correctly formatted dates are stored as numeric values.
#' 
#' @family helper functions
#' @param x A date -- converted into a date class object, then converted into a numeric value.  

numDate <- function(x) as.numeric(as.Date(x))

# Problem: entries saved unnecessarily as decimals

## PROBLEM: terrible way to input data

# Data entry functions.
# 
# A series of functions to update the course.db database.  
# They are not convenient to use; they will definitely be superceded.  

#' Enter a new row into the "students" table.
#' 
#' @family data entry functions
#' 
#' @param ID A student's ID number (should be a 9 digit integer).  Note that 999999999 is used as the ID for correct responses.  
#' @param email A string, an email address.
#' @param lastName A string, the student's last name.
#' @param givenNames A string, that student's first name (and any middle names used).  
#' @param program A string, a program name.  Only use a few standardized names.  You don't want "econ", "Econ", "economics", etc.
#' @param notes A string, any notes you wish to include.  
NewStudentEntry <- function(ID, email = "", lastName, givenNames, program = "", notes = "") {
      df <- data.frame(ID = ID, email = email, lastName = lastName, givenNames = givenNames, program = program, notes = notes)
      sql <- "INSERT INTO students VALUES (:ID, :email, :lastName, :givenNames, :program, :notes)" 
      dbGetPreparedQuery(conn, sql, bind.data = df)
}
# NewStudentEntry(ID = 111444777, 
#                 email = "ameining@uwo.ca", 
#                 lastName = "Meining", 
#                 givenNames = "Andrew", 
#                 program = "economics", 
#                 notes = "")
# readStudents()

#' Update "assignments" table with new (or corrected) data.
#' 
#' If in a given row of the data.frame parameter (aDF) ID and date are the same as a row in the assignments table, 
#' that row is assumed to be a duplicate and the assignmentNumber and grade values of the existing row in 
#' the table are updated rather than a new row created.  
#' @family data entry functions
#' @param aDF A dataframe with four columns.  
#' \describe{
#'    \item{ID}{Strings, the students' IDs (typically 9-digit integers).}
#'    \item{assignmentNumber}{Strings, the names or numbers of the assignments.}
#'    \item{grade}{Numeric values, the grades recieved on assignments.}
#'    \item{date}{\code{\link{date}} class objects.}
#' }
#' @seealso \code{\link{date}}
#   aDF <- data.frame(ID = c(993456888, 222222229, 222222229), 
#                    assignmentNumber = c("1", "1", "three"), 
#                    grade = c(4, 5.5, 10)
#                    date = rep(Sys.Date(), 3) )
#           UpdateAssignments(aDF)

UpdateAssignments <- function(aDF) {
      ID <- as.character(aDF[ , 1])
      assignmentNumber <- as.character(adF[ , 2])
      grade <- as.numeric(aDF[ , 3])
      date <- numDate(aDF[ , 4])
      df <- data.frame(ID = ID, 
                       assignmentNumber = assignmentNumber, 
                       date = date, 
                       grade = grade)
      # Writing subfunction to actually handle adding/ updating.
      aRowUpdater <- function(df) {
            sql <- "SELECT * FROM assignments AS a WHERE a.ID = :ID AND a.date = :date"
            query <- dbGetPreparedQuery(conn, sql, bind.data = df)      
            if (nrow(query) == 0) {
                  sql <- "INSERT INTO assignments VALUES (:ID, :assignmentNumber, :date, :grade)"
            } else {
                  sql <- "UPDATE assignments 
                  SET assignmentNumber = :assignmentNumber, grade = :grade 
                  WHERE ID = :ID AND date = :date"
            }
            dbGetPreparedQuery(conn, statement = sql, bind.data = df)      
            }
      # Running subfunction
      aRowUpdater(aDF)
      }



#' Update "mcAnswers" table with new (or corrected) data.
#' 
#' Use one function call per exam.  
#' Key variables identifying a given table row as unique: ID, questionNumber, date. 
#'    If a row with those values does not exist (eg. if ID or date were entered wrong initially), 
#'          UpdateMCAnswers() will assume that a new row should be added to the table.  
#'          See function AmendMCEntry() to fix such occurrances.  
#'    If a row with given values for those parameters already exists, 
#'          UpdateMCAnswers() assumes that the line should be corrected.  
#' The questionNumber column in the mcAnswers table is derived from the column 
#'    names of the 'answers' parameter.  If the columns are not named, the 
#'    column's position (1:ncol(answers)) is used.
#' @family data entry functions
#' @param ID A vector (typically of 9-digit integers), students' ID numbers.  
#'          Note that 999999999, is used as the ID for correct responses.  
#' @param answer A vector or matrix, the multiple choice answers to be entered 
#'          or updated.  Each row should be the answers of a specific ID, each 
#'          column the answers to a given question; if the columns are named, 
#'          colnames(answer) is used to name the questions in the questionNumber 
#'          column in the mcAnswers table.
#'          Note that a set of correct answers should be entered with the ID: 999999999.
#' @param examNumber A string, the number (or name) assigned to an exam.
#' @param examCode A string, typically a 3-digit integer, 
#'          but it is entered using as.character().  
#' @param questionNumber A vector of strings containing question names.  
#'          Overwritten by colnames(answer) if that attrib exists.  
#'          If questionNumber is not given as a parameter or as colnames(answer), 
#'                it defaults to 1:ncol(answer).  
#' @param questionValue A vector of numeric values, the marks that question is worth.
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}

UpdateMCAnswers <- function(ID, answer, examNumber, examCode, questionNumber = NULL, questionValue = rep(1, ncol(answer)), date = Sys.Date()) {
      # Generate 'questionNumber' from colnames(answer).
      questionNumber = colnames(answer)
      if (is.null(questionNumber)) {
            questionNumber <- 1:ncol(answer)
      } 
      # Old NewMCEntry() function; updates an SQL database row.
      MCRowUpdater <- function(df) {
            sql <- "SELECT * FROM mcAnswers AS m WHERE m.ID = :ID AND m.questionNumber = :questionNumber AND m.date = :date"
            query <- dbGetPreparedQuery(conn, sql, bind.data = df)      
            if (nrow(query) == 0) {
                  sql <- "INSERT INTO mcAnswers VALUES (:ID, :answer, :questionNumber, :questionValue, :examNumber, :date, :examCode)"
            } else {
                  sql <- "UPDATE mcAnswers 
                  SET answer = :answer, questionValue = :questionValue, examNumber = :examNumber, examCode = :examCode 
                  WHERE ID = :ID AND questionNumber = :questionNumber AND date = :date"
            }
            dbGetPreparedQuery(conn, statement = sql, bind.data = df)      
      }
      
      # Loop through answer matrix, calling MCRowUpdater() to update each 
      # database row appropriately.
      df <- data.frame(examNumber = as.character(examNumber), date = as.Date(date))
      for (i in 1:length(ID)) {
            df$ID <- ID[i]
            df$examCode <- as.character(examCode[i])
            for (j in 1:length(questionNumbers)) {
                  df$answer = as.character(answer[i, j])
                  df$questionNumber = as.character(questionNumber[j])
                  df$questionValue = as.numeric(questionValue[j])
                  MCRowUpdater(df)
            } 
      }
}

#' Update "longformGrades" table with new (or corrected) data.
#' 
#' Use one function call per exam.  
#' Key variables identifying a given table row as unique: ID, questionNumber, date. 
#'    If a row with those values does not exist (eg. if ID or date were entered wrong initially), 
#'          UpdateLFGrades() will assume that a new row should be added to the table.  
#'          See function AmendLFEntry() to fix such occurrances.  
#'    If a row with given values for those parameters already exists, 
#'          UpdateLFGrades() assumes that the line should be corrected.  
#' The questionNumber column in the longformGrades table is derived from the column 
#'    names of the 'grade' parameter.  If the columns are not named, the 
#'    column's position (1:ncol(grade)) is used.
#' @family data entry functions
#' @param ID A vector (typically of 9-digit integers), students' ID numbers.  
#'          Note that 999999999, is used as the ID for correct responses.  
#' @param grade A vector or matrix of numeric elements containing the grades to be entered or updated.  
#'          Each row should be the results for a specific ID, each 
#'          column the grades for a given question; if the columns are named, 
#'          colnames(grade) is used to name the questions in the questionNumber 
#'          column in the longformGrades table.
#'          Note that a set of maximum grades should be entered with the ID: 999999999.
#' @param examNumber A string, the number (or name) assigned to an exam.
#' @param examCode A string, typically a 3-digit integer, 
#'          but it is entered using as.character().  
#' @param questionNumber A vector of strings containing question names.  
#'          Overwritten by colnames(grade) if that attrib exists.  
#'          If questionNumber is not given as a parameter or as colnames(grade), 
#'                it defaults to 1:ncol(grade).  
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}

UpdateLFGrades <- function(ID, grade, examNumber, examCode, questionNumber = NULL, date = Sys.Date()) {
      # Generate 'questionNumber' from colnames(grade).
      questionNumber = colnames(grade)
      if (is.null(questionNumber)) {
            questionNumber <- 1:ncol(grade)
      } 
      # Heart of NewLFEntry() function; updates an SQL database row.
      # Old NewMCEntry() function; updates an SQL database row.
      LFRowUpdater <- function(df) {
            sql <- "SELECT * FROM longformGrades AS l WHERE l.ID = :ID AND l.questionNumber = :questionNumber AND l.date = :date"
            query <- dbGetPreparedQuery(conn, sql, bind.data = df)      
            if (nrow(query) == 0) {
                  sql <- "INSERT INTO longformGrades VALUES (:ID, :grade, :questionNumber, :examNumber, :date, :examCode)"
            } else {
                  sql <- "UPDATE longformGrades 
                  SET grade = :grade, examNumber = :examNumber, examCode = :examCode 
                  WHERE ID = :ID AND questionNumber = :questionNumber AND date = :date"
            }
            dbGetPreparedQuery(conn, statement = sql, bind.data = df)      
      }     
      # Loop through grade matrix, calling LFRowUpdater() to update each 
      # database row appropriately.
      df <- data.frame(examNumber = as.character(examNumber), date = as.Date(date))
      for (i in 1:length(ID)) {
            df$ID <- ID[i]
            df$examCode <- as.character(examCode[i])
            for (j in 1:length(questionNumbers)) {
                  df$grade = as.numeric(grade[i, j])
                  df$questionNumber = as.character(questionNumber[j])
                  LFRowUpdater(df)
            } 
      }
}





#' Update "classParticipation" table with new (or corrected) data.
#' 
#' If ID, questionAnswered, questionAsked, participationNotes, and date are all the same, 
#' a given row is assumed to be a duplicate and the attended value of the existing row in 
#' the table is updated rather than a new row created.  
#' @family data entry functions
#' @param cpDataFrame A dataframe with six columns.  
#' \describe{
#'    \item{ID}{The students' IDs, typically 9-digit integers, but entered as strings.}
#'    \item{attended}{Boolean/ logical values, whether students attended on that date.}
#'    \item{questionAnswered}{Strings, defaulting to ""; the questions the student answered.}
#'    \item{questionAsked}{Strings, defaulting to ""; the questions the student asked.}
#'    \item{participationNotes}{Strings, any notes you wish to include.}
#'    \item{date}{\code{\link{date}} class objects.}
#' }
#' @seealso \code{\link{date}}
#   cpDF <- data.frame(ID = c(993456888, 222222229, 222222229), 
#                    attended = c(F, T, T), 
#                    questionAnswered = c("", "You're tired.", ""),
#                    questionAsked = c("", "", "Why are you so tired?"),
#                    participationNotes = c("", "", "He said sleepily."),
#                    date = rep(Sys.Date(), 3) )
#           UpdateClassParticipation(cpDF)

UpdateClassParticipation <- function(cpDataFrame) {
      # Making sure columns are in the right format.
      ID <- as.character(cpDataFrame[ , 1])
      attended <- as.logical(cpDataFrame[ , 2])
      questionAnswered <- as.character(cpDataFrame[ , 3])
      questionAsked <- as.character(cpDataFrame[ , 4])
      participationNotes <- as.character(cpDataFrame[ , 5])
      date <- numDate(cpDataFrame[ , 6])
      df <- data.frame(ID = as.character(cpDataFrame[ , 1]), 
                       attended = as.logical(cpDataFrame[ , 2]), 
                       questionAnswered = as.character(cpDataFrame[ , 3]), 
                       questionAsked = as.character(cpDataFrame[ , 4]), 
                       participationNotes = as.character(cpDataFrame[ , 5]), 
                       date = numDate(cpDataFrame[ , 6]))
      # Writing subfunction to actually handle adding/ updating.
      CPRowUpdater <- function(df) {
            sql <- "SELECT * FROM classParticipation AS c WHERE c.ID = :ID AND c.questionAnswered = :questionAnswered AND c.questionAsked = :questionAsked AND c.participationNotes = :participationNotes AND c.date = :date"
            query <- dbGetPreparedQuery(conn, sql, bind.data = df)      
            if (nrow(query) == 0) {
                  sql <- "INSERT INTO classParticipation VALUES (:ID, :date, :attended, :questionAnswered, :questionAsked, :participationNotes)"
            } else {
                  sql <- "UPDATE classParticipation 
                  SET attended = :attended
                  WHERE ID = :ID AND questionAnswered = :questionAnswered AND questionAsked = :questionAsked AND participationNotes = :participationNotes AND date = :date"
            }
            dbGetPreparedQuery(conn, statement = sql, bind.data = df)      
      }
      # Running subfunction
      CPRowUpdater(cpDataFrame)
}