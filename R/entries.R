# Data entry functions.
# 
# A series of functions to update the course.db database.  


#' Update "students" table with new (or corrected) data.
#' 
#' One function call can update an arbitrary number of student entries.  
#' The key variable identifying a given table row as unique is ID.  
#'    If a row with that ID value does not exist (eg. if ID was entered wrong initially), 
#'          UpdateMCAnswers() will assume that a new row should be added to the table.  
#'          See function AmendStudents() to fix such occurrances.  
#'    If a row with a given value for ID already exists, 
#'          UpdateStudents() assumes that the row should be corrected.  
#' @family data entry functions
#' @param sDF A dataframe with at least a column named "ID"; recommended: email, lastName, givenNames, perhaps program, notes, or whatever else you may wish to include in the students table of your database.  For example:
#' \describe{
#'    \item{ID}{Strings, the students' IDs (typically 9-digit integers). Note that 999999999 is used as the ID for correct responses.}
#'    \item{email}{Strings, the email addresses.}
#'    \item{lastName}{Strings, the students' last names.}
#'    \item{givenNames}{Strings, the students' first (and possibly middle) names.}
#'    \item{program}{Strings, the program a student is enrolled in.}
#'    \item{notes}{Strings, any notes you would like to include.}
#' }
#' @param columns A vector containing the names of the columns you wish to include.
#' @examples sDF <- data.frame(ID = c("asd", 222222229, 122222229), email = c(NA, "222semail@@address.com", "abcd@@email.com"), lastName = c("Abelard", "Semekovic", "Kovacs"), givenNames = c("Eugene", "Juliana", "Takeshi"), program = c("statistics", "financial modelling", ""), notes = c("", "", "Terrifying.") ) 
#' UpdateStudents(sDF) # Adds three lines to the students table.
#' sDF <- data.frame(ID = c("asd", 222222229, 122222229), email = c(NA, "amended.email@@address.com", "abcd@@email.com"), lastName = c("Abelard", "Semekovic", "Kovacs"), givenNames = c("Eugene", "Juliana", "Takeshi"), program = c("statistics", "financial modelling", ""), notes = c("", "Amended line.", "Terrifying.") ) 
#' UpdateStudents(sDF) # Amends student 222222229's entry.

UpdateStudents <- function(sDF, columns = c(ID= "ID", email = "email", lastName = "lastName", givenNames = "givenNames", program = "program", notes = "notes")) {
      # Cutting 'sDF' data.frame down to only those columns listed in 'columns'
      sDF[ , names(sDF) %in% columns]
      
      sql1 <- "SELECT * FROM students AS s WHERE s.ID = :ID"
      # Cobbling together ifsql and elsesql from info in 'columns'
      ifVarNames <- columns[1]
      for (i in columns[-1]){
            ifVarNames <- paste(ifVarNames, ", :", i, sep = "")
      }
      ifsql <- paste("INSERT INTO students VALUES (:", ifVarNames, ")", sep = "")
      elseVarNames <- paste(columns[columns != "ID"][1], " = :", columns[columns != "ID"][1], sep = "")
      for (i in columns[columns != "ID"][-1]) {
            currentPiece <- paste(columns[columns != "ID"][i], " = :", columns[columns != "ID"][i], sep = "")
            elseVarNames <- paste(elseVarNames, ", ", currentPiece, sep = "")
      }
      elsesql <- paste("UPDATE students SET ",  elseVarNames, "WHERE ID = :ID", sep = "")
      # Loop through variables, calling rowUpdater() to update each 
      # database row appropriately.
      for (i in 1:nrow(sDF)) {
            df <- sDF[i, ]
            rowUpdater(df, sql1, ifsql, elsesql)
      }
}


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
#' @examples aDF <- data.frame(ID = c(993456888, 222222229, 222222229), assignmentNumber = c("1", "1", "three"), grade = c(4, 5.5, 10), date = rep(Sys.Date(), 3) ) 
#' UpdateAssignments(aDF) # Creates three new rows in assignments table.
#' aDF <- data.frame(ID = c(993456888, 222222229, 222222229), assignmentNumber = c("1", "1", "three"), grade = c(4, 5.5, 9), date = rep(Sys.Date(), 3) ) 
#' UpdateAssignments(aDF) # Reduces grade in last entry from 10 to 9.
 
UpdateAssignments <- function(aDF) {
      ID <- as.character(aDF[ , 1])
      assignmentNumber <- as.character(aDF[ , 2])
      grade <- as.numeric(aDF[ , 3])
      date <- numDate(aDF[ , 4])
      sql1 <- "SELECT * FROM assignments AS a WHERE a.ID = :ID AND a.date = :date"
      ifsql <- "INSERT INTO assignments VALUES (:ID, :assignmentNumber, :date, :grade)"
      elsesql <- "UPDATE assignments SET assignmentNumber = :assignmentNumber, grade = :grade WHERE ID = :ID AND date = :date"
      # Loop through variables, calling rowUpdater() to update each 
      # database row appropriately.
      for (i in 1:length(ID)) {
            df <- data.frame(ID = ID[i], 
                             assignmentNumber = assignmentNumber[i],
                             date = date[i],
                             grade = grade[i])
            rowUpdater(df, sql1, ifsql, elsesql)
      }
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
#' @param examCode A vector of strings, typically 3-digit integers, 
#'          but it is entered using as.character().  
#' @param questionNumber A vector of strings containing question names.  
#'          Overwritten by colnames(answer) if that attrib exists.  
#'          If questionNumber is not given as a parameter or as colnames(answer), 
#'                it defaults to 1:ncol(answer).  
#' @param questionValue A vector of numeric values, the marks that question is worth.
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @examples #studentIDs <- c(567567567, 678678678, 999999999)
#' answerMatrix <- matrix(data = c(1, 1, 1, 2, 3, 2, 3, 2, 3), nrow = 3)
#' examName <- "exm1.1(makeup)"
#' examCodes <- c(101, 101, 101) # All the same exam version.
#' currentDate <- Sys.Date()
#' UpdateMCAnswers(studentIDs, answerMatrix, examName, examCodes, date = currentDate) # Enter answers to 3 MC questions for two students as well as the correct answers.
#' newQuestionValues <- c(2, 2, 1) 
#' UpdateMCAnswers(studentIDs, answerMatrix, examName, examCodes, date = currentDate, questionValue = newQuestionValues) # Amend the question values from c(1, 1, 1) to c(2, 2, 1).

UpdateMCAnswers <- function(ID, answer, examNumber, examCode, questionNumber = NULL, questionValue = rep(1, ncol(answer)), date = Sys.Date()) {
      # Generate 'questionNumber' from colnames(answer).
      questionNumber = colnames(answer)
      if (is.null(questionNumber)) {
            questionNumber <- 1:ncol(answer)
      } 
      sql1 <- "SELECT * FROM mcAnswers AS m WHERE m.ID = :ID AND m.questionNumber = :questionNumber AND m.date = :date"
      ifsql <- "INSERT INTO mcAnswers VALUES (:ID, :answer, :questionNumber, :questionValue, :examNumber, :date, :examCode)"
      elsesql <- "UPDATE mcAnswers 
      SET answer = :answer, questionValue = :questionValue, examNumber = :examNumber, examCode = :examCode 
      WHERE ID = :ID AND questionNumber = :questionNumber AND date = :date"
      # Loop through answer matrix, calling rowUpdater() to update each 
      # database row appropriately.
      df <- data.frame(examNumber = as.character(examNumber), date = as.Date(date))
      for (i in 1:length(ID)) {
            df$ID <- ID[i]
            df$examCode <- as.character(examCode[i])
            for (j in 1:length(questionNumber)) {
                  df$answer = as.character(answer[i, j])
                  df$questionNumber = as.character(questionNumber[j])
                  df$questionValue = as.numeric(questionValue[j])
                  rowUpdater(df, sql1, ifsql, elsesql)
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
#' @examples #studentIDs <- c(567567567, 678678678, 999999999)
#' gradeMatrix <- matrix(data = c(8, 9, 10, 5, 4, 5, 2, 4, 5), nrow = 3)
#' examName <- "exm1.1(makeup)"
#' examCodes <- c(101, 101, 101) # All the same exam version.
#' currentDate <- Sys.Date()
#' UpdateLFGrades(studentIDs, gradeMatrix, examName, examCodes, date = currentDate) # Enter grades to 3 questions for two students as well as the maximum grades.
#' newGradeMatrix <- matrix(data = c(8, 9, 10, 3, 4, 5, 2, 5, 5), nrow = 3)
#' UpdateMCAnswers(studentIDs, newGradeMatrix, examName, examCodes, date = currentDate) # Correct mistaken grade entries.

UpdateLFGrades <- function(ID, grade, examNumber, examCode, questionNumber = NULL, date = Sys.Date()) {
      # Generate 'questionNumber' from colnames(grade).
      questionNumber = colnames(grade)
      if (is.null(questionNumber)) {
            questionNumber <- 1:ncol(grade)
      }
      sql1 <- "SELECT * FROM longformGrades AS l WHERE l.ID = :ID AND l.questionNumber = :questionNumber AND l.date = :date"
      ifsql <- "INSERT INTO longformGrades VALUES (:ID, :grade, :questionNumber, :examNumber, :date, :examCode)"
      elsesql <- "UPDATE longformGrades 
      SET grade = :grade, examNumber = :examNumber, examCode = :examCode 
      WHERE ID = :ID AND questionNumber = :questionNumber AND date = :date"
      # Loop through grade matrix, calling rowUpdater() to update each 
      # database row appropriately.
      df <- data.frame(examNumber = as.character(examNumber), date = as.Date(date))
      for (i in 1:length(ID)) {
            df$ID <- ID[i]
            df$examCode <- as.character(examCode[i])
            for (j in 1:length(questionNumber)) {
                  df$grade = as.numeric(grade[i, j])
                  df$questionNumber = as.character(questionNumber[j])
                  rowUpdater(df, sql1, ifsql, elsesql)
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
#' @examples cpDF <- data.frame(ID = c(993456888, 222222229, 222222229), attended = c(F, T, T), questionAnswered = c("", "Q3", ""), questionAsked = c("", "", "Why is option pricing so complicated?"), participationNotes = c("", "", "Came in late. Again."), date = rep(Sys.Date(), 3) ) 
#' UpdateClassParticipation(cpDF) # Enters three new participation records.
#' cpDF$attended <- c(T, T, T)
#' UpdateClassParticipation(cpDF) # Amends attendance for 1st student.

UpdateClassParticipation <- function(cpDataFrame) {
      # Making sure vectors are in the right format.
      ID <- as.character(cpDataFrame[ , 1])
      attended <- as.logical(cpDataFrame[ , 2])
      questionAnswered <- as.character(cpDataFrame[ , 3])
      questionAsked <- as.character(cpDataFrame[ , 4])
      participationNotes <- as.character(cpDataFrame[ , 5])
      date <- numDate(cpDataFrame[ , 6])
      sql1 <- "SELECT * FROM classParticipation AS c WHERE c.ID = :ID AND c.questionAnswered = :questionAnswered AND c.questionAsked = :questionAsked AND c.participationNotes = :participationNotes AND c.date = :date"
      ifsql <- "INSERT INTO classParticipation VALUES (:ID, :date, :attended, :questionAnswered, :questionAsked, :participationNotes)"
      elsesql <- "UPDATE classParticipation 
      SET attended = :attended
      WHERE ID = :ID AND questionAnswered = :questionAnswered AND questionAsked = :questionAsked AND participationNotes = :participationNotes AND date = :date"
      # Loop through variables, calling rowUpdater() to update each 
      # database row appropriately.
      df <- data.frame()
      for (i in 1:length(ID)) {
            df <- data.frame(ID = ID[i], 
                             attended = attended[i], 
                             questionAnswered = questionAnswered[i], 
                             questionAsked = questionAsked[i], 
                             participationNotes = participationNotes[i], 
                             date = date[i])
            rowUpdater(df, sql1, ifsql, elsesql)
      }
}