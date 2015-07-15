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

#' Enter a new row into the "assignments" table.
#' 
#' @family data entry functions
#' 
#' @param ID A student's ID number (should be a 9 digit integer).  Note that 999999999, the default, is used as the ID for correct responses.  
#' @param assignmentNumber A string, the number (or name) of an assignment.
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @param grade A numeric value, the grade the student achieved on the assignment.  Note that a set of perfect grades should be entered with the ID: 999999999.  
NewAssignmentEntry <- function(ID = 999999999, assignmentNumber, date = Sys.Date(), grade) {
      df <- data.frame(ID = ID, assignmentNumber = assignmentNumber, date = date, grade = grade)
      sql <- "INSERT INTO assignments VALUES (:ID, :assignmentNumber, :date, :grade)" 
      dbGetPreparedQuery(conn, sql, bind.data = df)      
}
# NewAssignmentEntry(ID = 111444777,
#                    assignmentNumber = 4,
#                    grade = 10)
# readAssignments()


#' Enter a new row into the "mcAnswers" table.
#' 
#' @family data entry functions
#' 
#' @param ID A student's ID number (should be a 9 digit integer).  Note that 999999999, the default, is used as the ID for correct responses.  
#' @param answer An integer, the multiple choice answer given by the student.  Note that a set of correct answers should be entered with the ID: 999999999.
#' @param questionNumber The number of the question on the test.
#' @param questionValue A numeric value, the marks that question is worth.
#' @param examNumber A string, the number (or name) assigned to an exam.
#' @param examCode A string, typically a 3-digit integer, but exam versions 
#'    can be distinguished from each other using names.  
NewMCEntry <- function(ID = 999999999, answer, questionNumber, questionValue = 1, examNumber, examCode, date = Sys.Date()) {
      df <- data.frame(ID = as.integer(ID), answer = as.integer(answer), questionNumber = as.integer(questionNumber), questionValue = questionValue, examNumber = examNumber, date = date, examCode = examCode)
      sql <- "INSERT INTO mcAnswers VALUES (:ID, :answer, :questionNumber, :questionValue, :examNumber, :date, :examCode)"
      dbGetPreparedQuery(conn, sql, bind.data = df)      
}
# NewMCEntry(ID = 111444777,
#            answer = 1,
#            questionNumber = 1,
#            questionValue = 2,
#            examNumber = 4,
#            examCode = 101)
# readMCAnswers()
dfA <- mcMatrix
dfQV <- data.frame(questionValues = questionValues, 
                   examNumber = examNumber, 
                   date = date, 
                   examCode = examCode)
rownames(dfQV) <- questionNumbers
dfQV
dfA
dfLarge <- data.frame()
for (i in 1:length(questionNumbers)) {
      for (j in 1:length(idVector)) {
            dfRow <- c(dfA[j,i], questionValues[i])
            rownames(dfRow) <-
                  ******* Name rows and columns? of dfLarge?  Tack on always-same info after (eg. exam code)
            dfLarge <- rbind(dfLarge, dfRow)
      }
}

#' Enter a new row into the "longFormGrades" table.
#' 
#' @family data entry functions
#' @param ID A student's ID number (should be a 9 digit integer).  Note that 999999999, the default, is used as the ID for correct responses.  
#' @param grade A numeric value, the grade the student achieved on the assignment.  Note that a set of perfect grades should be entered with the ID: 999999999.  
#' @param questionNumber The number of the question on the test.
#' @param examNumber A string, the number (or name) assigned to an exam.
#' @param examCode A string, typically a 3-digit integer, but exam versions 
#'    can be distinguished from each other using names.  
#'    
NewLFEntry <- function(ID = 999999999, grade, questionNumber, examNumber, examCode, date = Sys.Date()) {
      df <- data.frame(ID = as.integer(ID), grade = grade, questionNumber = as.integer(questionNumber), examNumber = examNumber, date = date, examCode = examCode)
      sql <- "INSERT INTO longformGrades VALUES (:ID, :grade, :questionNumber, :examNumber, :date, :examCode)"
      dbGetPreparedQuery(conn, sql, bind.data = df)
}
# NewLFEntry(ID = 111444777,
#            grade = 10,
#            questionNumber = 1,
#            examNumber = 4,
#            examCode = 101)
# readLongformGrades()




#' Enter a new row into the "classParticipation" table.
#' 
#' @family data entry functions
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @param attended A boolean value, whether the student attended on that date.
#' @param questionAnswered A string, the question the student answered.  
#' @param questionAsked A string, the question the student asked.  
#' @param notes A string, any notes you wish to include.
#'    
NewCPEntry <- function(ID, date = Sys.Date(), attended = TRUE, questionAnswered = "", questionAsked = "", participationNotes = "") {
      df <- data.frame(ID = as.integer(ID), date = date, attended = attended, questionAnswered = questionAnswered, questionAsked = questionAsked, participationNotes = participationNotes)
      sql <- "INSERT INTO classParticipation VALUES (:ID, :date, :attended, :questionAnswered, :questionAsked, :participationNotes)"
      dbGetPreparedQuery(conn, sql, bind.data = df)
}
# NewCPEntry(ID = 111444777,
#            attended = FALSE)
# readClassParticipation()