#' Basic outputs
#' 
#' List course.db's tables and return their contents.  
#' \describe{
#'    \item{listTables()}{Returns a vector of strings containing the names of 
#'          the tables for reference.}  
#'    \item{showTable()}{Takes one of the strings returned by listTables() as 
#'          an input and returns that table's contents.}
#'    \item{readStudents()}{Returns the contents of the "students" table.}
#'    \item{readAssignments()}{Returns the contents of the "assignments" table.}
#'    \item{readMCAnswers()}{Returns the contents of the "mcAnswers" table.}
#'    \item{readLFGrades()}{Returns the contents of the "longFormGrades" table.}
#'    \item{classParticipation()}{Returns the contents of the "classParticipation" table.}
#'    \item{readAllInfo()}{Returns the contents of all tables.}
#' }
#' @seealso \code{\link{createDB}}
#' 
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @param table The name of a table to be shown.  Enter in quotations ("table").
#' @examples tbls <- listTables() # Assign names of tables to vector.
#' showTable(tbls[3]) # Show the 3rd table, longformGrades.
#' readStudents() # Show the students table.
#' readAllInfo() # List the entries in all tables by columns.
#' 
#' @name basicOutputs
NULL
#> NULL

#' @rdname basicOutputs
listTables <- function(conn = DBconn()) {
      dbListTables(conn)
}

# Not for users; shows internal rowNames and del columns.  
#' @rdname basicOutputs
trueTable <- function(table, conn = DBconn()) {
      dbGetQuery(conn, paste("SELECT * FROM", table))
}

#' @rdname basicOutputs
showTable <- function(table, conn = DBconn()) {
      if (table == "students") {
            dbGetQuery(conn, "SELECT ID, email, givenNames, lastname, program, notes FROM students AS t WHERE t.del = 0")
      } else if (table == "assignments") {
            dbGetQuery(conn, "SELECT ID, date, grade, assignmentName FROM assignments AS t WHERE t.del = 0")
      } else if (table == "mcAnswers") {
            dbGetQuery(conn, "SELECT ID, date, answer, questionName, questionValue, examName, examCode FROM mcAnswers AS t WHERE t.del = 0")
      } else if (table == "longformGrades") {
            dbGetQuery(conn, "SELECT ID, date, grade, questionName, examName, examCode FROM longformGrades AS t WHERE t.del = 0")
      } else if (table == "classParticipation") {
            dbGetQuery(conn, "SELECT ID, date, attended, questionAnswered, questionAsked, participationNotes FROM classParticipation AS t WHERE t.del = 0")
      } else {
            print("Invalid table name.")
      }
}

#' @rdname basicOutputs
readStudents <- function(conn = DBconn()) {
      dbGetQuery(conn, "SELECT ID, email, givenNames, lastname, program, notes FROM students AS t WHERE t.del = 0")
}

#' @rdname basicOutputs
readAssignments <- function(conn = DBconn()) {
      dbGetQuery(conn, "SELECT ID, date, grade, assignmentName FROM assignments AS t WHERE t.del = 0")
}

#' @rdname basicOutputs
readMCAnswers <- function(conn = DBconn()) {
      dbGetQuery(conn, "SELECT ID, date, answer, questionName, questionValue, examName, examCode FROM mcAnswers AS t WHERE t.del = 0")
}

#' @rdname basicOutputs
readLFGrades <- function(conn = DBconn()) {
      dbGetQuery(conn, "SELECT ID, date, grade, questionName, examName, examCode FROM longformGrades AS t WHERE t.del = 0")
}

#' @rdname basicOutputs
readClassParticipation <- function(conn = DBconn()) {
      dbGetQuery(conn, "SELECT ID, date, attended, questionAnswered, questionAsked, participationNotes FROM classParticipation AS t WHERE t.del = 0")
}

#' @rdname basicOutputs
readAllInfo <- function(conn = DBconn()) {
      a <- dbGetQuery(conn, "select * from students")
      b <- dbGetQuery(conn, "select * from assignments")
      c <- dbGetQuery(conn, "select * from mcAnswers")
      d <- dbGetQuery(conn, "select * from longformGrades")
      e <- dbGetQuery(conn, "select * from classParticipation")
      return(c(a,b,c,d,e))
}

#' Like readAssignments(), but more legiblle
#' 
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
showAssignments <- function(conn = DBconn()) {
      a <- dbGetQuery(conn, "SELECT ID, date, grade, assignmentName FROM assignments AS t WHERE t.del = 0")
      a$date <- as.Date(a$date, origin = "1970-01-01")
      wide <- reshape(a, direction = "wide", timevar = "assignmentName", idvar = "ID")
      return(wide)
}

## Name variants!!! LIKE in SQL?
## phonetic matching somehow?

#' Get student's ID number from their name.  
#' 
#' Enter a student's name -- given names in the first string, last name in the second.  
#' NameToID() will return a dataframe with the IDs and names of all students 
#' with names similar (according to \code{\link{agrep}}) to the strings the user 
#' gives.  
#' 
#' @param givenNames A string, a student's first name (and any middle names used).  
#' @param lastName A string,  that student's last name.
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A dataframe containing the IDs, given names, and last names of all matches.
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' NameToID("Jarl", "Imhotep", conn = DBconn(tmpcoursedb))

NameToID <- function(givenNames, lastName, conn = DBconn()) {
      IDs <- dbGetQuery(conn, 
                        "SELECT ID, lastName, givenNames 
                               FROM students AS t WHERE t.del = 0")
      givenMatch <- agrep(givenNames, IDs$givenNames)
      lastMatch <- agrep(lastName, IDs$lastName)
      matchingIDs <- IDs[intersect(givenMatch, lastMatch),]
      if (nrow(matchingIDs) == 0) {warning("No matching IDs.")}
      if (nrow(matchingIDs) > 1) {warning("More than one ID.")}
      return(matchingIDs)
}

#' Get a given student's grade on a given exam.  
#' 
#' Enter a student's ID and the name (or 'examName') of an exam.  
#' Returns their score, the score on a perfectly completed exam, and their 
#' score as a fraction.
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param examName A string, the number (or name) assigned to an exam.
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A vector with 3 elements.  
#' \enumerate{
#'    \item The student's score.
#'    \item The full potential score.
#'    \item The student's score as a fraction of the full score.  
#' }
#' @seealso \code{\link{IDToAssignmentMark}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' IDAndExamNameToGrade(555555555, "1", conn = DBconn(tmpcoursedb))
IDAndExamNameToGrade <- function(ID, examName, conn = DBconn()) {
      # 1         match ID to answers (mcAnswers) and grades (longformGrades)
      df <- as.data.frame(cbind(ID, examName))
      a <- dbGetPreparedQuery(conn,
                              "SELECT answer, questionName, questionValue, examCode 
                                    FROM mcAnswers AS m 
                                    WHERE m.ID = :ID 
                                    AND m.examName = :examName
                                    AND m.del = 0",
                              bind.data = df)
      if (length(unique(a$examCode)) > 1) {warning("Exam code not unique.")}      
      g <- dbGetPreparedQuery(conn,
                              "SELECT grade, questionName, examCode 
                                    FROM longformGrades AS l 
                                    WHERE l.ID = :ID 
                                    AND l.examName = :examName
                                    AND l.del = 0",
                              bind.data = df)
      if (length(unique(g$examCode)) > 1) {warning("Exam code not unique.")}
      # 2     compare answers to correct answers, grades to total grades
      df <- as.data.frame(cbind(examName, a$examCode[1])); colnames(df) <- c("examName", "examCode")
      ca <- dbGetPreparedQuery(conn,
                               "SELECT answer, questionName, questionValue 
                                    FROM mcAnswers AS m 
                                    WHERE m.ID = 999999999 
                                    AND m.examName = :examName 
                                    AND m.examCode = :examCode
                                    AND m.del = 0",
                               bind.data = df)
      tg <- dbGetPreparedQuery(conn,
                               "SELECT grade, questionName 
                                    FROM longformGrades AS l 
                                    WHERE l.ID = 999999999 
                                    AND l.examName = :examName 
                                    AND l.examCode = :examCode
                                    AND l.del = 0",
                               bind.data = df)
      # 3     tally total mark on test
      total <- (sum(as.integer(a$questionValue[a$questionName == ca$questionName & 
                                                     a$answer == ca$answer])) 
               + sum(as.integer(g$grade)))
      outOf <- sum(as.integer(ca$questionValue)) + sum(as.integer(tg$grade))
      fraction <- total / outOf
      return(c(total = total, outOf = outOf, fraction = fraction))
}


#' Get a given student's mark on a given assignment.    
#' 
#' Enter a student's ID and the name ('assignmentName') of an assignment.  
#' Returns their score, the score on a perfectly completed assignment, and their 
#' score as a fraction.
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param assignmentName A string, the number (or name) of an assignment.
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A vector with 3 elements.  
#' \enumerate{
#'    \item The student's score.
#'    \item The full potential score.
#'    \item The student's score as a fraction of the full score.  
#' }
#' @seealso \code{\link{IDAndExamNameToGrade}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' IDToAssignmentMark("555555555", "2", conn = DBconn(tmpcoursedb))
IDToAssignmentMark <- function(ID, assignmentName, conn = DBconn()) {
      # 1   Match ID and assignmentName to score (from assignments table)
      #     and get full score for assignment
      df <- as.data.frame(cbind(ID, assignmentName))
      a <- dbGetPreparedQuery(conn,
                              "SELECT grade, ID FROM assignments AS a 
                                    WHERE a.ID = :ID 
                                    AND a.assignmentName = :assignmentName
                                    AND a.del = 0
                                    OR a.ID = 999999999
                                    AND a.assignmentName = :assignmentName
                                    AND a.del = 0",
                              bind.data = df)
      if (length(unique(a$assignmentName)) > 2) {stop("Assignment name not unique.")}
      # 2   Return vector: mark, fraction
      return(c(mark = a[1, 1], outOf = a[2, 1], fraction = a[1, 1] / a[2, 1]))
}

#' Get a given student's attendance grade.    
#' 
#' Enter a student's ID, the date up to which to count attendance, 
#' and an evaluation method.  Returns their attendance grade as evaluated by 
#' the chosen method.  
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @param attendanceMethod One of
#'    \enumerate{
#'          \item A string, "toDate" -- evaluate attendance as a fraction of 
#'                all dates attended by the student out of all unique dates 
#'                entered in the "classParticipation" table up until the date 
#'                parameter.  
#'          \item A numeric value --  evaluate attendance by dividing the 
#'                number of dates attended up until the date parameter by the 
#'                numeric value entered, capped at 1.  
#'    }
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A numeric value.  
#' @seealso \code{\link{IDToClassParticipation}}
#' @examples IDToAttendance(555555555)
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' IDToAttendance(111111111, conn = DBconn(tmpcoursedb)) # 4 / 5 classes attended
#' IDToAttendance(111111111, attendanceMethod = 4, conn = DBconn(tmpcoursedb)) # One day considered a freebie.  
#' IDToAttendance(111111111, attendanceMethod = 6, conn = DBconn(tmpcoursedb)) # Also possible
#' IDToAttendance(222222222, conn = DBconn(tmpcoursedb)) # 3 / 5 classes attended

IDToAttendance <- function(ID, date = Sys.Date(), attendanceMethod = "toDate", conn = DBconn()) {
      # Attendance weighting default: % of dates so far attended; otherwise x/y(specified), capped at 1.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn, 
                              "SELECT date, attended 
                              FROM classParticipation AS c
                              WHERE c.ID = :ID
                              AND c.del = 0",
                              bind.data = ID)
      attendedSoFar <- length(na.omit(unique(c$date[c$attended == TRUE]))) # Number of dates attended to date
      evalMethod <- checkNumeric(attendanceMethod)
      if (is.numeric(attendanceMethod)) {
            potentialDates <- attendanceMethod
      } else {
            if (attendanceMethod != "toDate") {
                  warning("Invalid attendanceMethod value; defaulting to 'toDate'.")
            } 
            potentialDates <- length(unique(c$date[c$date <= date]))
      }
      attendanceGrade <- min(attendedSoFar/potentialDates, 1)
      return(attendanceGrade)
}



#' Get a mark based off of the number of questions a student has asked.    
#' 
#' Enter a student's ID, the date up to which to count, and an evaluation 
#' method.  Returns a mark according to the evaluation method chosen.  
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @param evalMethod One of
#'    \enumerate{
#'          \item A string, "fraction" -- determine the student's grade as a 
#'                fraction with the number of questions the student has asked 
#'                as the numerator and the of the maximum number of questions 
#'                asked by a single student in the table as the denominator.
#'          \item A numeric value --  if the student has asked at least that 
#'                number of questions, they get full marks; otherwise, none.  
#'    }
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A vector with 2 or 3 elements.  
#' \enumerate{
#'    \item The number of questions the student asked.
#'    \item The student's mark as a fraction.  
#'    \item If a threshold number was given as the evalMethod, the threshold 
#'          for full marks.
#' }
#' @seealso \code{\link{IDToClassParticipation}}
#' @seealso \code{\link{IDToQuestionsAnswered}}
#' @seealso \code{\link{IDToCombinedQuestions}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' IDToQuestionsAsked(111111111, conn = DBconn(tmpcoursedb))
#' IDToQuestionsAsked(222222222, conn = DBconn(tmpcoursedb))
#' IDToQuestionsAsked(333333333, conn = DBconn(tmpcoursedb))
#' IDToQuestionsAsked(333333333, evalMethod = 1, conn = DBconn(tmpcoursedb)) # 100% if at least 1 question answered.
#' IDToQuestionsAsked(222222222, evalMethod = 1, conn = DBconn(tmpcoursedb)) # Still did not meet threshold.

IDToQuestionsAsked <- function(ID, date = Sys.Date(), evalMethod = "fraction", conn = DBconn()) {
      #     Default evaluation method:
      #     Find the maximum number of questions asked.  
      #     Find the fraction of that maximum that the given ID asked. 
      #     Alternate: 100% if >= evalMethod (a number) questions were asked.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn, 
                              "SELECT date, questionAsked
                              FROM classParticipation AS c
                              WHERE c.ID = :ID
                              AND c.del = 0",
                              bind.data = ID)
      qs <- sum(c$questionAsked[c$date <= date] != "")
      evalMethod <- checkNumeric(evalMethod)
      if (is.numeric(evalMethod)) {
            qGrade <- c(questionsAsked = qs, mark = 0, threshold = evalMethod)
            if (qs >= evalMethod) {qGrade[2] <- 1} 
      } else {
            if (evalMethod != "fraction") {warning("Invalid evalMethod; defaulting to 'fraction'.")}
            allQs <- dbGetQuery(conn,
                                "SELECT ID, questionAsked 
                                FROM classParticipation as c
                                WHERE c.del = 0
                                ORDER BY c.ID")
            potentialQs <- qs
            for (i in unique(allQs$ID)) {
                  currentID <- sum(allQs$questionAsked[allQs$ID == i] != "")
                  if (currentID > potentialQs) {potentialQs <- currentID}
            }
            qGrade <- c(questionsAsked = qs, fraction = qs/potentialQs)
      }
      return(qGrade)
}



#' Get a mark based off of the number of questions a student has answered.    
#' 
#' Enter a student's ID, the date up to which to count, and an evaluation 
#' method.  Returns a mark according to the evaluation method chosen.  
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @param evalMethod One of
#'    \enumerate{
#'          \item A string, "fraction" -- determine the student's grade as a 
#'                fraction with the number of questions the student has answered 
#'                as the numerator and the of the maximum number of questions 
#'                answered by a single student in the table as the denominator.
#'          \item A numeric value --  if the student has answered at least that 
#'                number of questions, they get full marks; otherwise, none.  
#'    }
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A vector with 2 or 3 elements.  
#' \enumerate{
#'    \item The number of questions the student answered.
#'    \item The student's mark as a fraction.  
#'    \item If a threshold number was given as the evalMethod, the threshold 
#'          for full marks.
#' }
#' @seealso \code{\link{IDToClassParticipation}}
#' @seealso \code{\link{IDToQuestionsAsked}}
#' @seealso \code{\link{IDToCombinedQuestions}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' IDToQuestionsAnswered(111111111, conn = DBconn(tmpcoursedb))
#' IDToQuestionsAnswered(222222222, conn = DBconn(tmpcoursedb))
#' IDToQuestionsAnswered(333333333, conn = DBconn(tmpcoursedb))
#' IDToQuestionsAnswered(222222222, evalMethod = 3, conn = DBconn(tmpcoursedb)) # Answered 3 questions; full marks.
#' IDToQuestionsAnswered(111111111, evalMethod = 3, conn = DBconn(tmpcoursedb)) # Did not answer 3 questions; no marks.
IDToQuestionsAnswered <- function(ID, date = Sys.Date(), evalMethod = "fraction", conn = DBconn()) {
      #     Default evaluation method:
      #     Find the maximum number of questions answered.  
      #     Find the fraction of that maximum that the given ID answered.
      #     Alternate: 100% if >= evalMethod (a number) questions were answered.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn, 
                              "SELECT date, questionAnswered
                              FROM classParticipation AS c
                              WHERE c.ID = :ID
                              AND c.del = 0",
                              bind.data = ID)
      qs <- sum(c$questionAnswered[c$date <= date] != "")
      evalMethod <- checkNumeric(evalMethod)
      if (is.numeric(evalMethod)) {
            qGrade <- c(questionsAnswered = qs, mark = 0, threshold = evalMethod)
            if (qs >= evalMethod) {qGrade[2] <- 1} 
      } else {
            if (evalMethod != "fraction") {warning("Invalid evalMethod; defaulting to 'fraction'.")}
            allQs <- dbGetQuery(conn,
                                "SELECT ID, questionAnswered 
                                FROM classParticipation as c
                                WHERE c.del = 0
                                ORDER BY c.ID")
            potentialQs <- qs
            for (i in unique(allQs$ID)) {
                  currentID <- sum(allQs$questionAnswered[allQs$ID == i] != "")
                  if (currentID > potentialQs) {potentialQs <- currentID}
            }
            qGrade <- c(questionsAnswered = qs, fraction = qs/potentialQs)
      }
      return(qGrade)
}



#' Get a mark based off of the number of questions a student has asked and answered.    
#' 
#' Enter a student's ID, the date up to which to count, and an evaluation 
#' method.  Returns a mark according to the evaluation method chosen.  
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @param evalMethod One of
#'    \enumerate{
#'          \item A string, "fraction" -- determine the student's grade as a 
#'                fraction with the number of questions the student has asked 
#'                and answered as the numerator and the of the maximum number 
#'                of questions asked and answered by a single student in the 
#'                table as the denominator.
#'          \item A numeric value --  if the student has asked and/ or 
#'                answered at least that number of questions, they get full 
#'                marks; otherwise, none.  
#'    }
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A vector with 2 or 3 elements.  
#' \enumerate{
#'    \item The number of questions the student asked and answered.
#'    \item The student's mark as a fraction.  
#'    \item If a threshold number was given as the evalMethod, the threshold 
#'          for full marks.
#' }
#' @seealso \code{\link{IDToClassParticipation}}
#' @seealso \code{\link{IDToQuestionsAsked}}
#' @seealso \code{\link{IDToQuestionsAnswered}}
#' @seealso \code{\link{IDToCombinedQuestions}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' IDToCombinedQuestions(333333333, conn = DBconn(tmpcoursedb)) # Most responses --> full marks.
#' IDToCombinedQuestions(111111111, conn = DBconn(tmpcoursedb)) # 3 responses while another student gave 5 --> 60%
#' IDToCombinedQuestions(111111111, evalMethod = 3, conn = DBconn(tmpcoursedb)) # 3 as the threshold number --> full marks.
IDToCombinedQuestions <- function(ID, date = Sys.Date(), evalMethod = "fraction", conn = DBconn()) {
      #     Default evaluation method:
      #     Find the maximum number of questions asked and answered.  
      #     Find the fraction of that maximum that the given ID asked and answered.
      #     Alternate: 100% if >= evalMethod (a number) questions were asked and/ or answered.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn, 
                              "SELECT date, questionAnswered, questionAsked
                              FROM classParticipation AS c
                              WHERE c.ID = :ID
                              AND c.del = 0",
                              bind.data = ID)
      qs <- sum(c$questionAsked[c$date <= date] != "") + sum(c$questionAnswered[c$date <= date] != "")
      evalMethod <- checkNumeric(evalMethod)
      if (is.numeric(evalMethod)) {
            qGrade <- c(questions = qs, mark = 0, threshold = evalMethod)
            if (qs >= evalMethod) {qGrade[2] <- 1} 
      } else {
            if (evalMethod != "fraction") {warning("Invalid evalMethod; defaulting to 'fraction'.")}
            allQs <- dbGetQuery(conn,
                                "SELECT ID, questionAnswered, questionAsked
                                FROM classParticipation as c
                                WHERE c.del = 0
                                ORDER BY c.ID")
            potentialQs <- qs
            for (i in unique(allQs$ID)) {
                  currentID <- sum(allQs$questionAsked[allQs$ID == i] != "") + sum(allQs$questionAnswered[allQs$ID == i] != "")
                  if (currentID > potentialQs) {potentialQs <- currentID}
            }
            qGrade <- c(questions = qs, fraction = qs/potentialQs)
      }
      return(qGrade)
}



#' Get a class participation grade by weighting the attendance mark and the 
#' questions asked and/ or answered mark.
#' 
#' Enter a student's ID, the date up to which to count, a weighting of  
#' attendance vs. participation, a method for evaluating attendance, and a 
#' method for evaluating questions.  The function uses 
#' \code{\link{IDToAttendance}}, and one of 
#' \code{\link{IDToQuestionsAnswered}},  \code{\link{IDToQuestionsAsked}}, and 
#' \code{\link{IDToCombinedQuestions}}.  Returns a mark according to the 
#' parameters chosen.
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @param cpWeighting A 2-element vector: \enumerate{
#'    \item The weight to assign to attendance.
#'    \item the weight to assign to questions.
#'    }  
#' @param attendanceMethod Either a string, "toDate", or a numeric value; 
#'    see \code{\link{IDToAttendance}}.  
#' @param questionMethod A 2-element vector; \enumerate{
#'    \item A string -- either "ask", "answer", or "both", depending on 
#'          which function you want to use to evaluate questions for the 
#'          class participation grade.  
#'    \item Either a string, "fraction", or a numeric value.  See the Arguments 
#'          in \code{\link{IDToQuestionsAsked}}, for an explanation.  
#'    }
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A numeric value, the fraction of a full grade the student receives.  
#' @seealso \code{\link{IDToAttendance}}
#' @seealso \code{\link{IDToQuestionsAsked}}
#' @seealso \code{\link{IDToQuestionsAnswered}}
#' @seealso \code{\link{IDToCombinedQuestions}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' IDToClassParticipation(111111111, conn = DBconn(tmpcoursedb))
#' IDToClassParticipation(111111111, questionMethod = c("ask", "fraction"), conn = DBconn(tmpcoursedb)) # Count questions asked instead of answered.
#' IDToClassParticipation(222222222, conn = DBconn(tmpcoursedb))
#' IDToClassParticipation(222222222, cpWeighting = c(1, 4), conn = DBconn(tmpcoursedb)) # Put 80% weight on questions answered.
#' IDToClassParticipation(222222222, cpWeighting = c(0, 1), conn = DBconn(tmpcoursedb)) # Ignore attendance (only count questions).
#' IDToClassParticipation(222222222, cpWeighting = c(0, 1), questionMethod = c("both", "fraction"), conn = DBconn(tmpcoursedb)) # Ignore attendance, count questions asked and answered.
#' IDToClassParticipation(222222222, cpWeighting = c(0, 1), questionMethod = c("both", 3), conn = DBconn(tmpcoursedb)) # Same as previous, but use a threshold of 3 responses.
#' IDToClassParticipation(333333333, questionMethod = c("both", "fraction"), conn = DBconn(tmpcoursedb)) # Full attendance, the most questions asked/answered.
IDToClassParticipation <- function(ID, date = Sys.Date(), cpWeighting = c(0.5,0.5), attendanceMethod = "toDate", questionMethod = c("answer", "fraction"), conn = DBconn()) {
      # Weighting default: .5 attendance, .5 questions
      if (is.numeric(cpWeighting) != TRUE) {
            warning("IDToClassParticipation requires 'cpWeighting' to be a vector of two numbers; defaulting to c(0.5, 0.5).")
            cpWeighting <- c(0.5, 0.5)
      }
      # Normalizing cpWeighting
      cpW <- cpWeighting/sum(cpWeighting)

      a <- IDToAttendance(ID, date, attendanceMethod, conn)
      if (questionMethod[1] == "ask") {
            q <- IDToQuestionsAsked(ID, date, questionMethod[2], conn)
      } else if (questionMethod[1] == "both") {
            q <- IDToCombinedQuestions(ID, date, questionMethod[2], conn)
      } else {
            if (questionMethod[1] != "answer") {
                  warning("questionMethod[1] is invalid; defaulting to 'answer'.")
            }
            q <- IDToQuestionsAnswered(ID, date, questionMethod[2])
      }
      
      return(a * cpW[1] + q[2] * cpW[2])
}


#' Get a student's marks on assignments.    
#' 
#' Enter a student's ID and date.  Function uses \code{\link{IDToAssignmentMark}}.  
#' Returns a matrix of a student's marks on assignments up to that date.  
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A matrix with 3 columns:  
#' \enumerate{
#'    \item The student's marks on assignments.
#'    \item The full potential marks.
#'    \item The student's marks as fractions of the full marks.  
#' }
#' @seealso Uses \code{\link{readAssignments}}
#' @seealso \code{\link{IDToAssignmentMark}}
#' @seealso \code{\link{TestMarks}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' AssignmentMarks("111111111", conn = DBconn(tmpcoursedb)) # Check student 111111111's assignment marks.

AssignmentMarks <- function(ID, date = Sys.Date(), conn = DBconn()) {
      a <- readAssignments(conn)
      uniqueA <- unique(a$assignmentName[a$date <= date])
      aMarks <- matrix(ncol = 3, nrow = length(uniqueA))
      colnames(aMarks) = c("mark", "outOf", "fraction")
      rownames(aMarks) = uniqueA
      for (i in uniqueA) {
            aMarks[i,] <- IDToAssignmentMark(ID, i)
      }
      return(aMarks)
}


#' Get a student's marks on tests.    
#' 
#' Enter a student's ID and date.  Function uses \code{\link{IDAndExamNameToGrade}}.  
#' Returns a matrix of a student's marks on tests up to that date.  
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A matrix with 3 columns:  
#' \enumerate{
#'    \item The student's marks on tests.
#'    \item The full potential marks.
#'    \item The student's marks as fractions of the full marks.  
#' }
#' @seealso \code{\link{IDAndExamNameToGrade}}
#' @seealso \code{\link{AssignmentMarks}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#'
#' TestMarks("111111111", conn = DBconn(tmpcoursedb))
TestMarks <- function(ID, date = Sys.Date(), conn = DBconn()) {
      allMC <- readMCAnswers(conn)
      allLF <- readLFGrades(conn)
      uniqueT <- unique(c(unique(allMC$examName[allMC$date <= date])), unique(allLF$examName[allLF$date <= date]))
      tMarks <- matrix(ncol = 3, nrow = length(uniqueT))
      colnames(tMarks) = c("mark", "outOf", "fraction")
      rownames(tMarks) = uniqueT
      for (i in uniqueT) {
            tMarks[i, ] <- IDAndExamNameToGrade(ID, i)
      }
      for (i in rev(seq_len(nrow(tMarks)))) {
            if (is.na(tMarks[i, 2]) || tMarks[i, 2] == 0) {
                  tMarks <- tMarks[-i,]
            }
      }
      return(tMarks)
}

#' Get student's overall grade.  
#' 
#' Calculates and returns a student's overall grade using functions 
#' \code{\link{IDToClassParticipation}}, \code{\link{AssignmentMarks}}, 
#' and \code{\link{TestMarks}}, their inputs, and a vector of weights to 
#' assign to assignments, class participation, (other) tests, and the final exam.  
#' The number will be off if the totalWeighting vector does not add up to 1.  
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param totalWeighting A 4-element vector: \enumerate{
#'    \item The weight to assign to assignments.
#'    \item the weight to assign to class participation.
#'    \item The weight to assign to all tests other than the last one.
#'    \item The weight to assign to the final exam.  
#'    } The overall grade will be off if the totalWeighting vector does not 
#'    sum to 1.  
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @param cpWeighting A 2-element vector: \enumerate{
#'    \item The weight to assign to attendance.
#'    \item the weight to assign to questions.
#'    }
#' @param attendanceMethod Either a string, "toDate", or a numeric value; 
#'    see \code{\link{IDToAttendance}}.  
#' @param questionMethod A 2-element vector; \enumerate{
#'    \item A string -- either "ask", "answer", or "both", depending on 
#'          which function you want to use to evaluate questions for the 
#'          class participation grade.  
#'    \item Either a string, "fraction", or a numeric value.  See the Arguments 
#'          in \code{\link{IDToQuestionsAsked}}, for an explanation.  
#'    }
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A numeric value, the fraction of a full grade the student receives.  
#' @seealso \code{\link{IDToClassParticipation}}
#' @seealso \code{\link{AssignmentMarks}}
#' @seealso \code{\link{TestMarks}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' IDToCurrentGrade(111111111, c(2, 0, 3, 5), conn = DBconn(tmpcoursedb)) # 20% assignments, 30% other tests, 50% final exam
#' IDToCurrentGrade(111111111, c(2, 1, 3, 4), cpWeighting = c(0,1), conn = DBconn(tmpcoursedb)) # 20% assignments, 10% class participation (ignoring attendance) 30% other tests, 40% final exam
IDToCurrentGrade <- function(ID, totalWeighting, date = Sys.Date(), cpWeighting = c(0.5, 0.5), attendanceMethod = "toDate", questionMethod = c("answer", "fraction"), conn = DBconn()) {
      # 1   Determine weighting (eg. assignments = .2, exams = .5)
      #                                   eg.   20% Assignments
      #                                          0% Participation
      #                                         30% Tests
      #                                         50% Final
      tW <- totalWeighting/sum(totalWeighting)
      
      # 2   Fetch all the grades -- assignments, tests, participation, etc.
      # 2.1 Fetch assignments
      a <- AssignmentMarks(ID, date, conn)
      
      # 2.2 Fetch tests
      t <- TestMarks(ID, date, conn)
      
      # 2.3 Fetch class participation
      c <- IDToClassParticipation(ID, date, cpWeighting, attendanceMethod, questionMethod, conn)
      
      # 3   Weight grades according to weighting function.
      aw <- sum(a[,1])/sum(a[,2]) * tW[1]
      cw <- c * tW[2]
      tw <- sum(t[-nrow(t), 1], na.rm = TRUE) / sum(t[-nrow(t), 2]) * tW[3] # Tests excluding final.
      ew <- t[nrow(t), 1]/ t[nrow(t), 2] * tW[4] # Final test/ exam.
      
      return(sum(aw, cw, tw, ew, na.rm = TRUE))
}

#' Retrieve all notes on a given student.
#' 
#' Enter the student's ID.  Returns a data.frame with any notes entered.  
#' 
#' @param ID A string containing a student's ID number (should be a 9 digit integer).
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A data.frame with a single column.  
#' @seealso \code{\link{readStudents}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' IDToNotes("111111111", conn = DBconn(tmpcoursedb))
#' IDToNotes("222222222", conn = DBconn(tmpcoursedb))
#' IDToNotes("333333333", conn = DBconn(tmpcoursedb))
#' IDToNotes("444444444", conn = DBconn(tmpcoursedb)) # No notes. 
IDToNotes <- function(ID, conn = DBconn()) {
      df <- as.data.frame(ID)     
      dbGetPreparedQuery(conn, 
                         "SELECT notes FROM students AS s WHERE s.ID = :ID AND s.del = 0", 
                         bind.data = df)
}

#' Retrieve a given student's name from their ID.
#' 
#' Enter the student's ID.  Returns a data.frame with given names and last name.  
#' 
#' @param ID A string containing a student's ID number (should be a 9 digit integer).
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A data.frame with two columns.  
#' @seealso \code{\link{readStudents}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' IDToName("111111111", conn = DBconn(tmpcoursedb))
#' IDToName("222222222", conn = DBconn(tmpcoursedb))
#' IDToName("333333333", conn = DBconn(tmpcoursedb))
#' IDToName("444444444", conn = DBconn(tmpcoursedb)) 
IDToName <- function(ID, conn = DBconn()) {
      df <-as.data.frame(ID)
      dbGetPreparedQuery(conn, 
                         "SELECT givenNames, lastName FROM students AS s WHERE s.ID = :ID AND s.del = 0",
                         bind.data = df)
}

#' Show all relevant data on a student.
#' 
#' @param ID A student ID, the only necessary parameter.
#' @param date A date object; if altered from default, will only return information up until that date.
#' @param totalWeighting,cpWeighting,attendanceMethod,questionMethod See \code{\link{IDToCurrentGrade}}
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @return A list with outputs from IDToName(), AssignmentMarks(), TestMarks(), IDToNotes(), and (if testWeighting is given) IDToCurrentGrade().
#' @examples
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' showStudent("111111111", conn = DBconn(tmpcoursedb))
showStudent <- function(ID, date = Sys.Date(), totalWeighting = NULL, cpWeighting = c(0.5, 0.5), attendanceMethod = "toDate", questionMethod = c("answer", "fraction"), conn = DBconn()) {
      Name <- IDToName(ID, conn)
      Name <- paste(Name[1], Name[2])
      Assignments <- AssignmentMarks(ID, date, conn)
      Tests <- TestMarks(ID, date, conn)
      Notes <- IDToNotes(ID, conn)
      result <- list(Name = Name, Assignments = Assignments, Tests = Tests, Notes = Notes)
      if (is.null(totalWeighting) == FALSE) {
            Current.Grade <- IDToCurrentGrade(ID, totalWeighting, date, cpWeighting, attendanceMethod, questionMethod, conn)
            result <- list(Name = Name, Assignments = Assignments, Tests = Tests, Current.Grade = Current.Grade, Notes = Notes)
      }
      class(result) <- c("studentInfo", class(result))
      return(result)
}

#' Display a student's results for a given test.
#' 
#' @param ID A character vector, the student's ID.
#' @param examName A character vector identifying the exam.
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#' 
#' ID <- "111111111"; examName <- "1"
#' IDAndExamNameToResults(ID, examName, conn = DBconn(tmpcoursedb))
IDAndExamNameToResults <- function(ID, examName, conn = DBconn()) {
      # 1         match ID to answers (mcAnswers) and grades (longformGrades)
      df <- as.data.frame(cbind(ID, examName))
      a <- dbGetPreparedQuery(conn, "SELECT answer, questionName, questionValue, examCode FROM mcAnswers AS m WHERE m.ID = :ID AND m.examName = :examName AND m.del = 0", bind.data = df)
      if (length(unique(a$examCode)) > 1) {warning("Exam code not unique. (mcAnswers table)")}      
      if (nrow(a) == 0) {stop("No records for this student ID found in the mcAnswers table.")}
      g <- dbGetPreparedQuery(conn, "SELECT grade, questionName, examCode FROM longformGrades AS l WHERE l.ID = :ID AND l.examName = :examName AND l.del = 0", bind.data = df)
      if (length(unique(g$examCode)) > 1) {warning("Exam code not unique. (longformGrades table)")}
      if (nrow(a) == 0) {stop("No records for this student ID found in the longformGrades table.")}
      # 2     compare answers to correct answers, grades to total grades
      df <- as.data.frame(cbind(examName, a$examCode[1])); colnames(df) <- c("examName", "examCode")
      ca <- dbGetPreparedQuery(conn, "SELECT answer, questionName, questionValue FROM mcAnswers AS m WHERE m.ID = 999999999 AND m.examName = :examName AND m.examCode = :examCode AND m.del = 0", bind.data = df)
      tg <- dbGetPreparedQuery(conn, "SELECT grade, questionName FROM longformGrades AS l WHERE l.ID = 999999999 AND l.examName = :examName AND l.examCode = :examCode AND l.del = 0", bind.data = df)
      # 3     reformat to display desired information.
      a$answerKey <- ca$answer
      a$correct <- FALSE
      a$correct[a$answer == ca$answer] <- TRUE
      a <- a[ , c(2, 3, 6, 1, 5, 4)]
      g$maxGrade <- tg$grade
      g <- g[ , c(2, 1, 4, 3)]
      # 4     tally total mark on test
      total <- (sum(as.integer(a$questionValue[a$questionName == ca$questionName & 
                                                     a$answer == ca$answer])) 
                + sum(as.integer(g$grade)))
      outOf <- sum(as.integer(ca$questionValue)) + sum(as.integer(tg$grade))
      fraction <- total / outOf
      #5 give result
      result <- list(mcAnswers = a, longformGrade = g, tally = c(total = total, outOf = outOf, fraction = fraction))
      class(result) <- c("testInfo", class(result))
      return(result)
}

#' Display the results of a test for an entire class.
#' 
#' If either the mcAnswers table or the longformGrades table contains rows 
#' for which an answer key does not exist (a row with ID == "999999999" 
#' where the correct answers are given) then the calculations resulting in 
#' class averages will omit those rows.
#' @param examName A string, the name of the test (or exam).
#' @param summaryOnly A logical variable.  If TRUE, will only display a summary of the class's data.  
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#'
#' examNameToResults("1", conn = DBconn(tmpcoursedb))
#' examNameToResults("2", summary = FALSE, conn = DBconn(tmpcoursedb))
examNameToResults <- function(examName, summaryOnly = TRUE, conn = DBconn()) {
      # Retrieve data from tables
      df <- data.frame(examName = as.character(examName))
      a <- dbGetPreparedQuery(conn, "SELECT ID, answer, questionName, questionValue, examCode FROM mcAnswers AS m WHERE m.examName = :examName AND m.del = 0", bind.data = df)
      g <- dbGetPreparedQuery(conn, "SELECT ID, grade, questionName, examCode FROM longformGrades AS l WHERE l.examName = :examName AND l.del = 0", bind.data = df)
      # Separate answer keys/ full marks
      ca <- a[a$ID == "999999999", ]; a <- a[a$ID != "999999999", ]
      tg <- g[g$ID == "999999999", ]; g <- g[g$ID != "999999999", ]
      # Tack correct answer/ full marks into each row.
      for (i in 1:nrow(a)) { 
            if (length(ca$answer[ca$examCode == a$examCode[i] & ca$questionName == a$questionName[i]]) == 1) {
                  a$answerKey[i] <- ca$answer[ca$examCode == a$examCode[i] & ca$questionName == a$questionName[i]]
            } else {
                  warning("In the mcAnswers table, there is a row with an examCode or questionName that does not correspond to any answer key (row with ID == \"999999999\").")
                  a$answerKey[i] <- NA
            }
      }
      for (i in 1:nrow(g)) {
            if (sum(tg$examCode == g$examCode[i] & tg$questionName == g$questionName[i]) == 0) {
                  warning("In the longformGrades table, there is a row with an examCode or questionName that does not correspond to any answer key (row with ID == \"999999999\").")
                  g$maxGrade[i] <- NA
            } else {
                  g$maxGrade[i] <- tg$grade[tg$examCode == g$examCode[i] & tg$questionName == g$questionName[i]]
            }
      }
      # Tack on question summaries
      a$correct <- a$answer == a$answerKey
      g$fraction <- g$grade / g$maxGrade
      # Reorder columns
      a <- a[ , c(1, 3, 7, 4, 2, 6, 5)]
      g <- g[ , c(1, 3, 6, 2, 5, 4)]
      # Tally marks 
      ids <- unique(a$ID); total <- rep(NA, length(ids))
      for (i in 1:length(ids)) {
            total[i] <- (sum(as.integer(a$questionValue[a$ID == ids[i] & a$answer == a$answerKey])) + sum(as.integer(g$grade[g$ID == ids[i]])))
      }
      outOf <- sum(as.integer(ca$questionValue[ca$examCode == ca$examCode[1]])) + sum(as.integer(tg$grade)[tg$examCode == tg$examCode[1]])
      fraction <- total / outOf
      names(fraction) <- ids
      if (summaryOnly == TRUE) {
            # Only for summaries
            a <- data.frame(ID = a$ID, questionName = a$questionName, mc = a$correct, stringsAsFactors = FALSE)
            g <- data.frame(ID = g$ID, questionName = g$questionName, lf = g$fraction, stringsAsFactors = FALSE) 
            a <- reshape(a, idvar = "ID", timevar = "questionName", direction = "wide")
            g <- reshape(g, idvar = "ID", timevar = "questionName", direction = "wide")
            sum <- merge(a, g)
            sum$fraction <- fraction[names(fraction) == sum$ID]
            sum.minus.ID <- sum[names(sum) != "ID"]
            last.row <- as.data.frame(t(apply(sum.minus.ID, 2, mean))); last.row$ID <- "class"
            sum <- rbind(sum, last.row)
            return(sum)
      } else {
            # Format for printing
            tally <- data.frame(total = total); tally$outOf <- outOf; tally$fraction <- fraction
            last.tally <- as.data.frame(t(apply(tally, 2, mean, na.rm = TRUE))); last.tally$ID <- "class"
            tally$ID <- ids; tally <- rbind(tally, last.tally); tally <- tally[ , c(4, 1, 2, 3)]
            result <- list(mcAnswers = a, longformGrade = g, tally = tally)
            class(result) <- c("testInfo", class(result))
            return(result)      
      }
}

#' Displays test results
#' @param examName A string, the name of the test or exam to display.
#' @param ID The name of a specific student to focus upon; if FALSE, displays slightly less detailed information for entire class.
#' @param summaryOnly A logical variable, if TRUE, displays only a relatively brief summary.
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @examples 
#' td <- tempdir() # Create temporary directory for sample database.
#' tmpcoursedb <- paste(td, "course.db", sep = "\\") # Record location of sample database.
#' if (!file.exists(tmpcoursedb)) createDB(sample = TRUE, conn = DBconn(tmpcoursedb)) # Create sample database.
#'
#' showTest("1", conn = DBconn(tmpcoursedb))
#' showTest("1", ID = "111111111", conn = DBconn(tmpcoursedb))
#' showTest("2", summary = TRUE, conn = DBconn(tmpcoursedb))
showTest <- function(examName, ID = FALSE, summaryOnly = FALSE, conn = DBconn()) {
      if (ID == FALSE) {
            examNameToResults(examName, summaryOnly, conn)
      } else {
            IDAndExamNameToResults(ID, examName, conn)
      }
}