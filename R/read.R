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
#'    \item{readLongFormGrades()}{Returns the contents of the "longFormGrades" table.}
#'    \item{classParticipation()}{Returns the contents of the "classParticipation" table.}
#'    \item{readAllInfo()}{Returns the contents of all tables.}
#' }
#' @seealso \code{\link{createDB}}
#' 
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' @param table The name of a table to be shown.  Enter in quotations ("table").
#' 
#' @name basicOutputs
NULL
#> NULL

#' @rdname basicOutputs
listTables <- function(conn = DBconn()) {
      dbListTables(conn)
}

#' @rdname basicOutputs
showTable <- function(table, conn = DBconn()) {
      dbGetQuery(conn, paste("select * from", table))
}

#' @rdname basicOutputs
readStudents <- function(conn = DBconn()) {
      dbGetQuery(conn, "select * from students")
}

#' @rdname basicOutputs
readAssignments <- function(conn = DBconn()) {
      dbGetQuery(conn, "select * from assignments")
}

#' @rdname basicOutputs
readMCAnswers <- function(conn = DBconn()) {
      dbGetQuery(conn, "select * from mcAnswers")
}

#' @rdname basicOutputs
readLongformGrades <- function(conn = DBconn()) {
      dbGetQuery(conn, "select * from longformGrades")
}

#' @rdname basicOutputs
readClassParticipation <- function(conn = DBconn()) {
      dbGetQuery(conn, "select * from classParticipation")
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

## Name variants!!! LIKE in SQL?
## phonetic matching somehow?

#' Get student's ID number from their name.  
#' 
#' Enter a student's name -- given names in the first string, last name in the second.  
#' NameToID() will return a dataframe with the IDs and names of all students 
#' with names similar (according to \link{agrep()}) to the strings the user 
#' gives.  
#' 
#' @param givenNames A string, a student's first name (and any middle names used).  
#' @param lastName A string,  that student's last name.
#' 
#' @return A dataframe containing the IDs, given names, and last names of all matches.

NameToID <- function(givenNames = "deedoublename", lastName = "cee") {
      IDs <- dbGetQuery(conn = DBconn(), 
                        "SELECT ID, lastName, givenNames 
                               FROM students")
      givenMatch <- agrep(givenNames, IDs$givenNames)
      lastMatch <- agrep(lastName, IDs$lastName)
      matchingIDs <- IDs[intersect(givenMatch, lastMatch),]
      if (nrow(matchingIDs) == 0) {warning("No matching IDs.")}
      if (nrow(matchingIDs) > 1) {warning("More than one ID.")}
      return(matchingIDs)
}

#' Get a given student's grade on a given exam.  
#' 
#' Enter a student's ID and the name (or 'examNumber') of an exam.  
#' Returns their score, the score on a perfectly completed exam, and their 
#' score as a fraction.
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param examNumber A string, the number (or name) assigned to an exam.
#' @return A vector with 3 elements.  
#' \enumerate{
#'    \item The student's score.
#'    \item The full potential score.
#'    \item The student's score as a fraction of the full score.  
#' }
#' @seealso \code{\link{IDToAssignmentMark}}

IDAndExamNumberToGrade <- function(ID = 111111111, examNumber = "2") {
      # 1         match ID to answers (mcAnswers) and grades (longformGrades)
      df <- as.data.frame(cbind(ID, examNumber))
      a <- dbGetPreparedQuery(conn = DBconn(),
                              "SELECT answer, questionNumber, questionValue, examCode 
                                    FROM mcAnswers AS m 
                                    WHERE m.ID = :ID 
                                    AND m.examNumber = :examNumber",
                              bind.data = df)
      if (length(unique(a$examCode)) > 1) {warning("Exam code not unique.")}      
      g <- dbGetPreparedQuery(conn = DBconn(),
                              "SELECT grade, questionNumber, examCode 
                                    FROM longformGrades AS l 
                                    WHERE l.ID = :ID 
                                    AND l.examNumber = :examNumber",
                              bind.data = df)
      if (length(unique(g$examCode)) > 1) {warning("Exam code not unique.")}
      # 2     compare answers to correct answers, grades to total grades
      df <- as.data.frame(cbind(examNumber, a$examCode[1])); colnames(df) <- c("examNumber", "examCode")
      ca <- dbGetPreparedQuery(conn = DBconn(),
                               "SELECT answer, questionNumber, questionValue 
                                    FROM mcAnswers AS m 
                                    WHERE m.ID = 999999999 
                                    AND m.examNumber = :examNumber 
                                    AND m.examCode = :examCode",
                               bind.data = df)
      tg <- dbGetPreparedQuery(conn = DBconn(),
                               "SELECT grade, questionNumber 
                                    FROM longformGrades AS l 
                                    WHERE l.ID = 999999999 
                                    AND l.examNumber = :examNumber 
                                    AND l.examCode = :examCode",
                               bind.data = df)
      # 3     tally total mark on test
      total <- (sum(as.integer(a$questionValue[a$questionNumber == ca$questionNumber 
                                              && a$answer == ca$answer])) 
               + sum(as.integer(g$grade)))
      outOf <- sum(as.integer(ca$questionValue)) + sum(as.integer(tg$grade))
      fraction <- total / outOf
      return(c(total = total, outOf = outOf, fraction = fraction))
}


#' Get a given student's mark on a given assignment.    
#' 
#' Enter a student's ID and the name (or 'examNumber') of an assignment.  
#' Returns their score, the score on a perfectly completed assignment, and their 
#' score as a fraction.
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param assignmentNumber A string, the number (or name) of an assignment.
#' @return A vector with 3 elements.  
#' \enumerate{
#'    \item The student's score.
#'    \item The full potential score.
#'    \item The student's score as a fraction of the full score.  
#' }
#' @seealso \code{\link{IDAndExamNumberToGrade}}
IDToAssignmentMark <- function(ID = 111111111, assignmentNumber = "1") {
      # 1   Match ID and assignmentNumber to score (from assignments table)
      #     and get full score for assignment
      df <- as.data.frame(cbind(ID, assignmentNumber))
      a <- dbGetPreparedQuery(conn = DBconn(),
                              "SELECT grade, ID FROM assignments AS a 
                                    WHERE a.ID = :ID 
                                    AND a.assignmentNumber = :assignmentNumber
                                    OR a.ID = 999999999
                                    AND a.assignmentNumber = :assignmentNumber",
                              bind.data = df)
      a
      if (length(unique(a$assignmentNumber)) > 2) {stop("Assignment number not unique.")}
      # 2   Return vector: mark, fraction
      return(c(mark = a[1,1], outOf = a[2,1], fraction = a[1,1]/a[2,1]))
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
#' @return A numeric value.  
#' @seealso \code{\link{IDToClassParticipation}}
IDToAttendance <- function(ID = 111111111, date = Sys.Date(), attendanceMethod = "toDate") {
      # Attendance weighting default: % of dates so far attended; otherwise x/y(specified), capped at 1.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn = DBconn(), 
                              "SELECT date, attended 
                              FROM classParticipation AS c
                              WHERE c.ID = :ID",
                              bind.data = ID)
      attendedSoFar <- sum(unique(c$attended[c$date <= date])) # Number of dates attended to date
      if (is.numeric(attendanceMethod)) {
            potentialDates <- attendanceMethod
      } else {
            if (attendanceMethod != "toDate") {
                  warning("Invald attendanceMethod value; defaulting to 'toDate'.")
            } 
            potentialDates <- length(unique(c$date[c$date <= date]))
      }
      attendanceGrade <- min(attendedSoFar/potentialDates, 1)
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
#'          \item A string, "percent" -- determine the student's grade as a 
#'                fraction with the number of questions the student has asked 
#'                as the numerator and the of the maximum number of questions 
#'                asked by a single student in the table as the denominator.
#'          \item A numeric value --  if the student has asked at least that 
#'                number of questions, they get full marks; otherwise, none.  
#'    }
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
IDToQuestionsAsked <- function(ID = 111111111, date = Sys.Date(), evalMethod = "percent") {
      #     Default evaluation method:
      #     Find the maximum number of questions asked.  
      #     Find the percentage of that maximum that the given ID asked. 
      #     Alternate: 100% if >= evalMethod (a number) questions were asked.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn = DBconn(), 
                              "SELECT date, questionAsked
                              FROM classParticipation AS c
                              WHERE c.ID = :ID",
                              bind.data = ID)
      qs <- sum(c$questionAsked[c$date <= date] != "")
      if (is.numeric(evalMethod)) {
            qGrade <- c(questionsAsked = qs, mark = 0, threshold = evalMethod)
            if (qs >= evalMethod) {qGrade[2] <- 1} 
      } else {
            if (evalMethod != "percent") {warning("Invalid evalMethod; defaulting to 'percent'.")}
            allQs <- dbGetQuery(conn = DBconn(),
                                "SELECT ID, questionAsked 
                                FROM classParticipation as c
                                ORDER BY c.ID")
            potentialQs <- qs
            for (i in unique(allQs$ID)) {
                  currentID <- sum(allQs$questionAsked[allQs$ID == i] != "")
                  if (currentID > potentialQs) {potentialQs <- currentID}
            }
            qGrade <- c(questionsAsked = qs, percentage = qs/potentialQs)
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
#'          \item A string, "percent" -- determine the student's grade as a 
#'                fraction with the number of questions the student has answered 
#'                as the numerator and the of the maximum number of questions 
#'                answered by a single student in the table as the denominator.
#'          \item A numeric value --  if the student has answered at least that 
#'                number of questions, they get full marks; otherwise, none.  
#'    }
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
IDToQuestionsAnswered <- function(ID = 111111111, date = Sys.Date(), evalMethod = "percent") {
      #     Default evaluation method:
      #     Find the maximum number of questions answered.  
      #     Find the percentage of that maximum that the given ID answered.
      #     Alternate: 100% if >= evalMethod (a number) questions were answered.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn = DBconn(), 
                              "SELECT date, questionAnswered
                              FROM classParticipation AS c
                              WHERE c.ID = :ID",
                              bind.data = ID)
      qs <- sum(c$questionAnswered[c$date <= date] != "")
      if (is.numeric(evalMethod)) {
            qGrade <- c(questionsAnswered = qs, mark = 0, threshold = evalMethod)
            if (qs >= evalMethod) {qGrade[2] <- 1} 
      } else {
            if (evalMethod != "percent") {warning("Invalid evalMethod; defaulting to 'percent'.")}
            allQs <- dbGetQuery(conn = DBconn(),
                                "SELECT ID, questionAnswered 
                                FROM classParticipation as c
                                ORDER BY c.ID")
            potentialQs <- qs
            for (i in unique(allQs$ID)) {
                  currentID <- sum(allQs$questionAnswered[allQs$ID == i] != "")
                  if (currentID > potentialQs) {potentialQs <- currentID}
            }
            qGrade <- c(questionsAnswered = qs, percentage = qs/potentialQs)
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
#'          \item A string, "percent" -- determine the student's grade as a 
#'                fraction with the number of questions the student has asked 
#'                and answered as the numerator and the of the maximum number 
#'                of questions asked and answered by a single student in the 
#'                table as the denominator.
#'          \item A numeric value --  if the student has asked and/ or 
#'                answered at least that number of questions, they get full 
#'                marks; otherwise, none.  
#'    }
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
IDToCombinedQuestions <- function(ID = 111111111, date = Sys.Date(), evalMethod = "percent") {
      #     Default evaluation method:
      #     Find the maximum number of questions asked and answered.  
      #     Find the percentage of that maximum that the given ID asked and answered.
      #     Alternate: 100% if >= evalMethod (a number) questions were asked and/ or answered.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn = DBconn(), 
                              "SELECT date, questionAnswered, questionAsked
                              FROM classParticipation AS c
                              WHERE c.ID = :ID",
                              bind.data = ID)
      qs <- sum(c$questionAsked[c$date <= date] != "") + sum(c$questionAnswered[c$date <= date] != "")
      if (is.numeric(evalMethod)) {
            qGrade <- c(questions = qs, mark = 0, threshold = evalMethod)
            if (qs >= evalMethod) {qGrade[2] <- 1} 
      } else {
            if (evalMethod != "percent") {warning("Invalid evalMethod; defaulting to 'percent'.")}
            allQs <- dbGetQuery(conn = DBconn(),
                                "SELECT ID, questionAnswered, questionAsked
                                FROM classParticipation as c
                                ORDER BY c.ID")
            potentialQs <- qs
            for (i in unique(allQs$ID)) {
                  currentID <- sum(allQs$questionAsked[allQs$ID == i] != "") + sum(allQs$questionAnswered[allQs$ID == i] != "")
                  if (currentID > potentialQs) {potentialQs <- currentID}
            }
            qGrade <- c(questions = qs, percentage = qs/potentialQs)
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
#'    } The mark returned will be incorrect if the cpWeighting vector does not 
#'    sum to 1.  
#' @param attendanceMethod Either a string, "toDate", or a numeric value; 
#'    see \code{\link{IDToAttendance}}.  
#' @param questionMethod A 2-element vector; \enumerate{
#'    \item A string -- either "ask", "answer", or "both", depending on 
#'          which function you want to use to evaluate questions for the 
#'          class participation grade.  
#'    \item Either a string, "percent", or a numeric value.  See the Arguments 
#'          in \code{\link{IDToQuestionsAsked}}, for an explanation.  
#'    }
#' @return A numeric value, the fraction of a full grade the student receives.  

#' @seealso \code{\link{IDToAttendance}}
#' @seealso \code{\link{IDToQuestionsAsked}}
#' @seealso \code{\link{IDToQuestionsAnswered}}
#' @seealso \code{\link{IDToCombinedQuestions}}
IDToClassParticipation <- function(ID = 111111111, date = Sys.Date(), cpWeighting = c(0.5,0.5), attendanceMethod = "toDate", questionMethod = c("answer", "percent")) {
      # Weighting default: .5 attendance, .5 questions
      if (is.numeric(cpWeighting) != TRUE) {
            warning("IDToClassParticipation requires 'cpWeighting' to be a vector of two numbers; defaulting to c(0.5, 0.5).")
            cpWeighting <- c(0.5, 0.5)
      }
      a <- IDToAttendance(ID, date, attendanceMethod)
      if (questionMethod[1] == "ask") {
            q <- IDToQuestionsAsked(ID, date, questionMethod[2])
      } else if (questionMethod[1] == "both") {
            q <- IDToCombinedQuestions(ID, date, questionMethod[2])
      } else {
            if (questionMethod[1] != "answer") {
                  warning("questionMethod[1] is invalid; defaulting to 'answer'.")
            }
            q <- IDToQuestionsAnswered(ID, date, questionMethod[2])
      }
      
      return(a * cpWeighting[1] + q[2] * cpWeighting[2])
}


#' Get a student's marks on assignments.    
#' 
#' Enter a student's ID and date.  Function uses \code{\link{IDToAssignmentMark}}.  
#' Returns a matrix of a student's marks on assignments up to that date.  
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @return A matrix with 3 columns:  
#' \enumerate{
#'    \item The student's marks on assignments.
#'    \item The full potential marks.
#'    \item The student's marks as fractions of the full marks.  
#' }
#' @seealso \code{\link{IDToAssignmentMark}}
#' @seealso \code{\link{TestMarks}}
AssignmentMarks <- function(ID = 111111111, date = Sys.Date()) {
      a <- readAssignments()
      uniqueA <- unique(a$assignmentNumber[a$date <= date])
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
#' Enter a student's ID and date.  Function uses \code{\link{IDAndExamNumberToGrade}}.  
#' Returns a matrix of a student's marks on tests up to that date.  
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @param date A \code{\link{date}} class object.  @seealso \code{\link{date}}
#' @return A matrix with 3 columns:  
#' \enumerate{
#'    \item The student's marks on tests.
#'    \item The full potential marks.
#'    \item The student's marks as fractions of the full marks.  
#' }
#' @seealso \code{\link{IDAndExamNumberToGrade}}
#' @seealso \code{\link{AssignmentMarks}}
TestMarks <- function(ID = 111111111, date = Sys.Date()) {
      allMC <- readMCAnswers()
      allLF <- readLongformGrades()
      uniqueT <- unique(c(unique(allMC$examNumber[allMC$date <= date])), unique(allLF$examNumber[allLF$date <= date]))
      tMarks <- matrix(ncol = 3, nrow = length(uniqueT))
      colnames(tMarks) = c("mark", "outOf", "fraction")
      rownames(tMarks) = uniqueT
      for (i in uniqueT) {
            tMarks[i, ] <- IDAndExamNumberToGrade(ID, i)
      }
      for (i in nrow(tMarks):1) {
            if (is.na(tMarks[i,2]) || tMarks[i,2] == 0) {
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
#'    } The mark returned will be incorrect if the cpWeighting vector does not 
#'    sum to 1.  
#' @param attendanceMethod Either a string, "toDate", or a numeric value; 
#'    see \code{\link{IDToAttendance}}.  
#' @param questionMethod A 2-element vector; \enumerate{
#'    \item A string -- either "ask", "answer", or "both", depending on 
#'          which function you want to use to evaluate questions for the 
#'          class participation grade.  
#'    \item Either a string, "percent", or a numeric value.  See the Arguments 
#'          in \code{\link{IDToQuestionsAsked}}, for an explanation.  
#'    }
#' @return A numeric value, the fraction of a full grade the student receives.  
#' @seealso \code{\link{IDToClassParticipation}}
#' @seealso \code{\link{AssignmentMarks}}
#' @seealso \code{\link{TestMarks}}

IDToCurrentGrade <- function(ID = 111111111, totalWeighting = c(0.2, 0, 0.3, 0.5), date = Sys.Date(), cpWeighting = c(0.5, 0.5), attendanceMethod = "toDate", questionMethod = c("answer", "percent")) {
      # 1   Determine weighting (eg. assignments = .2, exams = .5)
      #                              Default:   20% Assignments
      #                                          0% Participation
      #                                         30% Tests
      #                                         50% Final
      #           Should default be out of all marks to date = Sys.Date() ? 
      
      # 2   Fetch all the grades -- assignments, tests, participation, etc.
      # 2.1 Fetch assignments
      a <- AssignmentMarks(ID, date)
      
      # 2.2 Fetch tests
      t <- TestMarks(ID, date)
      
      # 2.3 Fetch class participation
      c <- IDToClassParticipation(ID, date, cpWeighting, attendanceMethod, questionMethod)
      
      # 3   Weight grades according to weighting function.
      aw <- sum(a[,1])/sum(a[,2]) * totalWeighting[1]
      cw <- c * totalWeighting[2]
      tw <- sum(t[-nrow(t), 1], na.rm = TRUE) / sum(t[-nrow(t), 2]) * totalWeighting[3] # Tests excluding final.
      ew <- t[nrow(t), 1]/ t[nrow(t), 2] * totalWeighting[4] # Final test/ exam.
      
      return(sum(aw, cw, tw, ew, na.rm = TRUE))
}

#' Retrieve all notes on a given student.
#' 
#' Enter the student's ID.  Returns a data.frame with any notes entered.  
#' 
#' @param ID A student's ID number (should be a 9 digit integer).
#' @return A data.frame with a single column.  
#' @seealso \code{\link{readStudents}}

# What are my notes on this student?
IDToNotes <- function(ID = 111111111) {
      df <- as.data.frame(ID)     
      dbGetPreparedQuery(conn = DBconn(), 
                         "SELECT notes FROM students AS s WHERE s.ID = :ID", 
                         bind.data = df)
}