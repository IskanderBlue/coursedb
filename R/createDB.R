#' Create initial database.
#' 
#' Create initial database; only run once.  
#' Afterwards, use other functions to update the database.
#' 
#' @param overwrite Defaults to FALSE; if TRUE, overwrites the existing course.db database.
#' @param sample Defaults to FALSE; if TRUE, generates a sample database.
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' 
#' @examples
#' createDB()
#' 
#' @return createDB does not return a value.

createDB <- function(overwrite = FALSE, sample = FALSE, conn = DBconn()) {
      if (sample == FALSE) {
            students <- data.frame(rowNumber = numeric(0), del = logical(0), ID = character(0), email = character(0), givenNames = character(0), lastName = character(0), program = character(0), notes = character(0))
            assignments <- data.frame(rowNumber = numeric(0), del = logical(0), ID = character(0), date = numeric(0), grade = numeric(0), assignmentName = character(0))
            mcAnswers <- data.frame(rowNumber = numeric(0), del = logical(0), ID = character(0), date = numeric(0), answer = character(0), questionName = character(0), questionValue = numeric(0), examName = character(0), examCode = character(0))
            longformGrades <- data.frame(rowNumber = numeric(0), del = logical(0), ID = character(0), date = numeric(0), grade = numeric(0), questionName = character(0), examName = character(0), examCode = character(0))
            classParticipation <- data.frame(rowNumber = numeric(0), del = logical(0), ID = character(0), date = numeric(0), attended = logical(0), questionAnswered = character(0), questionAsked = character(0), participationNotes = character(0))
      } else {
            students <- read.csv(system.file("csv/students.csv", package="coursedb"), stringsAsFactors = FALSE) 
            students$ID <- as.character(students$ID)
            assignments <- read.csv(system.file("csv/assignments.csv", package="coursedb"), stringsAsFactors = FALSE)
            assignments$ID <- as.character(assignments$ID)
            assignments$date <- as.Date(assignments$date)
            assignments$assignmentName <- as.character(assignments$assignmentName)
            mcAnswers <- read.csv(system.file("csv/mcAnswers.csv", package = "coursedb"), stringsAsFactors = FALSE)
            mcAnswers$ID <- as.character(mcAnswers$ID)
            mcAnswers$date <- as.Date(mcAnswers$date)
            mcAnswers$questionName <- as.character(mcAnswers$questionName)
            mcAnswers$examName <- as.character(mcAnswers$examName)
            longformGrades <- read.csv(system.file("csv/longformGrades.csv", package = "coursedb"), stringsAsFactors = FALSE)
            longformGrades$ID <- as.character(longformGrades$ID)
            longformGrades$date <- as.Date(longformGrades$date)
            longformGrades$grade <- as.numeric(longformGrades$grade)
            longformGrades$questionName <- as.character(longformGrades$questionName)
            longformGrades$examName <- as.character(longformGrades$examName)
            classParticipation <- read.csv(system.file("csv/classParticipation.csv", package = "coursedb"), stringsAsFactors = FALSE)
            classParticipation$ID <- as.character(classParticipation$ID)
            classParticipation$date <- as.Date(classParticipation$date)
      }
      dbWriteTable(conn, "students", students, overwrite = overwrite, row.names = FALSE)
      dbWriteTable(conn, "assignments", assignments, overwrite = overwrite, row.names = FALSE)
      dbWriteTable(conn, "mcAnswers", mcAnswers, overwrite = overwrite, row.names = FALSE)
      dbWriteTable(conn, "longformGrades", longformGrades, overwrite = overwrite, row.names = FALSE)
      dbWriteTable(conn, "classParticipation", classParticipation, overwrite = overwrite, row.names = FALSE)
}