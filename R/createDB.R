# Roxygen help file commentary

#' Create initial database.
#' 
#' Create initial database; only run once.  
#' Afterwards, use other functions to update the database.
#' 
#' @param conn An SQL connection to a database file.  @seealso \code{\link{DBconn}}
#' 
#' @examples
#' createDB()
#' 
#' @return createDB does not return a value.



createDB <- function(overwrite = FALSE, conn = DBconn()) {
  students <- read.csv(system.file("csv/students.csv", package="coursedb"))
  students$ID <- as.character(students$ID)
  dbWriteTable(conn, "students", students, overwrite = overwrite, row.names = FALSE)
  
  assignments <- read.csv(system.file("csv/assignments.csv", package="coursedb"))
  assignments$date <- as.Date(assignments$date)
  if (all(!is.na(as.Date(as.character(assignments$date)))) == FALSE)
        warning("Not all assignments dates read as valid dates.")
  dbWriteTable(conn, "assignments", assignments, overwrite = overwrite, row.names = FALSE)
  
  mcAnswers <- read.csv(system.file("csv/mcAnswers.csv", package = "coursedb"))
  mcAnswers$date <- as.Date(mcAnswers$date)
  if (all(!is.na(as.Date(as.character(mcAnswers$date)))) == FALSE)
        warning("Not all mcAnswers dates read as valid dates.")
  dbWriteTable(conn, "mcAnswers", mcAnswers, overwrite = overwrite, row.names = FALSE)
  
  longformGrades <- read.csv(system.file("csv/longformGrades.csv", package = "coursedb"))
  longformGrades$date <- as.Date(longformGrades$date)
  if (all(!is.na(as.Date(as.character(longformGrades$date)))) == FALSE)
        warning("Not all longformGrades dates read as valid dates.")
  dbWriteTable(conn, "longformGrades", longformGrades, overwrite = overwrite, row.names = FALSE)
  
  classParticipation <- read.csv(system.file("csv/classParticipation.csv", package = "coursedb"))
  classParticipation$date <- as.Date(classParticipation$date)
  if (all(!is.na(as.Date(as.character(classParticipation$date)))) == FALSE)
        warning("Not all classParticipation dates read as valid dates.")
  dbWriteTable(conn, "classParticipation", classParticipation, overwrite = overwrite, row.names = FALSE)
}

# ConvertToDates <- function(data.frame(table)) {
#       table$date <- as.Date(table$date)
#       if (all(!is.na(as.Date(as.character(table$date)))) == FALSE) {
# ###            warning("Not all", table, " dates read as valid dates.")
#       }
# }