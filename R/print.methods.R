#' Method to print class studentInfo objects (from the showStudent() function)
#' 
#' @param x A studentInfo object.

print.studentInfo <- function(x) {
      cat("Name: \n")
      cat(x[[1]], "\n\n")
      cat("Assignments: \n")
      print(x[[2]]); cat("\n")
      cat("Tests: \n")
      print(x[[3]]); cat("\n")
      if (is.null(x$Current.Grade) == FALSE) {
            cat("Current grade: \n")
            cat(x[[4]], "\n\n")
            last <- 5
      } else {
            last <- 4
      }
      cat("Notes: \n")
      print(x[[last]])
}