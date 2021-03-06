#' Method to print class studentInfo objects (from the showStudent() function)
#' 
#' @param x A studentInfo object.
#' @param ... Further arguments passed to or from other methods.

print.studentInfo <- function(x, ...) {
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

#' Method to print class testInfo objects (from the showTest() function)
#' 
#' @param x A testInfo object.
#' @param ... Further arguments passed to or from other methods.
print.testInfo <- function(x, ...) {
      cat("Multiple choice: \n")
      print(x[[1]]); cat("\n")
      cat("Long form: \n")
      print(x[[2]]); cat("\n")
      cat("Tally: \n")
      print(x[[3]]); cat("\n")
}