#' Returns a numeric value if as.numeric() can successfully coerce it.
#' @param maybeANumber A variable of unknown type, possibly coercible to a numeric value.
checkNumeric <- function(maybeANumber) {
      # Avoiding warning when assigning as.numeric(maybeANumber) to num
      storeWarn <- getOption("warn")
      options(warn = -1)
      asNum <- as.numeric(maybeANumber)
      options(warn = storeWarn)
      # Testing 
      if (!is.na(asNum)) {
            return(as.numeric(maybeANumber))
      } else {
            return(maybeANumber)
      }
}      