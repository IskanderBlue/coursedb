#' Ensure that correctly formatted dates are stored as numeric values.
#' 
#' @family helper functions
#' @param x A date -- converted into a date class object, then converted into a numeric value.  
#' @examples numDate(Sys.Date())
#' numDate("2015-07-29")
numDate <- function(x) as.numeric(as.Date(x))

#' Returns a numeric value if as.numeric() can successfully coerce it.
#' 
#' Not expected to be accessed directly by users.
#' @family helper functions
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

#' A helper function for the all of the Updater-() functions.
#' 
#' Not expected to be accessed directly by users.
#' Used by the \code{\link{data entry functions}}.
#' 
#' @family helper functions
#' @param df A data.frame to be used to update the table.
#' @param sql1 A string, the initial sql statement to find rows matching the criteria for an existing row.
#' @param ifsql A string, the sql statement to be used if there are no such matching rows 
#'    (tells Updater-() to add new rows to table).
#' @param elsesql A string, the sql statement to be used if no rows match; 
#'    (tells Updater-() to update matching rows).

rowUpdater <- function(df, sql1, ifsql, elsesql) {
      conn <- DBconn()
      query <- dbGetPreparedQuery(conn, sql1, bind.data = df)      
      if (nrow(query) == 0) {
            sql <- ifsql
      } else {
            sql <- elsesql
      }
      dbGetPreparedQuery(conn, statement = sql, bind.data = df)      
}

#' Edit tables directly.
#' 
#' Not used in package; used to develop package.
#' Can mess things up; not really for user use.  
#' @param table A database table to be edited.
#' @param conn An SQL connection.
DBedit <- function(table, conn = DBconn()) {
      tab <- dbReadTable(conn, table)
      tab <- edit(tab)
      dbWriteTable(conn, table, tab, overwrite=TRUE, row.names = FALSE)
}
