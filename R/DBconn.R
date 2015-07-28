#' Create a connecdtion to SQL database "course.db".  
#' 
#' Opens up and returns a connection to the file "course.db", an SQL database.
#' Used by \code{\link{createDB}}.  
#' @param dbfile The name of the file you'd like to use as your database.

DBconn <- local({
  conn <- NULL
  function(dbfile = "course.db") {
    if (!missing(dbfile) && !is.null(conn)) {
      if (dbIsValid(conn))
        dbDisconnect(conn)
	conn <<- NULL
    }
    if (is.null(conn) || !dbIsValid(conn)) {
      sqlite <- SQLite()
      conn <<- dbConnect(sqlite, dbfile)
    }
    conn
  }
})