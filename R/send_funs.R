#' Send a SQL Statement to Postgres
#' @param conn Connection object
#' @param sql_statement SQL to send
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbSendStatement function
#' @import DatabaseConnector
#' @export

send <-
    function(conn,
             sql_statement,
             ...) {

        DatabaseConnector::dbSendStatement(conn = conn,
                                           statement = sql_statement,
                                           ...)

    }





#' Send a List of SQL Statements
#' @description
#' Iteratively send a list of SQL Statements such as the object returned by \code{\link{parseSQL}}.
#' @import progress
#' @param conn Connection object
#' @param sqlList list object of queries
#' @param verbose If TRUE, the SQL is printed back before executing
#' @param progressBar If TRUE, a progress bar is returned in the console.
#' @param skipErrors If TRUE, if a SQL execution returns an error, the statement is printed back in red in the console and the iteration will proceed to the next sql statement in line.
#' @param errorFile (optional) path to the error file where any errors are written to if not null.
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbGetQuery function
#' @export

sendList <-
    function(conn,
             sqlList,
             verbose = TRUE,
             progressBar = TRUE,
             skipErrors = TRUE,
             errorFile = NULL,
             ...) {

            if (!is.list(sqlList)) {

                    stop("'sqlList' must be a list")

            }

            if (progressBar) {

                    pb <- progress::progress_bar$new(total = length(sqlList),
                                                     format = "[:bar] :elapsedfull :current/:total (:percent)",
                                                     clear = FALSE)
                    pb$tick(0)
                    Sys.sleep(.2)

            }

            for (i in 1:length(sqlList)) {

                    sql <- sqlList[[i]]

                    if (verbose) {
                            secretary::typewrite("\n", sql)
                    }

                    if (progressBar) {
                            pb$tick()
                            Sys.sleep(.2)
                    }


                    if (skipErrors) {

                            tryCatch(send(conn = conn,
                                        sql_statement = sql,
                                        ...),
                                     error = function(err) {
                                             secretary::typewrite_error("\n", sql)
                                             if (!is.null(errorFile)) {
                                                     .writeErrorReport(message = err$message,
                                                                       sql = sql,
                                                                       errorFile = errorFile)
                                             }
                                     })


                    } else {

                            send(conn = conn,
                                 sql_statement = sql,
                                 ...)
                    }

            }
    }





