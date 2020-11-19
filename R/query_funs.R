#' Send Query to any Postgres connection
#' @param conn Connection object
#' @param sql_statement SQL Query
#' @param ... Additional arguments to pass onto \code{\link[DatabaseConnector]{dbGetQuery}}
#' @import DatabaseConnector
#' @export

query <-
    function(conn,
             sql_statement,
             verbose = TRUE,
             render_sql = TRUE,
             ...) {

            if (render_sql) {

                    typewrite_sql(sql_statement = sql_statement)

            }

            if (verbose) {

                    typewrite_activity("Querying...")

            }

            resultset <- DatabaseConnector::dbGetQuery(conn,
                                              statement = sql_statement,
                                              ...)


            if (verbose) {

                    secretary::typewrite("Querying...complete")

            }

            resultset

    }





#' Query a List of SQL Statements
#' @description
#' Iteratively query over a list of SQL Statements such as the object returned by \code{\link{parseSQL}}.
#' @import progress
#' @param conn Connection object
#' @param sqlList list object of queries
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbGetQuery function
#' @export

queryList <-
    function(conn,
             sqlList,
             verbose = TRUE,
             progressBar = TRUE,
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

            output <- list()
            for (i in 1:length(sqlList)) {

                    sql <- sqlList[[i]]

                    if (verbose) {
                            secretary::typewrite(sql)
                    }

                    if (progressBar) {
                            pb$tick()
                            Sys.sleep(.2)
                    }

                    output[[i]] <-
                    query(conn = conn,
                          sql_statement = sql,
                          ...)

                    names(output)[i] <- sql

            }

            output
    }





