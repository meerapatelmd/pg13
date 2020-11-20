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

            brake_closed_conn(conn = conn)
            on.exit(flag_no_rows(data = resultset))

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

                    typewrite_activity("Querying...complete")

            }

            resultset

    }





#' @title
#' Query with more than 1 SQL Statements
#'
#' @description
#' Query multiple SQL Statements in a single function call.
#'
#' @inheritParams DatabaseConnector::executeSql
#' @inheritParams base_args
#'
#' @importFrom DatabaseConnector executeSql
#'
#' @export

queries <-
    function(conn,
             sql_statements,
             render_sql = TRUE,
             profile = FALSE,
             progressBar = TRUE,
             reportOverallTime = TRUE,
             errorReportFile = file.path(getwd(), "errorReportSql.txt"),
             runAsBatch = FALSE) {

            sql_statement <- paste(sql_statements, collapse = ";\n\n\n")

            if (render_sql) {

                    typewrite_sql(sql_statement = sql_statement)
                    cat("\n\n\n")

            }


            DatabaseConnector::executeSql(connection = conn,
                                          sql = sql_statement,
                                          profile = profile,
                                          progressBar = progressBar,
                                          reportOverallTime = reportOverallTime,
                                          errorReportFile = errorReportFile,
                                          runAsBatch = runAsBatch)
    }





