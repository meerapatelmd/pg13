#' Send Query to any Postgres connection
#' @param conn Connection object
#' @param sql_statement SQL Query
#' @param warn_no_rows If TRUE, a warning is given that query has returned 0 rows.
#' @param ... Additional arguments to pass onto \code{\link[DatabaseConnector]{dbGetQuery}}
#' @import DatabaseConnector
#' @export

query <-
    function(conn,
             sql_statement,
             verbose = TRUE,
             render_sql = TRUE,
             warn_no_rows = FALSE,
             ...) {

            brake_closed_conn(conn = conn)

            if (warn_no_rows) {

                on.exit(flag_no_rows(data = resultset), add = TRUE, after = TRUE)

            }

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
#' @rdname execute_n
#'
#' @export

execute_n <-
    function(conn,
             sql_statements,
             verbose = TRUE,
             render_sql = TRUE,
             profile = FALSE,
             progressBar = TRUE,
             reportOverallTime = TRUE,
             errorReportFile = file.path(getwd(), "errorReportSql.txt"),
             runAsBatch = FALSE) {

            if (is.list(sql_statements)) {

                    sql_statements <- unlist(sql_statements)

            }

            sql_statement <- paste(sql_statements, collapse = ";\n")

            if (render_sql) {

                    typewrite_sql(sql_statement = sql_statement)
                    cat("\n\n")

            }

            if (verbose) {

                    typewrite_activity("Executing...")

            }

            DatabaseConnector::executeSql(connection = conn,
                                          sql = sql_statement,
                                          profile = profile,
                                          progressBar = progressBar,
                                          reportOverallTime = reportOverallTime,
                                          errorReportFile = errorReportFile,
                                          runAsBatch = runAsBatch)
            cat("\n\n")
    }





