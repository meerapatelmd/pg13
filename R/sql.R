#' Send Query to any Postgres connection
#' @param conn Connection object
#' @param sql_statement SQL Query
#' @param render_only If TRUE, `sql_statement` is printed to the console, but the query is not run. This is an option to QA SQL if a query is throwing errors.
#' @param warn_no_rows If TRUE, a warning is given that query has returned 0 rows.
#' @param ... Additional arguments to pass onto \code{\link[DatabaseConnector]{dbGetQuery}}
#' @import DatabaseConnector
#' @export

query <-
    function(conn,
             conn_fun,
             sql_statement,
             verbose = TRUE,
             render_sql = TRUE,
             warn_no_rows = TRUE,
             render_only = FALSE,
             ...) {


            if (render_only) {

                    typewrite_sql(sql_statement = sql_statement)
                    invisible(sql_statement)

            } else {


            if (!missing(conn_fun)) {

                    conn <- eval(rlang::parse_expr(conn_fun))
                    on.exit(dc(conn = conn))

            }

            # +++
            # Checks
            # +++

            check_conn(conn = conn)


            if (render_sql) {

                    typewrite_sql(sql_statement = sql_statement)

            }


                if (warn_no_rows) {

                        on.exit(flag_no_rows(data = resultset),
                                add = TRUE,
                                after = TRUE)

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

    }





#' @title
#' Query with more than 1 SQL Statements
#'
#' @description
#' Query multiple SQL Statements in a single function call.
#'
#' @inheritParams DatabaseConnector::executeSql
#' @inheritParams args
#' @inheritParams query
#'
#' @importFrom DatabaseConnector executeSql
#'
#' @rdname execute_n
#'
#' @export

execute_n <-
    function(conn,
             conn_fun,
             sql_statements,
             verbose = TRUE,
             render_sql = TRUE,
             render_only = FALSE,
             profile = FALSE,
             progressBar = TRUE,
             reportOverallTime = TRUE,
             errorReportFile = file.path(getwd(), "errorReportSql.txt"),
             runAsBatch = FALSE) {

            if (render_only) {

                    sapply(sql_statements,
                           FUN = typewrite_sql)
                invisible(sql_statements)


            } else {

                    if (!missing(conn_fun)) {

                            conn <- eval(rlang::parse_expr(conn_fun))
                            on.exit(dc(conn = conn))

                    }

                    # +++
                    # Checks
                    # +++


                    check_conn(conn = conn)

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
    }



#' Send a SQL Statement to Postgres
#' @param conn Connection object
#' @param sql_statement SQL to send
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbSendStatement function
#' @import DatabaseConnector
#' @export

send <-
        function(conn,
                 conn_fun,
                 sql_statement,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {


                if (render_only) {

                        typewrite_sql(sql_statement = sql_statement)
                        invisible(sql_statement)

                } else {

                if (!missing(conn_fun)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(dc(conn = conn))

                }
                # +++
                # Checks
                # +++


                check_conn(conn = conn)

                if (render_sql) {

                        typewrite_sql(sql_statement = sql_statement)

                }

                if (verbose) {

                        secretary::typewrite("Sending...")

                }

                DatabaseConnector::dbSendStatement(conn = conn,
                                                   statement = sql_statement,
                                                   ...)

                if (verbose) {

                        secretary::typewrite("Sending...complete")

                }
                }

        }

