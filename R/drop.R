#' @title
#' Drop a Table
#'
#' @inheritParams base_args
#' @param           if_exists   If TRUE, the table will be dropped only if it exists.
#' @param           ...         Additional arguments passed to `DatabaseConnector::dbSendStatement()`
#'
#' @rdname drop_table
#' @export
#' @example inst/example/table.R
#' @family table functions

drop_table <-
        function(conn,
                 conn_fun,
                 schema,
                 table,
                 if_exists = TRUE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {

                if (!missing(conn_fun)) {
                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(dc(conn = conn,
                                   verbose = verbose),
                                add = TRUE,
                                after = TRUE)
                }

                check_conn(conn = conn)


                if (if_exists) {

                        sql_statement <- sprintf("DROP TABLE IF EXISTS %s.%s;", schema, table)

                } else {

                        sql_statement <- sprintf("DROP TABLE %s.%s;", schema, table)

                }

                if (render_sql) {

                        typewrite_sql(sql_statement = sql_statement)

                }

                if (verbose) {

                        typewrite_activity(sprintf("Dropping %s.%s...", schema, table))

                }


                send(conn = conn,
                     sql_statement = sql_statement,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only,
                     ...)


                if (verbose) {

                        typewrite_activity(sprintf("Dropping %s.%s...complete", schema, table))

                }

        }
