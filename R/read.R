#' @title
#' Read an Entire Table
#'
#' @description
#' Shortcut for a `SELECT *` SQL statement.
#'
#' @inheritParams base_args
#'
#' @export
#' @example inst/example/table.R
#' @family table functions
#' @family read functions
read_table <-
        function(conn,
                 conn_fun,
                 schema,
                 table,
                 verbose = TRUE,
                 render_sql = TRUE,
                 warn_no_rows = TRUE,
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


                sql_statement <- sprintf("SELECT * FROM %s.%s;", schema, table)

                if (render_sql) {

                        typewrite_sql(sql_statement = sql_statement)
                }



                if (verbose) {

                        typewrite_activity(sprintf("Reading %s.%s...", schema, table))

                }

                resultset <-
                        query(conn = conn,
                              sql_statement = sql_statement,
                              verbose = verbose,
                              render_sql = render_sql,
                              warn_no_rows = warn_no_rows,
                              render_only = render_only,
                              ...)

                if (verbose) {

                        typewrite_activity(sprintf("Reading %s.%s...complete", schema, table))

                }


                check_inflow(data = resultset)
                resultset

        }
