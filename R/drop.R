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
#' @family drop functions

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

                if (if_exists) {

                        sql_statement <- sprintf("DROP TABLE IF EXISTS %s.%s;", schema, table)

                } else {

                        sql_statement <- sprintf("DROP TABLE %s.%s;", schema, table)

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



#' @title
#' Drop a Batch of Tables using Regex
#'
#' @inheritParams base_args
#' @inheritParams base::grep
#' @param           if_exists   If TRUE, the table will be dropped only if it exists.
#' @param           ...         Additional arguments passed to `DatabaseConnector::dbSendStatement()`
#'
#' @rdname drop_table_batch
#' @export
#' @example inst/example/table.R
#' @family table functions
#' @family drop functions

drop_table_batch <-
        function(conn,
                 conn_fun,
                 schema,
                 table,
                 if_exists = TRUE,
                 pattern,
                 ignore.case = FALSE,
                 perl = FALSE,
                 fixed = FALSE,
                 useBytes = FALSE,
                 invert = FALSE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {

                tables <- ls_tables(conn = conn,
                                  conn_fun = conn_fun,
                                  schema = schema,
                                  verbose = verbose,
                                  render_sql = render_sql)

                tables <-
                        grep(pattern = pattern,
                             x = tables,
                             ignore.case = ignore.case,
                             perl = perl,
                             value = TRUE,
                             fixed = fixed,
                             useBytes = useBytes,
                             invert = invert)



                for (table in tables) {

                        if (if_exists) {

                                sql_statement <- sprintf("DROP TABLE IF EXISTS %s.%s;", schema, table)

                        } else {

                                sql_statement <- sprintf("DROP TABLE %s.%s;", schema, table)

                        }


                        if (verbose) {

                                typewrite_activity(sprintf("Dropping %s.%s...", schema, table))


                        }


                        if (interactive()) {

                                secretary::press_enter()
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

        }


#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export
#' @example inst/example/schema.R
#' @rdname drop_schema
#' @family schema functions
#' @family drop functions
drop_schema <-
        function(conn,
                 conn_fun,
                 schema,
                 cascade = FALSE,
                 if_exists = TRUE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {


                if (if_exists) {

                        if_exists_clause <- "IF EXISTS"

                } else {

                        if_exists_clause <- NULL

                }


                if (cascade) {

                        cascade_clause <- "CASCADE"

                } else {

                        cascade_clause <- NULL

                }


                sql_statement <-
                        SqlRender::render(
                                "DROP SCHEMA @if_exists_clause @schema @cascade_clause;",
                                schema = schema,
                                if_exists_clause = if_exists_clause,
                                cascade_clause = cascade_clause
                        )

                send(conn = conn,
                     conn_fun = conn_fun,
                     sql_statement = sql_statement,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only,
                     ...)

        }


#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export
#' @example inst/example/schema.R
#' @rdname drop_cascade
#' @family schema functions
#' @family drop functions
drop_cascade <-
        function(conn,
                 conn_fun,
                 schema,
                 if_exists = TRUE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {

                drop_schema(
                        conn = conn,
                        conn_fun = conn_fun,
                        schema = schema,
                        cascade = TRUE,
                        if_exists = if_exists,
                        verbose = verbose,
                        render_sql = render_sql,
                        render_only = render_only,
                        ...
                )

        }



#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export
#' @example inst/example/schema.R
#' @rdname drop_if_exists
#' @family schema functions
#' @family drop functions

drop_if_exists <-
        function(conn,
                 conn_fun,
                 schema,
                 cascade = FALSE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {


                drop_schema(
                        conn = conn,
                        conn_fun = conn_fun,
                        schema = schema,
                        cascade = cascade,
                        if_exists = TRUE,
                        verbose = verbose,
                        render_sql = render_sql,
                        render_only = render_only,
                        ...
                )

        }
