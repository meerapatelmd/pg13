#' @title
#' Append a Table
#'
#' @description
#' Like the writeTable function, this function is a wrapper around a DatabaseConnector function rather than one where a SQL statement is rendered using the SqlRender package. This function performs the additional step of converting all inputs to the data.frame class, especially in cases where the input is a tibble.
#'
#' @inheritParams base_args
#' @param           ...     Additional arguments passed to `DatabaseConnector::dbAppendTable()`
#'
#' @rdname append_table
#' @family table functions
#'
#' @importFrom DatabaseConnector dbAppendTable
#'
#' @export
#' @example inst/example/table.R
#' @family table functions

append_table <-
        function(conn,
                 conn_fun,
                 schema,
                 table,
                 data,
                 verbose = TRUE,
                 render_sql = TRUE,
                 ...) {

                if (!missing(conn_fun)) {
                    conn <- eval(rlang::parse_expr(conn_fun))
                    on.exit(dc(conn = conn,
                               verbose = verbose),
                            add = TRUE,
                            after = TRUE)
                }

            check_conn(conn = conn)
            check_outflow(data = data)

            schema_table <- sprintf("%s.%s", schema, table)


                if (render_sql) {

                    typewrite_sql(sql_statement = "N/A")

                }

                if (verbose) {

                    typewrite_activity(sprintf("Appending %s...", schema_table))

                }

                DatabaseConnector::dbAppendTable(conn = conn,
                                                 name = schema_table,
                                                 value = as.data.frame(data),
                                                 ...)

                if (verbose) {

                    typewrite_activity(sprintf("Appending %s...complete", schema_table))

                }

        }


#' @title
#' Write a Table
#'
#' @description
#' Unlike the dropTable and renameTable functions, this function is a wrapper around the `DatabaseConnector::dbWriteTable()` function rather than one where a SQL statement is rendered using the SqlRender package. This function that converts all inputs to a dataframe, especially in cases where the input is a tibble, in which case an error would be thrown when writing.
#'
#' @inheritParams base_args
#' @param       ...     Additional arguments passed to `DatabaseConnector::dbWriteTable()`
#'
#' @importFrom DatabaseConnector dbWriteTable
#'
#' @rdname write_table
#'
#' @export
#' @example inst/example/table.R
#' @family table functions

write_table <-
        function(conn,
                 conn_fun,
                 schema,
                 table_name,
                 data,
                 drop_existing = FALSE,
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


                # <---! Performs any checks on the connection already so it is skipped --->

                if (drop_existing) {

                    drop_table(conn = conn,
                               conn_fun = conn_fun,
                               schema = schema,
                               table = table_name,
                               if_exists = TRUE,
                               verbose = verbose,
                               render_sql = render_sql,
                               render_only = render_only)

                } else {

                    check_conn(conn = conn)

                }


                check_outflow(data = data,
                              table_name = table_name)

                schema_table_name <- sprintf("%s.%s", schema, table_name)


                if (!render_only) {

                if (verbose) {

                    typewrite_activity(sprintf("Writing %s...", schema_table_name))

                }

                DatabaseConnector::dbWriteTable(conn = conn,
                                                name = schema_table_name,
                                                value = as.data.frame(data),
                                                ...)

                if (verbose) {

                    typewrite_activity(sprintf("Writing %s...complete", schema_table_name))

                }

                } else {

                    typewrite_activity(sprintf("No SQL to render for write_table()"))

                }

        }


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




#' @title
#' Search a Table for a Value
#'
#' @description
#' Loop a query for a set of one or more values in a table across all the existing fields or optionally, a subset of the fields. Both the values and the table fields are ensured compatibility by 1. Converting each value in the `values` argument to the character class and 2. Casting each table field as varchar in the query.
#'
#' @inheritParams base_args
#' @param case_insensitive  If TRUE, both sides of the query are converted to lowercase.
#' @param values            Vector of length 1 or greater to search for.
#' @param ...               (Optional) Character strings of 1 or more fields in the table to search in.
#'
#' @importFrom rlang list2
#'
#' @rdname search_table
#' @family table functions
#' @example inst/example/table.R

search_table <-
        function(conn,
                 conn_fun,
                 schema,
                 table,
                 ...,
                 values,
                 case_insensitive = TRUE,
                 verbose = TRUE,
                 render_sql = TRUE) {

                if (!missing(conn_fun)) {
                    conn <- eval(rlang::parse_expr(conn_fun))
                    on.exit(dc(conn = conn,
                               verbose = verbose),
                            add = TRUE,
                            after = TRUE)
                }

                check_conn(conn = conn)

                # Format Values for SQL
                values <- as.character(values)

                if (case_insensitive) {
                        values <- tolower(values)
                }
                values <- s_quo(values)



                # Get Fields vector to loop over for each SQL query
                if (missing(...)) {

                        fields <- ls_fields(conn = conn,
                                           schema = schema,
                                           table = table,
                                           verbose = verbose,
                                           render_sql = FALSE)

                } else {

                        fields <- unlist(rlang::list2(...))

                }


                sql_statements <- list()
                for (field in fields) {

                    i <- 1+length(sql_statements)

                    if (case_insensitive) {


                                sql_statements[[i]] <-
                                SqlRender::render(
                                                    "
                                                    SELECT *
                                                    FROM @schema.@table t
                                                    WHERE LOWER(t.@Field::varchar) IN (@values)
                                                    ;
                                                    ",
                                    schema = schema,
                                    table = table,
                                    Field = field,
                                    values = values
                                )

                    } else {

                        sql_statements[[i]] <-
                            SqlRender::render(
                                                "
                                                SELECT *
                                                FROM @schema.@table t
                                                WHERE t.@Field::varchar IN (@values)
                                                ;
                                                ",
                                schema = schema,
                                table = table,
                                Field = field,
                                values = values
                            )



                    }

                }


                resultsets <- list()
                for (i in seq_along(sql_statements)) {

                        resultsets[[i]] <-
                            suppressWarnings(
                            query(conn = conn,
                                  sql_statement = sql_statements[[i]],
                                  verbose = verbose,
                                  render_sql = render_sql))

                }

                names(resultsets) <- fields

                metrics <-
                    resultsets %>%
                    purrr::map(~ tibble::as_tibble_col(x = nrow(.), column_name = "Rows")) %>%
                    dplyr::bind_rows(.id = "Field")


                list(ROWS = metrics,
                     RESULTSETS = resultsets %>%
                                        purrr::keep(~ nrow(.) > 0))

        }


