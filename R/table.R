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


append_table <-
        function(conn,
                 conn_fun,
                 schema,
                 tableName,
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

                brake_closed_conn(conn = conn)
                flag_no_rows(data = data)

                schema_table_name <- sprintf("%s.%s", schema, table_name)


                if (render_sql) {

                    typewrite_sql(sql_statement = "N/A")

                }

                if (verbose) {

                    typewrite_activity("Appending...")

                }

                DatabaseConnector::dbAppendTable(conn = conn,
                                                 name = schemaTableName,
                                                 value = as.data.frame(data),
                                                 ...)

                if (verbose) {

                    typewrite_activity("Appending...complete")

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


write_table <-
        function(conn,
                 conn_fun,
                 schema,
                 tableName,
                 data,
                 drop_existing = FALSE,
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

                brake_closed_conn(conn = conn)
                flag_no_rows(data = data)

                if (drop_existing) {

                    dropTable(conn = conn,
                              schema = schema,
                              tableName = tableName,
                              if_exists = TRUE)
                }

                schema_table_name <- sprintf("%s.%s", schema, table_name)


                if (render_sql) {

                    typewrite_sql(sql_statement = "N/A")

                }

                if (verbose) {

                    typewrite_activity("Writing...")

                }

                DatabaseConnector::dbWriteTable(conn = conn,
                                                name = schemaTableName,
                                                value = as.data.frame(data),
                                                ...)

                if (verbose) {

                    typewrite_activity("Writing...complete")

                }

        }

#' @title
#' Drop a Table
#'
#' @inheritParams base_args
#' @param           if_exists   If TRUE, the table will be dropped only if it exists.
#' @param           ...         Additional arguments passed to `DatabaseConnector::dbSendStatement()`
#'
#' @rdname drop_table
#' @export

drop_table <-
    function(conn,
             conn_fun,
             schema,
             tableName,
             if_exists = TRUE,
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

            brake_closed_conn(conn = conn)

            if (if_exists) {

                sql_statement <- sprintf("drop table if exists %s.%s;", schema, table_name)

            } else {

                sql_statement <- sprintf("drop table %s.%s;", schema, table_name)

            }

            if (render_sql) {

                typewrite_sql(sql_statement = sql_statement)

            }

            if (verbose) {

                typewrite_activity("Dropping...")

            }


            send(conn = conn,
                 sql_statement = sql_statement,
                 verbose = FALSE,
                 render_sql = FALSE,
                 ...)


            if (verbose) {

                typewrite_activity("Dropping...complete")

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


read_table <-
        function(conn,
                 schema,
                 tableName,
                 verbose = TRUE,
                 render_sql = TRUE) {


            sql_statement <- sprintf("select * from %s.%s;", schema, table_name)

            if (render_sql) {

                typewrite_sql(sql_statement = sql_statement)
            }



            if (verbose) {

                typewrite_activity("Reading...")

            }

            resultset <-
            query(conn = conn,
                  sql_statement = sql_statement,
                  verbose = FALSE,
                  render_sql = FALSE)

            if (verbose) {

                typewrite_activity("Reading...complete")

            }

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
#'
#' @export


search_table <-
        function(conn,
                 schema,
                 tableName,
                 ...,
                 values,
                 case_insensitive = TRUE,
                 verbose = TRUE,
                 render_sql = TRUE) {

                brake_closed_conn(conn = conn)

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
                                           tableName = tableName,
                                           verbose = verbose,
                                           render_sql = FALSE)

                } else {

                        fields <- unlist(rlang::list2(...))

                }


                sql_statements <- list()
                for (Field in Fields) {

                    i <- 1+length(sql_statements)

                    if (case_insensitive) {


                                sql_statements[[i]] <-
                                SqlRender::render(
                                                    "
                                                    SELECT *
                                                    FROM @schema.@tableName t
                                                    WHERE LOWER(t.@Field::varchar) IN (@values)
                                                    ;
                                                    ",
                                    schema = schema,
                                    tableName = tableName,
                                    Field = Field,
                                    values = values
                                )

                    } else {

                        sql_statements[[i]] <-
                            SqlRender::render(
                                                "
                                                SELECT *
                                                FROM @schema.@tableName t
                                                WHERE t.@Field::varchar IN (@values)
                                                ;
                                                ",
                                schema = schema,
                                tableName = tableName,
                                Field = Field,
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


