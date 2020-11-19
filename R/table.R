#' @title
#' Append a Table
#'
#' @description
#' Like the writeTable function, this function is a wrapper around a DatabaseConnector function rather than one where a SQL statement is rendered using the SqlRender package. This function performs the additional step of converting all inputs to the data.frame class, especially in cases where the input is a tibble.
#'
#' @param tableName Name of table to write to.
#' @param schema schema where `tableName` is located.
#' @param data dataframe to append
#' @param ... Additional arguments passed to DatabaseConnector::dbAppendTable
#'
#' @rdname appendTable
#' @family table functions
#'
#' @importFrom DatabaseConnector dbAppendTable
#'
#' @export


appendTable <-
        function(conn = conn,
                 schema,
                 tableName,
                 data,
                 verbose = TRUE,
                 render_sql = TRUE,
                 ...) {

                brake_closed_conn(conn = conn)
                flag_no_rows(data = data)

                schemaTableName <- sprintf("%s.%s", schema, tableName)


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
#' Unlike the dropTable and renameTable functions, this function is a wrapper around the DatabaseConnector::dbWriteTable function rather than one where a SQL statement is rendered using the SqlRender package. This function that converts all inputs to the data.frame class, especially in cases where the input is a tibble, in which case an error would be thrown when writing.
#' @param ... Additional arguments passed to DatabaseConnector::dbWriteTable
#'
#' @importFrom DatabaseConnector dbWriteTable
#'
#' @rdname writeTable
#'
#' @export


writeTable <-
        function(conn,
                 schema,
                 tableName,
                 data,
                 drop_existing = FALSE,
                 ...) {

                brake_closed_conn(conn = conn)
                flag_no_rows(data = data)

                if (drop_existing) {

                    dropTable(conn = conn,
                              schema = schema,
                              tableName = tableName,
                              if_exists = TRUE)
                }

                schemaTableName <- sprintf("%s.%s", schema, tableName)


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

#' Drop a table in a Postgres schema
#' @description Drop a table if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropTable <-
    function(conn,
             schema,
             tableName,
             if_exists = TRUE,
             verbose = TRUE,
             render_sql = TRUE,
             ...) {


            if (if_exists) {

                sql_statement <- sprintf("DROP TABLE IF EXISTS %s.%s;", schema, tableName)

            } else {

                sql_statement <- sprintf("DROP TABLE %s.%s;", schema, tableName)

            }

            if (render_sql) {

                typewrite_sql(sql_statement = sql_statement)

            }

            if (verbose) {

                typewrite_activity("Dropping...")

            }


            send(conn = conn,
                 sql_statement = sql_statement,
                 verbose = verbose,
                 render_sql = render_sql,
                 ...)


            if (verbose) {

                typewrite_activity("Dropping...complete")

            }

    }


#' Get Full Table
#' @export


readTable <-
        function(conn,
                 schema,
                 tableName,
                 verbose = TRUE,
                 render_sql = TRUE) {


            sql_statement <- sprintf("SELECT * FROM %s.%s;", schema, tableName)

            if (render_sql) {

                typewrite_sql(sql_statement = sql_statement)
            }



            if (verbose) {

                typewrite_activity("Reading...")

            }

            query(conn = conn,
                  sql_statement = sql_statement,
                  verbose = verbose,
                  render_sql = render_sql)

            if (verbose) {

                typewrite_activity("Reading...complete")

            }

        }



#' @title
#' Search a Table for a Value
#'
#' @description
#' If the Field to query is unknown, this function can be used to search and query all Fields or a subset of Fields for a given value. The `value` is converted to character and the Field is cast to varchar in the query and as a result, the query times may be long which may require providing a subset of Fields as an argument rather than searching the entire Table in many cases.
#'
#'
#'
#' @export


searchTable <-
        function(conn,
                 schema,
                 tableName,
                 values,
                 case_insensitive = TRUE,
                 ...) {

                # Prepare Value argument
                values <- as.character(values)

                if (case_insensitive) {
                        values <- tolower(values)
                }

                values <- sQuo(values)

                if (missing(...)) {
                        Fields <- lsFields(conn = conn,
                                           schema = schema,
                                           tableName = tableName)
                } else {
                        Fields <- unlist(as.list(...))
                }

                        output <- list()

                        if (case_insensitive) {

                                for (i in 1:length(Fields)) {

                                        Field <- Fields[i]

                                        sql_statement <-
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


                                        print(sql_statement)


                                        x <-
                                                query(conn = conn,
                                                      sql_statement = sql_statement)
                                        if (!is.null(x)) {
                                                output[[i]] <- x
                                                names(output)[i] <- Field
                                        }

                                }

                        } else {

                                for (i in 1:length(Fields)) {

                                Field <- Fields[i]

                                sql_statement <-
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

                                x <-
                                        query(conn = conn,
                                              sql_statement = sql_statement)
                                if (!is.null(x)) {
                                        output[[i]] <- x
                                        names(output)[i] <- Field
                                }


                                }
                        }



                output
        }








