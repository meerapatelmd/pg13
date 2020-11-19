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
                 ...) {

                brake_closed_conn(conn = conn)
                flag_no_rows(data = data)


                schemaTableName <- constructSchemaTableName(schema = schema,
                                                            tableName = tableName)


                DatabaseConnector::dbAppendTable(conn = conn,
                                                 name = schemaTableName,
                                                 value = as.data.frame(data),
                                                 ...)

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

                schemaTableName <- constructSchemaTableName(schema = schema,
                                                            tableName = tableName)

                DatabaseConnector::dbWriteTable(conn = conn,
                                                name = schemaTableName,
                                                value = as.data.frame(data),
                                                ...)

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

            send(conn = conn,
                 sql_statement = sql_statement,
                 verbose = verbose,
                 render_sql = render_sql,
                 ...)

    }


#' Get Full Table
#' @export


readTable <-
        function(conn,
                 schema,
                 tableName,
                 verbose = TRUE,
                 render_sql = TRUE) {

                query(conn = conn,
                      sql_statement = sprintf("SELECT * FROM %s.%s", schema, tableName),
                      verbose = verbose,
                      render_sql = render_sql)

        }





#' Refresh Table with New Data
#' @description A refresh is when a table needs to be overwritten and the table that is being overwritten is off-loaded to another table with today's date and the index of iterations for that day.
#' @import stringr
#' @import secretary
#' @import purrr
#' @export


refreshTable <-
            function(conn,
                     schema,
                     tableName,
                     .data) {

                tableNameHist <- appendDate(name = toupper(tableName))


                todayTables <-
                    grep(tableNameHist,
                         lsTables(conn = conn,
                                  schema = schema),
                         value = TRUE)

                n <- length(todayTables)


                # If this table has not been written yet today
                if (n == 0) {
                    secretary::typewrite_bold("No Off-Loaded Tables Today")
                    secretary::typewrite_bold("Next Table Name:", tableNameHist)
                    # If more than 1 table has been written today, the new table name would be the length of the list of today's table + 1
                } else {
                    secretary::typewrite_bold("Off-Loaded Tables Today:")
                    todayTables %>%
                        purrr::map(function(x) secretary::typewrite(x, tabs = 1))

                    tableNameHist <- paste0(tableNameHist, "_", (1+n))
                    secretary::typewrite_bold("Next Table Name:", tableNameHist)

                }

                secretary::press_enter()

                renameTable(conn = conn,
                            schema = schema,
                            tableName = tableName,
                            newTableName = tableNameHist)

                secretary::typewrite_bold(tableName, "renamed to", tableNameHist)

                writeTable(conn = conn,
                           tableName = tableName,
                           schema = schema,
                           .data = .data %>%
                                    as.data.frame())

                secretary::typewrite_bold("New", tableName, "written.")

            }





#' Rename a table in a Postgres schema
#' @description This function will rename a table in a schema, but not move it out of a schema.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

renameTable <-
    function(conn,
             schema,
             tableName,
             newTableName,
             ...) {


            sql_statement <- renderRenameTable(schema = schema,
                                               tableName = tableName,
                                               newTableName = newTableName)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

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








