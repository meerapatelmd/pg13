#' Append to am Existing Table
#' @import DatabaseConnector
#' @description Like the writeTable function, this function is a wrapper around a DatabaseConnector function rather than one where a SQL statement is rendered using the SqlRender package. This function performs the additional step of converting all inputs to the data.frame class, especially in cases where the input is a tibble.
#' @param tableName Name of table to write to.
#' @param schema schema where `tableName` is located.
#' @param .data dataframe to append
#' @param ... Additional arguments passed to DatabaseConnector::dbAppendTable
#' @export


appendTable <-
    function(conn = conn,
             schema,
             tableName,
             .data,
             ...) {

            .Deprecated(new = "append")

            schemaTableName <- constructSchemaTableName(schema = schema,
                                                        tableName = tableName)


            if (nrow(.data)) {

                    DatabaseConnector::dbAppendTable(conn = conn,
                                                    name = schemaTableName,
                                                    value = .data %>%
                                                        as.data.frame(),
                                                    ...)
            }

    }

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
#' @rdname append
#' @family table functions
#'
#' @importFrom DatabaseConnector dbAppendTable
#'
#' @export


append <-
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


#' Write a Table
#' @import DatabaseConnector
#' @description Unlike the dropTable and renameTable functions, this function is a wrapper around the DatabaseConnector::dbWriteTable function rather than one where a SQL statement is rendered using the SqlRender package. This function that converts all inputs to the data.frame class, especially in cases where the input is a tibble, in which case an error would be thrown when writing.
#' @param ... Additional arguments passed to DatabaseConnector::dbWriteTable
#' @export


writeTable <-
        function(conn = conn,
                 schema,
                 tableName,
                 .data,
                 ...) {

                .Deprecated(new = "write")

                schemaTableName <- constructSchemaTableName(schema = schema,
                                                            tableName = tableName)

                DatabaseConnector::dbWriteTable(conn = conn,
                                                name = schemaTableName,
                                                value = .data %>%
                                                        as.data.frame(),
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
#' @export


write <-
        function(conn,
                 schema,
                 tableName,
                 data,
                 ...) {

                brake_closed_conn(conn = conn)
                flag_no_rows(data = data)

                schemaTableName <- constructSchemaTableName(schema = schema,
                                                            tableName = tableName)

                DatabaseConnector::dbWriteTable(conn = conn,
                                                name = schemaTableName,
                                                value = as.data.frame(data),
                                                ...)

        }
