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
