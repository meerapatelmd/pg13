#' Write a Table
#' @import DatabaseConnector
#' @description Unlike the dropTable and renameTable functions, this function is a wrapper around the DatabaseConnector::dbWriteTable function rather than one where a SQL statement is rendered using the SqlRender package. This function that converts all inputs to the data.frame class, especially in cases where the input is a tibble, in which case an error would be thrown when writing.
#' @param ... Additional arguments passed to DatabaseConnector::dbWriteTable
#' @export


writeTable <-
    function(conn = conn,
             tableName,
             .data,
             ...) {



            DatabaseConnector::dbWriteTable(conn = conn,
                                            name = tableName,
                                            value = .data %>%
                                                as.data.frame(),
                                            ...)

    }
