#' Drop a table in a Postgres schema
#' @description Drop a table if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropSchema <-
    function(conn,
             schema,
             cascade = FALSE,
             tableName,
             ...) {


            sql_statement <- renderDropSchema(schema = schema,
                                               cascade = cascade)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

    }

