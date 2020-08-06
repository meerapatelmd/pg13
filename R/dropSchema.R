#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropSchema <-
    function(conn,
             schema,
             cascade = FALSE,
             ...) {


            sql_statement <- renderDropSchema(schema = schema,
                                               cascade = cascade)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

    }

