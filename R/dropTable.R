#' Drop a table in a Postgres schema
#' @description Drop a table if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropTable <-
    function(conn,
             schema,
             tableName,
             if_exists = TRUE,
             ...) {


            sql_statement <- renderDropTable(schema = schema,
                                               tableName = tableName,
                                             if_exists = if_exists)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

    }

