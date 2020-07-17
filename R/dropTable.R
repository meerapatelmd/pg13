#' Drop a table in a Postgres schema
#' @description Drop a table if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropTable <- 
    function(conn,
             schema,
             tableName,
             ...) {
        
        
            sql_statement <- renderRenameTable(schema = schema,
                                               tableName = tableName)
        
            pgSend(conn = conn,
                   sql_statement = sql_statement,
                   ...)
        
    }

