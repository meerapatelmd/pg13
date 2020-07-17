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
        
            pgSend(conn = conn,
                   sql_statement = sql_statement,
                   ...)
        
    }

