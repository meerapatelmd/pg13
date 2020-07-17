#' Render SQL to Rename a Table
#' @description This will rename a table within a schema, but not move the table out of a schema.
#' @import SqlRender
#' @export

renderRenameTable <- 
    function(schema,
             tableName,
             newTableName) {
        
        SqlRender::render(SqlRender::readSql("inst/sql/renameTable.sql"),
                          schema = schema,
                          tableName = tableName,
                          newTableName = newTableName)
        
    }
