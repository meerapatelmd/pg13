#' Render SQL to Rename a Table
#' @description This will rename a table within a schema, but not move the table out of a schema.
#' @import SqlRender
#' @export

renderRenameTable <-
    function(schema,
             tableName,
             newTableName) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        SqlRender::render(SqlRender::readSql(paste0(path,"/renameTable.sql")),
                          schema = schema,
                          tableName = tableName,
                          newTableName = newTableName)

    }
