#' Render SQL to copy a file to a table
#' @import SqlRender
#' @export

renderCopy <-
    function(schema,
             tableName,
             csvFilePath) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        SqlRender::render(SqlRender::readSql(paste0(path, "/copy.sql")),
                          schema = schema,
                          tableName = tableName,
                          csvFilePath = csvFilePath)

    }
