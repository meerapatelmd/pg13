#' Render SQL to copy a file to a table
#' @import SqlRender
#' @export

renderCopy <-
    function(schema,
             tableName,
             csvFilePath) {

        SqlRender::render(SqlRender::readSql("inst/sql/copy.sql"),
                          schema = schema,
                          tableName = tableName,
                          csvFilePath = csvFilePath)

    }
