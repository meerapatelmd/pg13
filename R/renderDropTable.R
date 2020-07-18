#' Render SQL to Drop a Table
#' @description Drop a table if it exists
#' @import SqlRender
#' @export

renderDropTable <-
    function(schema,
             tableName) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        SqlRender::render(SqlRender::readSql(paste0(path, "/dropTable.sql")),
                          schema = schema,
                          tableName = tableName)

    }
