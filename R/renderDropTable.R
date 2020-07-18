#' Render SQL to Drop a Table
#' @description Drop a table if it exists
#' @import SqlRender
#' @export

renderDropTable <-
    function(schema,
             tableName) {

        SqlRender::render(SqlRender::readSql("inst/sql/dropTable.sql"),
                          schema = schema,
                          tableName = tableName)

    }
