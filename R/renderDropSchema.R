#' Render SQL to Drop a Schema
#' @description Drop a schema if it exists
#' @import SqlRender
#' @export

renderDropSchema <-
    function(schema) {

        SqlRender::render(SqlRender::readSql("inst/sql/dropSchema.sql"),
                          schema = schema)

    }
