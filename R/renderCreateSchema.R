#' Render SQL to Drop a Schema
#' @description Drop a schema if it exists
#' @import SqlRender
#' @export

renderCreateSchema <-
    function(schema) {

        SqlRender::render(SqlRender::readSql("inst/sql/createSchema.sql"),
                          schema = schema)

    }
