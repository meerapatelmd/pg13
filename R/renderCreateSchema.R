#' Render SQL to Drop a Schema
#' @description Drop a schema if it exists
#' @import SqlRender
#' @export

renderCreateSchema <-
    function(schema) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        SqlRender::render(SqlRender::readSql(paste0(path, "/createSchema.sql")),
                          schema = schema)

    }
