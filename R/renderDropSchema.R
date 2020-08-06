#' Render SQL to Drop a Schema
#' @description Drop a schema if it exists
#' @param cascade If TRUE, a DROP SCHEMA CASCADE is performed.
#' @import SqlRender
#' @export

renderDropSchema <-
    function(schema,
             cascade = FALSE) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        if (cascade) {

        SqlRender::render(SqlRender::readSql(paste0(path, "/dropSchemaCascade.sql")),
                          schema = schema)

        } else {
            SqlRender::render(SqlRender::readSql(paste0(path, "/dropSchema.sql")),
                              schema = schema)
        }

    }
