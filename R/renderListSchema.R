#' Render SQL to List All Schema
#' @description
#' Renders a SQL statement that will list all schema in a database.
#' @import SqlRender
#' @export

renderLsSchema <-
    function(schema) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        SqlRender::render(SqlRender::readSql(paste0(path, "/lsSchema.sql")))

    }
