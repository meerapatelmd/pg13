#' Render y in WHERE x IN y Component
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param n rows to limit to
#' @export


constructLimit <-
    function(n) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql_constr")

        SqlRender::render(SqlRender::readSql(paste0(path, "/limit.sql")),
                          n = n)
    }
