#' Render "WHERE x LIKE '%y%'"
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param term Character string that the field parameter is searched for.
#' @export


constructWhereLike <-
    function(field,
             term) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql_constr")


        SqlRender::render(SqlRender::readSql(paste0(path, "/whereLike.sql")),
                          field = field,
                          term = term)
    }
