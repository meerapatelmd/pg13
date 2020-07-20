#' Render "WHERE x NOT IN y"
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param vector vector of values that the SQL query is filtering for
#' @export


constructWhereNotIn <-
    function(field,
             vector) {

        if (is.character(vector)) {

            vector <- paste0("'", vector, "'")

        }

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql_constr")

        SqlRender::render(SqlRender::readSql(paste0(path, "/whereNotIn.sql")),
                          field = field,
                          vector = vector)
    }
