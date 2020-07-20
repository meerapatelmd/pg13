#' Render WHERE lowercase x LIKE y
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param term Character string that the field parameter is searched for.
#' @export


constructWhereLowerLike <-
    function(field,
             term) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql_constr")


        SqlRender::render(SqlRender::readSql(paste0(path, "/whereLowerLike.sql")),
                          field = field,
                          term = tolower(term))

    }
