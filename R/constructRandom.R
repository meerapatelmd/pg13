#' Construct ORDER BY RANDOM() Part of SQL Query
#' @import SqlRender
#' @export


constructRandom <-
    function(n) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql_constr")

        SqlRender::render(SqlRender::readSql(paste0(path, "/orderByRandom.sql")),
                          n = n)

    }
