#' Construct ORDER BY RANDOM()
#' @param n Row number desired in the output
#' @import SqlRender
#' @export


constructRandom <-
    function(n) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql_constr")

        SqlRender::render(SqlRender::readSql(paste0(path, "/orderByRandom.sql")),
                          n = n)

    }
