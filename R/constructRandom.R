#' Construct ORDER BY RANDOM() Part of SQL Query
#' @import SqlRender
#' @export


constructRandom <-
    function(n) {


        SqlRender::render(SqlRender::readSql("inst/sql_constr/orderByRandom.sql"),
                          n = n)

    }
