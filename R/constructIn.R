#' Render y in WHERE x IN y Component
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param vector vector of values that the SQL query is filtering for
#' @export


constructIn <-
    function(vector) {

        SqlRender::render(SqlRender::readSql("inst/sql_constr/in.sql"),
                          vector = vector)
    }
