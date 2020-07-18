#' Render "WHERE x IN y" Component
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param vector vector of values that the SQL query is filtering for
#' @export


constructWhereLowerIn <-
    function(field,
             vector) {

        if (is.character(vector)) {

                vector <- paste0("'", vector, "'")

        }

        SqlRender::render(SqlRender::readSql("inst/sql_constr/whereLowerIn.sql"),
                          field = field,
                          vector = vector)
    }
