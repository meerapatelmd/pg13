#' Render SQL to List All Schema
#' @description
#' Renders a SQL statement that will list all schema in a database.
#' @import SqlRender
#' @export

renderLsSchema <-
    function() {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        SqlRender::render("
                          SELECT nspname
                          FROM pg_catalog.pg_namespace
                          ;
                          ")

    }
