#' Render SQL to Create Database
#' @import SqlRender
#' @export

renderCreateDB <-
    function(schema,
             db,
             newDB) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        SqlRender::render("
                          CREATE DATABASE @newDB;
                          ",
                          newDB = newDB)

    }
