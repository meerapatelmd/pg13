#' Render SQL to Create Database
#' @import SqlRender
#' @export

renderCreateDB <-
    function(schema,
             db,
             newDB) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        SqlRender::render(SqlRender::readSql(paste0(path,"/createDB.sql")),
                          newDB = newDB)

    }
