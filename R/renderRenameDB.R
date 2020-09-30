#' Render SQL to Rename a Table
#' @description This will rename a table within a schema, but not move the table out of a schema.
#' @import SqlRender
#' @export

renderRenameDB <-
    function(schema,
             db,
             newDB) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        SqlRender::render("ALTER DATABASE @db RENAME TO @newDB;",
                          schema = schema,
                          db = db,
                          newDB = newDB)

    }
