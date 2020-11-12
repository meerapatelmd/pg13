#' Create a new Database
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

createDB <-
    function(conn,
             newDB,
             ...) {


            sql_statement <- renderCreateDB(newDB = newDB)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

    }






#' Rename a table in a Postgres schema
#' @description This function will rename a table in a schema, but not move it out of a schema.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

renameDB <-
    function(conn,
             db,
             newDB,
             ...) {


            sql_statement <- renderRenameDB(schema = schema,
                                               db = db,
                                               newDB = newDB)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

    }






