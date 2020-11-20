#' @title
#' Create a New Database
#'
#' @inheritParams base_args
#' @param dbname Database name.
#' @param ... Additional arguments passed to `DatabaseConnector::dbSendStatement()`.
#' @export

createDB <-
    function(conn,
             dbname,
             ...) {


            sql_statement <- sprintf("CREATE DATABASE %s;", dbname)

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






