#' @title
#' Create a New Database
#'
#' @inheritParams base_args
#' @param dbname Database name.
#' @param ... Additional arguments passed to `DatabaseConnector::dbSendStatement()`.
#' @export

create_db <-
    function(conn,
             dbname,
             ...) {


            sql_statement <- sprintf("create database %s;", dbname)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

    }






#' Rename a table in a Postgres schema
#' @description This function will rename a table in a schema, but not move it out of a schema.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

rename_db <-
    function(conn,
             db,
             newDB,
             ...) {


            sql_statement <- render_rename_db(schema = schema,
                                               db = db,
                                               newDB = newDB)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

    }






