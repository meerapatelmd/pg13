#' Send Query to any Postgres connection
#' @param conn Connection object
#' @param file path to sql file
#' @param sql_statement SQL Query
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbGetQuery function
#' @import DatabaseConnector
#' @import SqlRender
#' @export

fileQuery <-
    function(conn,
             file,
             ...) {

        sql_statement <- SqlRender::readSql(sourceFile = file)

        query(conn = conn,
              sql_statement = sql_statement,
              ...)

    }
