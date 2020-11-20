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





#' Send Query to any Postgres connection
#' @param conn Connection object
#' @param file path to sql file
#' @param sql_statement SQL Query
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbGetQuery function
#' @import DatabaseConnector
#' @import SqlRender
#' @export

fileSend <-
    function(conn,
             file,
             ...) {

        sql_statement <- SqlRender::readSql(sourceFile = file)

        send(conn = conn,
              sql_statement = sql_statement,
              ...)

    }










#' Query using a link to SQL file
#' @import DatabaseConnector
#' @import SqlRender
#' @export

linkQuery <-
    function(conn,
             link) {

                tmp_sql <- tempfile(fileext = ".txt")

                download.file(link,
                              destfile = tmp_sql)



                query(conn = conn,
                      statement = SqlRender::readSql(tmp_sql))


                unlink(tmp_sql)

    }





#' Send a statument using a link to SQL file
#' @import DatabaseConnector
#' @import SqlRender
#' @export

linkSend <-
    function(conn,
             link) {

                tmp_sql <- tempfile(fileext = ".txt")

                download.file(link,
                              destfile = tmp_sql)



                send(conn = conn,
                      statement = SqlRender::readSql(tmp_sql))


                unlink(tmp_sql)

    }










