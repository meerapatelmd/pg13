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





