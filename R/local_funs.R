#' Query local Postgess from a file
#' @import DatabaseConnector
#' @export

localFileQuery <-
    function (file,
              dbname = "athena",
              port = 5432) {

        conn <- localConnect(dbname = dbname,
                             port = port)

        data <- fileQuery(conn = conn,
                          file = file)

        dc(conn = conn,
           remove = FALSE)

        return(data)
    }





#' Send Statement to a local Postgess from a file
#' @import DatabaseConnector
#' @export

localFileSend <-
    function (file,
              dbname = "athena",
              port = 5432) {

        conn <- localConnect(dbname = dbname,
                             port = port)

        data <- fileSend(conn = conn,
                          file = file)

        dc(conn = conn,
           remove = FALSE)

        return(data)
    }






#' Query local Postgess
#' @import DatabaseConnector
#' @export

localQuery <-
    function (sql_statement,
              dbname = "athena",
              port = 5432) {

            conn <- localConnect(dbname = dbname,
                                 port = port)

            data <- query(conn = conn,
                          sql_statement = sql_statement)

            dc(conn = conn,
               remove = FALSE)

            return(data)
    }





#' Send a SQL Statement to a Local Postgres
#' @export


localSend <-
    function(sql_statement,
             dbname = "athena",
             port = 5432) {

                conn <- localConnect(dbname = dbname,
                                    port = port)

                send(conn = conn,
                     sql_statement = sql_statement)

                dc(conn = conn,
                   remove = FALSE)
    }





