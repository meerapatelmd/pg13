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
