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
