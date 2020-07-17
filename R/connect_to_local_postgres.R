#' Connect to Local PostGres Server
#' @param dbname database name with the corresponding 'localhost/{dbname}' server
#' @param port   defults to "5432"
#' @param schema defaults to NULL
#' @import DatabaseConnector
#' @export

connect_to_local_postgres <-
        function(dbname, schema = NULL, port = "5432") {
                ##Postgres settings

                dbms     <- "postgresql"
                server   <- paste0("localhost/", dbname)

                conn_details <- DatabaseConnector::createConnectionDetails(dbms  = dbms,
                                                                   server   = server,
                                                                   port     = port,
                                                                   schema   = schema)

                return(DatabaseConnector::connect(conn_details))
        }
