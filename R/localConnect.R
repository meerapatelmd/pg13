#' Connect to Local PostGres Server
#' @param dbname database name with the corresponding 'localhost/{dbname}' server
#' @param port   defults to "5432"
#' @param schema defaults to NULL
#' @import DatabaseConnector
#' @export

localConnect <-
        function(dbname = "athena",
                 port = 5432) {

                conn_details <- makeLocalConnDetails(dbname = dbname,
                                                     port = port)
                DatabaseConnector::connect(conn_details)
        }
