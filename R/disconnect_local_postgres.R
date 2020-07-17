#' Disconnect from Postgres
#' @param conn connection object
#' @import DatabaseConnector
#' @export

disconnect_local_postgres <-
        function(conn) {
                DatabaseConnector::disconnect(conn)
        }
