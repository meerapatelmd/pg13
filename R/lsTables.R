#' List Tables
#' @import DatabaseConnector
#' @export


lsTables <-
    function(conn,
             schema = NULL) {

            DatabaseConnector::dbListTables(conn = conn,
                                        schema = schema)


    }
