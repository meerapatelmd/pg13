#' List Fields
#' @import DatabaseConnector
#' @export


lsFields <-
    function(conn,
             tableName,
             schema = NULL) {

            DatabaseConnector::dbListFields(conn = conn,
                                            name = tableName,
                                            schema = schema)


    }
