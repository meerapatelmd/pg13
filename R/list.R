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





#' List Schema
#' @description
#' List all schema in the connection.
#' @importFrom magrittr %>%
#' @export

lsSchema <-
        function(conn) {

                query(conn = conn,
                     renderLsSchema()) %>%
                        unlist() %>%
                        unname()

        }





#' List Tables
#' @import DatabaseConnector
#' @export


lsTables <-
    function(conn,
             schema = NULL) {

            DatabaseConnector::dbListTables(conn = conn,
                                        schema = schema)


    }





