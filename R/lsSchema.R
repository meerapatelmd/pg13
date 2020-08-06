#' List Schema
#' @description
#' List all schema in the connection.
#' @export

lsSchema <-
        function(conn,
                 schema) {

                query(conn = conn,
                     renderLsSchema())
        }
