#' List Schema
#' @description
#' List all schema in the connection.
#' @export

lsSchema <-
        function(conn) {

                query(conn = conn,
                     renderLsSchema())
        }
