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
