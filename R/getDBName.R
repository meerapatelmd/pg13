

#' @export

getDBName <-
        function(conn) {

                                conn@jConnection$getCatalog()

        }
