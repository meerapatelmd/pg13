#' @title
#' Get Connection Database
#'
#' @description
#' Get the name of the database that the connection object is connected to.
#'
#' @export
#' @rdname getConnDB

getConnDB <-
        function(conn) {
                conn@jConnection$getCatalog()

        }
