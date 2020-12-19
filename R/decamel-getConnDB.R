#' @title
#' Get Connection Database
#'
#' @description
#' Get the name of the database that the connection object is connected to.
#'
#' @export
#' @rdname get_conn_db

get_conn_db <-
        function(conn) {
                conn@jConnection$getCatalog()

        }
