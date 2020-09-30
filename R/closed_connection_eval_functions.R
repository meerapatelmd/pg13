#' @title
#' Is the Connection Closed?
#'
#' @description
#' This function checks if a connection object is closed.
#'
#' @param conn Postgres connection object
#'
#' @return
#' TRUE if the connection is closed and FALSE invisibly if it is open.
#'
#' @rdname isClosed
#' @export

isClosed <-
        function(conn) {

                results <- tryCatch(print(conn),
                                        error = function(e) NULL)

                if (is.null(results)) {
                        TRUE
                } else {
                        invisible(FALSE)
                }
        }


#' @title
#' Remove a Closed Connection Object
#'
#' @description
#' This function removes a onnection object from the Global Environment if it is closed.
#'
#' @param conn Postgres connection object
#'
#' @rdname rmIfClosed
#' @export

rmIfClosed <-
        function(conn) {

                results <- isClosed(conn = conn)

                if (results == TRUE) {

                        rm(list = deparse(substitute(conn)), envir = globalenv())

                }

        }
