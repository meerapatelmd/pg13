#' Disconnect a Postgres connection
#' @description This function also removes the connection object from the global environment if remove is set to true
#' @param ... Additional arguments passed to DatabaseConnector::dbDisconnect
#' @import DatabaseConnector
#' @export

dc <-
    function(conn,
             ...,
             remove = TRUE) {

                DatabaseConnector::dbDisconnect(conn = conn,
                                                ...)

                if (remove) {

                            rm(list = deparse(substitute(conn)), envir = globalenv())

                }
    }
