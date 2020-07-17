#' Disconnect a connection to a local database
#' @description This function also removes the connection object from the global environment.
#' @importFrom DBI dbDisconnect
#' @export

dc_local_conn <-
    function(conn_to_dc) {
        if ("MySQLConnection" %in% class(conn_to_dc)) {
            DBI::dbDisconnect(conn = conn_to_dc)
        } else {
            disconnect_local_postgres(conn_to_dc)
        }
        rm(list = deparse(substitute(conn_to_dc)), envir = globalenv())
    }
