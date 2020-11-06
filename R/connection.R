#' @title
#' Postgres Connection
#'
#' @description
#' Creating a connection to a Postgres Database involves first making a connectionDetails object, which is a list of credentials and other parameters for the target database, and is followed by making the database connection itself that is declared in a connection class object.
#'
#' @seealso
#'  \code{\link[DatabaseConnector]{createConnectionDetails}},
#'  \code{\link[DatabaseConnector]{connect}},
#'   \code{\link{local}}
#'
#'
#' @export
NULL




brake_bad_conn <-
        function(conn) {
                if (!is_conn_open(conn)) {
                        stop("`conn` is not open")
                }
        }
