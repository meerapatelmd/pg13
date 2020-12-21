#' @title
#' Postgres Connection
#'
#' @description
#' Creating a connection to a Postgres Database involves first making a connectionDetails object, which is a list of credentials and other parameters for the target database, followed by making the database connection itself that is declared in a connection class object.
#'
#' @seealso
#'  \code{\link[DatabaseConnector]{createConnectionDetails}},
#'  \code{\link[DatabaseConnector]{connect}}
#'
#' @name connection
NULL

#' @title
#' Connect to a Postgres Database
#'
#' @keywords internal
#'
#' @importFrom DatabaseConnector createConnectionDetails connect
#' @export


conn_db <-
        function(user,
                 password,
                 port,
                 server) {

                conn_details <- DatabaseConnector::create_connection_details(dbms = "postgresql",
                                                                           user = user,
                                                                           password = password,
                                                                           port = port,
                                                                           server = server)

                DatabaseConnector::connect(conn_details)
        }


#' @title
#' Connect to a Postgres Database
#'
#' @inheritParams DatabaseConnector::createConnectionDetails
#' @param verbose If TRUE, returns console messages when a connection has been secured.
#' @inherit DatabaseConnector::connect return
#'
#' @rdname connect
#'
#' @export

connect <-
        function() {
                "dummy"
        }


#' @title
#' Connection Function Factory
#'
#' @description
#' Customize a connection function.
#'
#' @inheritParams connect
#'
#' @rdname connect_ff
#' @export

connect_ff <-
        function() {
                "dummy"
        }


#' Connect without Console Messages
#' @keywords internal
#' @export

quietly.conn_db <-
        function() {
                "dummy"
        }

#' @title
#' Disconnect a Connection
#'
#' @description
#' Disconnect a Postgres Connection object with the option of removing the object from the parent environment.
#'
#' @inheritParams base_args
#' @param verbose If TRUE, returns a console message when the connection has been closed.
#' @param ...           Additional arguments passed to \code{\link[DatabaseConnector]{dbDisconnect}}.
#' @param remove        If TRUE, the Connection object argument is removed from the parent environment.
#'
#' @rdname dc
#'
#' @importFrom DatabaseConnector dbDisconnect
#'
#' @export

dc <-
        function(conn,
                 ...,
                 verbose = TRUE,
                 remove = FALSE) {

                DatabaseConnector::dbDisconnect(conn = conn,
                                                ...)

                if (verbose) {
                        secretary::typewrite("Postgres connection closed")
                }

                if (remove) {

                        rm(list = deparse(substitute(conn)), envir = parent.frame())

                }
        }
