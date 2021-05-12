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
#' @family connection functions


conn_db <-
        function(user = NULL,
                 password = NULL,
                 server = NULL,
                 port = NULL,
                 extraSettings = NULL,
                 oracleDriver = "thin",
                 connectionString = NULL,
                 pathToDriver =system.file(package = "pg13","driver")) {

                conn_details <-
                        DatabaseConnector::createConnectionDetails(
                                dbms = "postgresql",
                                user = user,
                                password = password,
                                server = server,
                                port = port,
                                extraSettings = extraSettings,
                                oracleDriver = oracleDriver,
                                connectionString = connectionString,
                                pathToDriver = system.file(package = "pg13","driver"))

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
#' @family connection functions

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
#' @family connection functions

connect_ff <-
        function() {
                "dummy"
        }


#' Connect without Console Messages
#' @keywords internal
#' @export
#' @family connection functions

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
#' @rdname dc0
#'
#' @importFrom DatabaseConnector dbDisconnect
#'
#' @export
#' @family connection functions

dc0 <-
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



#' @title
#' Disconnect a Connection
#'
#' @description
#' Disconnect a Postgres Connection object with the option of removing the object from the parent environment.
#'
#' @inheritParams args
#' @param verbose If TRUE, returns a console message when the connection has been closed.
#' @param ...           Additional arguments passed to \code{\link[DatabaseConnector]{dbDisconnect}}.
#' @param remove        If TRUE, the Connection object argument is removed from the parent environment.
#'
#' @rdname dc
#'
#' @importFrom DatabaseConnector dbDisconnect
#'
#' @export
#' @family connection functions

dc <-
        function(conn,
                 verbose = TRUE,
                 remove = FALSE) {


                db_name <-
                        tryCatch(
                          get_conn_db(conn = conn),
                          error = function(e) "Error"
                                )


                if (identical(x = db_name,
                              y = "Error")) {

                        if (verbose) {secretary::typewrite("Postgres connection was already closed")}

                } else {

                        DatabaseConnector::dbDisconnect(conn = conn)

                        if (verbose) {
                                secretary::typewrite(sprintf("Postgres connection to '%s' closed", db_name))
                        }

                }

                if (remove) {

                        rm(list = deparse(substitute(conn)), envir = parent.frame())

                }
        }
