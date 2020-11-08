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
#' @name connection
NULL

#' @title
#' Connect to a Postgres Database
#' @param user PARAM_DESCRIPTION
#' @param password PARAM_DESCRIPTION
#' @param port PARAM_DESCRIPTION
#' @param server PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[DatabaseConnector]{createConnectionDetails}},\code{\link[DatabaseConnector]{connect}}
#' @rdname connDB
#' @export
#' @importFrom DatabaseConnector createConnectionDetails connect


connDB <-
        function(user,
                 password,
                 port,
                 server) {

                conn_details <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                           user = user,
                                                                           password = password,
                                                                           port = port,
                                                                           server = server)

                DatabaseConnector::connect(conn_details)
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname connect
#' @export

connect <-
        function() {
                "dummy"
        }


#' @title
#' Connection FF
#' @rdname connect_ff
#' @export

connect_ff <-
        function() {
                "dummy"
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export

quietly.connDB <-
        function() {
                "dummy"
        }

#' @title
#' Disconnect a Connection
#'
#' @description
#' Disconnect a Postgres Connection object with the option of removing the object from the parent environment.
#'
#' @param conn          Connection object
#' @param ...           Additional arguments passed to \code{\link[DatabaseConnector]{dbDisconnect}}.
#' @param remove        If TRUE, the Connection object argument is removed from the Parent Environment.
#'
#' @rdname dc
#'
#' @importFrom DatabaseConnector dbDisconnect
#'
#' @export

dc <-
        function(conn,
                 ...,
                 remove = FALSE) {

                DatabaseConnector::dbDisconnect(conn = conn,
                                                ...)

                if (remove) {

                        rm(list = deparse(substitute(conn)), envir = parent.frame())

                }
        }






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

                .Deprecated(new = "is_conn_open")

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
#' (Deprecated) This function removes a connection object from the Global Environment if it is closed.
#'
#' @param conn Postgres connection object
#'
#' @rdname rmIfClosed
#' @export

rmIfClosed <-
        function(conn) {

                .Deprecated(new = "rm_if_closed")

                results <- isClosed(conn = conn)

                if (results == TRUE) {

                        rm(list = deparse(substitute(conn)), envir = globalenv())

                }

        }


#' @title
#' Remove a Closed Connection Object
#'
#' @description
#' If a connection is not open, it is removed from the parent environment from which this function is called.
#'
#' @param conn Postgres connection object
#'
#' @rdname rm_if_closed
#' @export


rm_if_closed <-
        function(conn) {

                if (!is_conn_open(conn = conn)) {

                        rm(list = deparse(substitute(conn)), envir = parent.frame())

                }


        }
