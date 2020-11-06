#' @title
#' Local Postgres Database Function Family
#'
#' @description
#' Connect and disconnect to a local Postgres database that does not require credentialing. These functions automatically set the dbms and server arguments to "postgresql" and "localhost/{dbname}", leaving the database name (`dbname`) and port (`port`) as the only connection details that are modifiable by the user. Note that these functions cannot be used to connect to a local instance that requires a username and password.
#'
#' @section Local ConnectionDetails:
#' This function returns a connectionDetails class object for functions that make the database connection within its caller environment, such as those belonging to the OHDSI Feature Extraction package. A connection can be made by passing the connectionDetails class object to the \code{\link[DatabaseConnector]{connect}}. This process can be simplified into a single function call with \code{\link{localConnect}}, which performs both operations but will not return a connectionDetails object if it is desired as a return value.
#'
#' @section Local Connection:
#' This function combines the process of creating a connectionDetails object and making the connection in a single function call. If a connectionDetails object is required, see \code{\link{makeLocalConnDetails}}.
#'
#' @param dbname        Name of a local Postgres database, Default: 'athena'
#' @param port          The port on the server to connect to, Default: 5432
#'
#' @name local_db_family
#' @export
NULL


#' @title
#' Make a local Postgres Connection Details object
#'
#' @inherit local_db_family description
#' @inheritSection local_db_family Local ConnectionDetails
#' @inheritParams local_db_family
#' @return
#' A connectionDetails class object that lists all the details needed to connect to a database.
#' @seealso
#'  \code{\link[DatabaseConnector]{createConnectionDetails}}
#' @rdname makeLocalConnDetails
#' @family local_db
#' @export
#' @importFrom DatabaseConnector createConnectionDetails

makeLocalConnDetails <-
    function(dbname = "athena",
             port = 5432) {

                dbms     <- "postgresql"
                server   <- paste0("localhost/", dbname)

                DatabaseConnector::createConnectionDetails(dbms  = dbms,
                                                           server   = server,
                                                           port     = port)
    }


#' @title
#' Connect to a Local Postgres Database
#'
#' @inherit local_db_family description
#' @inheritSection local_db_family Local Connection
#' @inheritParams local_db_family
#' @return
#' A connection class object to the database.
#' @seealso
#'  \code{\link[DatabaseConnector]{connect}}
#' @rdname localConnect
#' @family local_db
#' @export
#' @importFrom DatabaseConnector connect

localConnect <-
        function(dbname = "athena",
                 port = 5432) {

                conn_details <- makeLocalConnDetails(dbname = dbname,
                                                     port = port)
                DatabaseConnector::connect(conn_details)
        }
