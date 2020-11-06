#' @title
#' Local Postgres Database Function Family
#'
#' @description
#' Connect and disconnect to a local Postgres database that does not require credentialing. These functions automatically set the dbms and server arguments to "postgresql" and "localhost/{dbname}", leaving the database name (`dbname`) and port (`port`) as the only connection details that are modifiable by the user. Note that these functions cannot be used to connect to a local instance that requires a username and password.
#'
#' @section
#' Local Connection Details:
#' This function returns a connectionDetails class object for functions that make the database connection within its caller environment, such as those belonging to the OHDSI Feature Extraction package. A connection can be made by passing the connectionDetails class object to the \code{\link{[DatabaseConnector]{connect}}. This process can be simplified into a single function call with \code{\link{localConnect}}, which performs both operations but will not return a connectionDetails object if it is desired as a return value.
#'
#' @section
#' Local Connection:
#' This function combines the process of creating a connectionDetails object and making the connection in a single function call. If a connectionDetails object is required, see \code{\link{makeLocalConnDetails}}.
#'
#' @param dbname        Name of a local Postgres database, Default: 'athena'
#' @param port          The port on the server to connect to, Default: 5432
#'
#' @name local_db
#' @family local_db
#' @export
NULL


#' @title
#' Make a local Postgres Connection Details object
#'
#' @description
#' Creating a connection to a Postgres Database involves first making a connectionDetails object that is a list of the credentials and other parameters to the target database followed by making the connection itself that is stored as connection object.
#'
#' @section
#' Local Connection Details:
#' This function returns a connectionDetails class object for functions that make the database connection within its caller environment, such as those belonging to the OHDSI Feature Extraction package. A connection can be made by passing the connectionDetails class object to the \code{\link{[DatabaseConnector]{connect}}. This process can be simplified into a single function call with \code{\link{localConnect}}, which performs both operations but will not return a connectionDetails object if it is desired as a return value.
#'
#' @section
#' Local Connection:
#' This function combines the process of creating a connectionDetails object and making the connection in a single function call. If a connectionDetails object is required, see \code{\link{makeLocalConnDetails}}.
#'
#'
#' @param dbname Name of a local Postgres database, Default: 'athena'
#' @param port The port on the server to connect to, Default: 5432
#' @return
#' A connectionDetails class object that lists all the details needed to connect to a database.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[DatabaseConnector]{createConnectionDetails}}
#' @rdname makeLocalConnDetails
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


#' Connect to Local PostGres Server
#' @param dbname database name with the corresponding 'localhost/{dbname}' server
#' @param port   defults to "5432"
#' @param schema defaults to NULL
#' @import DatabaseConnector
#' @export

localConnect <-
        function(dbname = "athena",
                 port = 5432) {

                conn_details <- makeLocalConnDetails(dbname = dbname,
                                                     port = port)
                DatabaseConnector::connect(conn_details)
        }
