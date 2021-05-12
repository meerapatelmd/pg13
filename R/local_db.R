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
#' @name local
#' @family local functions
NULL


#' @title
#' Make a local Postgres Connection Details object
#'
#' @inherit local description
#' @inheritSection local Local ConnectionDetails
#' @inheritParams local
#' @return
#' A connectionDetails class object that lists all the details needed to connect to a database.
#' @seealso
#'  \code{\link[DatabaseConnector]{createConnectionDetails}}
#' @rdname make_local_conn_details
#' @family local functions
#' @export
#' @importFrom DatabaseConnector createConnectionDetails

make_local_conn_details <-
  function(dbname = "athena",
           port = 5432) {
    dbms <- "postgresql"
    server <- paste0("localhost/", dbname)

    DatabaseConnector::createConnectionDetails(
      dbms = dbms,
      server = server,
      port = port,
      pathToDriver = system.file(
        package = "pg13",
        "driver"
      )
    )
  }


#' @title
#' Connect to a Local Postgres Database
#'
#' @inherit local description
#' @inheritSection local Local Connection
#' @inheritParams local
#' @return
#' A connection class object to the database.
#' @seealso
#'  \code{\link[DatabaseConnector]{connect}}
#' @rdname local_connect
#' @family local functions
#' @export
#' @importFrom DatabaseConnector connect

local_connect <-
  function(dbname = "athena",
           port = 5432) {
    "dummy"
  }
