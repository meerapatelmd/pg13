#' Make a local Postgres Connection Details object
#' @description This function makes a Connection Details object required to use other R packages that execute functions on a Postgres instance, such as the OHDSI Feature Extraction package. Most of the internal packages have collapsed the two-step process of 1) making a Connection Details object and then 2) connecting to it using the localConnect function, for example, because in outside cases Database Management System needs to be derived from the Connection Details object to translate SQL to certain dialects.
#' @import DatabaseConnector
#' @export


makeLocalConnDetails <-
    function(dbname = "athena",
             port = 5432) {

                dbms     <- "postgresql"
                server   <- paste0("localhost/", dbname)

                DatabaseConnector::createConnectionDetails(dbms  = dbms,
                                                           server   = server,
                                                           port     = port)
    }
