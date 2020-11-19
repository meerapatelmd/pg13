#' List Fields
#' @import DatabaseConnector
#' @export


lsFields <-
    function(conn,
             schema,
             tableName,
             verbose = TRUE,
             render_sql = TRUE) {

            if (render_sql) {

                typewrite_sql("N/A")

            }


            if (verbose) {

                typewrite_activity("Listing Fields...")

            }

            resultset <- DatabaseConnector::dbListFields(conn = conn,
                                            name = tableName,
                                            schema = schema)

            if (verbose) {

                typewrite_activity("Listing Fields...complete")

            }

            resultset

    }





#' List Schema
#' @description
#' List all schema in the connection.
#' @importFrom magrittr %>%
#' @export

lsSchema <-
        function(conn) {

                query(conn = conn,
                     renderLsSchema()) %>%
                        unlist() %>%
                        unname()

        }





#' List Tables
#' @import DatabaseConnector
#' @export


lsTables <-
    function(conn,
             schema = NULL) {

            DatabaseConnector::dbListTables(conn = conn,
                                        schema = schema)


    }





