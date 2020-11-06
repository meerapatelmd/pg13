#' Send Query to any Postgres connection
#' @param conn Connection object
#' @param sql_statement SQL Query
#' @param ... Additional arguments to pass onto \code{\link[DatabaseConnector]{dbGetQuery}}
#' @import DatabaseConnector
#' @export

query <-
    function(conn,
             sql_statement,
             render_sql = TRUE,
             ...) {

            if (render_sql) {
                    typewrite_sql(sql_statement = sql_statement)
            }

            DatabaseConnector::dbGetQuery(conn,
                                              statement = sql_statement,
                                              ...)

    }
