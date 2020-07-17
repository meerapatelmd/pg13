#' Send a SQL Statement to Postgres
#' @param conn Connection object
#' @param sql_statement SQL to send
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbSendStatement function
#' @import DatabaseConnector
#' @export

pgSend <- 
    function(conn,
             sql_statement,
             ...) {
        
        DatabaseConnector::dbSendStatement(conn = conn,
                                           statement = sql_statement,
                                           ...)
        
    }