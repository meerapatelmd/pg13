#' Create a new Database
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

createDB <-
    function(conn,
             newDB,
             ...) {


            sql_statement <- renderCreateDB(newDB = newDB)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

    }

