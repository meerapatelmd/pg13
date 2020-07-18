#' Terminate a SQL Statement with a semicolon
#' @export

terminateConstruct <-
    function(sql_statement) {

                paste0(sql_statement, ";")

    }
