#' Terminate a SQL Statement with a semicolon
#' @export

terminateBuild <-
    function(sql_statement) {

                paste0(sql_statement, ";")

    }
