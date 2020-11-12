#' Parse SQL into Single Statements
#' @description
#' This function is helpful in splitting a SQL file into digestible smaller statements based on the semicolon for troubleshooting or to skip of the statements that do not work within the full input SQL.
#' @import centipede
#' @return
#' List of sql statements
#' @export

parseSQL <-
        function(sql_statement) {
                centipede::strsplit(sql_statement, split = "[\\;]{1}", type = "after") %>%
                        unlist() %>%
                        trimws() %>%
                        centipede::no_blank() %>%
                        as.list()
        }





#' Export a sql statement to a file
#' @param sql_statement sql statement R object
#' @param file File to write to.
#' @param ... Additional arguments passed to the readr::write_lines function
#' @importFrom readr write_lines
#' @export

saveSQL <-
        function(sql_statement,
                 file,
                 append = TRUE,
                 ...) {

                .Deprecated("write_sql_file")

                readr::write_lines(x = sql_statement,
                                   path = file,
                                   append = append,
                                   ...
                                   )
        }





