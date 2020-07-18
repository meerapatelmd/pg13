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

                readr::write_lines(x = sql_statement,
                                   path = file,
                                   append = append,
                                   ...
                                   )
        }
