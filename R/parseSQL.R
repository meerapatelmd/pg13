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
