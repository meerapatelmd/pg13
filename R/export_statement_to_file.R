#' Export a sql statement within a R object to a sql file
#' @param sql_statement sql statement R object
#' @param path_to_sql_file sql file path to output. A sql extension is added if it is not present in the input.
#' @importFrom readr write_lines
#' @importFrom cave strip_fn
#' @export

export_statement_to_file <-
        function(sql_statement, path_to_sql_file) {
                ##Making sure extension is sql
                path_to_sql_file <- cave::strip_fn(path_to_sql_file, rm_ext = TRUE, rm_path = FALSE)
                
                readr::write_lines(sql_statement, 
                                   path = path_to_sql_file)
        }