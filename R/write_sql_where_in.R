#' Writes a SQL query that includes a WHERE IN clause based on a vector
#' @param select vector of table_column_names. Defaults to "*".
#' @param table_name name of table
#' @param column_name name of column for WHERE IN clause
#' @param where_in_vector vector of WHERE IN clause
#' @param limit limit count
#' @export

write_query_where_in <-
        function(select = "*", table_name, column_name, where_in_vector, limit = NULL) {
                
                ##Prepare select statement
                select <- prepare_vector(select)
                
                sql_statement <- paste0("SELECT ", select, " FROM ", table_name)
                
                if (is.null(limit)) {
                        sql_statement <- paste0(sql_statement, 
                                                " WHERE ",
                                                column_name, " IN (", paste(paste0("'", where_in_vector,
                                                                                   "'"), collapse = ", "), ");")
                }
                else {
                        sql_statement <- paste0(sql_statement, 
                                                " WHERE ",
                                                column_name, " IN (", paste(paste0("'", where_in_vector,
                                                                                   "'"), collapse = ", "), ")")
                        sql_statement <- paste0(sql_statement, " LIMIT ", limit,
                                                ";")
                }
                return(sql_statement)
        }