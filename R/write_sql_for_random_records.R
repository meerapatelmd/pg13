#' Writes SQL to get random records of a certain limit
#' @param select character vector of column names for SELECT statement
#' @param table_name name of table, including schema if applicable
#' @param limit value LIMIT is assigned to in the sql statement
#' @return character string of length one of a sql statement that queries a table for a desired number of random records
#' @export


write_sql_for_random_records <-
        function(table_name, limit = 1000, select = "*") {
                
                ##Prepare select statement
                select <- prepare_vector(select)
                
                sql_statement <- paste0("SELECT ", select, " FROM ", table_name)
                
                sql_statement <- paste0(sql_statement, " ",
                                        "ORDER BY RANDOM() ",
                                        "LIMIT ", limit, ";")
                
                return(sql_statement)
        }