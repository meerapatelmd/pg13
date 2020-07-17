#' Write sql that gets distinct counts
#' @param select character vector of column names to call the DISTINCT function on
#' @param table_name name of table with schema if applicable
#' @return string of length one of a complete sql statement that will return distinct counts when executed
#' @export


write_sql_for_distinct_counts <-
        function(table_name, select = "*") {
                
                                ##Prepare select statement
                                select <- prepare_vector(select)
                                
                                sql_statement <- paste0("SELECT DISTINCT COUNT ", select, " FROM ", table_name, ";")
                                
                                return(sql_statement)
        }
