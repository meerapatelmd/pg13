#' Write a SQL statement that loops over a word in a string
#' @export

write_sql_query_string_as_vector <-
        function(string,
                 split,
                 select = "*",
                 table_name,
                 column_name,
                 case_insensitive = TRUE,
                 limit = NULL) {

            select <- prepare_vector(select)
            
            if (case_insensitive == FALSE) {
                Args <- strsplit(string, split = split) %>% unlist()
                sql_statement <- paste0("SELECT ", select, " FROM ", table_name, " WHERE ", column_name, " LIKE '%",
                                        Args[[1]], "%'")
                
                Args <- Args[-1]
                
                while (length(Args) > 0) {
                    
                    sql_statement <- paste0(sql_statement, paste0(" AND ", column_name, " LIKE '%",
                                                                  Args[1], "%'"))
                    Args <- Args[-1]
                }
                if (is.null(limit)) {
                    sql_statement <- paste0(sql_statement, ";")
                } else {
                    sql_statement <- paste0(sql_statement, " LIMIT ", limit,
                                            ";")
                }
                return(sql_statement)
            } else {
                Args <- strsplit(string, split = split) %>% unlist()
                Args <- tolower(Args)
                
                sql_statement <- paste0("SELECT ", select, " FROM ", table_name, " WHERE LOWER(", column_name, ") LIKE '%",
                                        Args[[1]], "%'")
                
                Args <- Args[-1]
                
                while (length(Args) > 0) {
                    
                    sql_statement <- paste0(sql_statement, paste0(" AND LOWER(", column_name, ") LIKE '%",
                                                                  Args[1], "%'"))
                    Args <- Args[-1]
                }
                if (is.null(limit)) {
                    sql_statement <- paste0(sql_statement, ";")
                } else {
                    sql_statement <- paste0(sql_statement, " LIMIT ", limit,
                                            ";")
                }
                return(sql_statement)
            }
        }

