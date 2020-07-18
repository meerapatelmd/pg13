#' Write a SQL statement that loops over a word in a string
#' @export

buildQueryString <-
        function(string,
                 split,
                 fields = "*",
                 schema,
                 tableName,
                 whereLikeField,
                 string,
                 split,
                 caseInsensitive = TRUE,
                 limit_n = NULL) {


                sql_construct <-
                constructBase(fields = fields,
                              schema = schema,
                              tableName = tableName)

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

