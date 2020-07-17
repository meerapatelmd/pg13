#' Query a local postgres database from a file
#' @param path_to_sql_file path to .sql file
#' @param dbname name of local postgres database
#' @importFrom readr read_file
#' @export

query_pg_from_file <-
        function(path_to_sql_file, dbname, schema = NULL, port = "5432") {
                sql_statement <- readr::read_file(file = path_to_sql_file)
                
                data <- get_pg_query(sql_statement = sql_statement,
                                     dbname = dbname,
                                     schema = schema,
                                     port = port)
                
                return(data)
                
        }
