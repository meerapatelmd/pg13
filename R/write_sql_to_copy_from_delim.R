#' Writes a dataframe as a table to the given database
#' @import readr
#' @export

write_sql_to_copy_from_delim <-
        function(source_fn,
                 to_table_name,
                 delimiter) {

                sql_statement <- paste0("COPY ", to_table_name, " FROM '", source_fn, "' DELIMITER '", delimiter, "';")
                return(sql_statement)
                
        }