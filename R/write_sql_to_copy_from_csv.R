#' Writes a dataframe as a table to the given database
#' @import readr
#' @export

write_sql_to_copy_from_csv <-
        function(csv_fn,
                 to_table_name) {

                sql_statement <- paste0("COPY ", to_table_name, " FROM '", csv_fn, "' DELIMITER ',';")
                return(sql_statement)
                
        }