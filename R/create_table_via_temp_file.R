#' Writes a dataframe to a new table via a temp file
#' @description This functionalizes a method of loading data into a local database that has worked as opposed to the other methods. To prevent errors in getting the nchar counts for the VARCHAR portion of the SQL statement, all true NAs are converted to "NA" at the beginning of the script.
#' The temp file is a text file and a delimiter can be customized. The temp file is also unlinked at the end of execution. The column names are removed from the temp file before writing to avoid collisions with the column names of the empty table that will be created.
#' @param dataframe dataframe to write to table
#' @param table_name name of the table
#' @param dbname name of database to be written to
#' @param schema optional.
#' @param delimiter delimiter used to write to a file and copy the contents of the file in the the SQL.
#' @importFrom readr write_delim
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbDisconnect
#' @export

create_table_via_temp_file <-
    function(dataframe, table_name, dbname, schema = NULL, delimiter = "\t") {
        #Error if dataframe has 0 rows
        if (nrow(dataframe) == 0) {
            stop("Dataframe has zero rows")
        }
        ##Essential because a real NA throws off counts when writing SQL
        dataframe <-
            dataframe %>%
            dplyr::mutate_all(stringr::str_replace_na)
        
        temp_file <- tempfile(fileext = ".txt")
        readr::write_delim(x = dataframe,
                           path = temp_file,
                           delim = delimiter,
                           col_names = FALSE)
        
        conn <- local_conn(dbname = dbname)
        sql_statement <-
            write_sql_to_drop_create_table_from_df(dataframe = dataframe,
                                              to_table_name = table_name)
        
        DBI::dbSendStatement(conn, statement = sql_statement)
        
        sql_statement <-
            write_sql_to_copy_from_delim(source_fn = temp_file,
                                         to_table_name = table_name,
                                         delimiter = delimiter)
        
        DBI::dbSendStatement(conn, statement = sql_statement)
        
        DBI::dbDisconnect(conn)
        
        unlink(temp_file)
    }