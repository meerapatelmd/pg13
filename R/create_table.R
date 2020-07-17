#' Create table in the given database
#' @description This function connects, writes a dataframe to a table using the DBI::dbWriteTable function, and disconnects.
#' @param dataframe data to write to table
#' @param table_name name of new table
#' @param dbname name of database for new table
#' @param schema name of schema for new table
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbDisconnect
#' @export

create_table <-
        function(dataframe,
                 table_name,
                 dbname,
                 schema = NULL)  {
            
            conn <- local_conn(dbname = dbname)
            
                DBI::dbWriteTable(conn = conn,
                                  name = table_name,
                                  value = dataframe)
                
                DBI::dbDisconnect(conn)
        }