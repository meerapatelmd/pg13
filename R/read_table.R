#' Read table in the given database
#' @param table_name name of new table
#' @param dbname name of database for new table
#' @param schema name of schema for new table
#' @importFrom DBI dbReadTable
#' @importFrom DBI dbDisconnect
#' @export

read_table <-
        function(
                 table_name,
                 dbname,
                 schema)  {
                
                conn <- connect_to_local_postgres(dbname = dbname, 
                                                  schema = schema)
                
                DBI::dbReadTable(conn = conn,
                                  name = table_name)
                
                DBI::dbDisconnect(conn)
        }