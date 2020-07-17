#' Drop table in the given database
#' @param table_name name of new table
#' @param dbname name of database for new table
#' @param schema name of schema for new table
#' @importFrom DBI dbRemoveTable
#' @importFrom DBI dbDisconnect
#' @export

drop_table <-
        function(
                 table_name,
                 dbname,
                 schema = NULL)  {
                
                conn <- local_conn(dbname = dbname)
                
                DBI::dbRemoveTable(conn = conn,
                                  name = table_name)
                
                DBI::dbDisconnect(conn)
        }