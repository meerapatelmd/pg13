#' Create schema for a database
#' @description This function connects, creates a new schema after dropping one of it exists, and disconnects.
#' @param dbname name of postgres database
#' @param new_schema name of new schema
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbDisconnect
#' @export

create_schema <-
        function(dbname, new_schema) {
                conn <- local_conn(dbname = dbname)
                
                sql_statement <- write_sql_to_create_schema(schema = new_schema)
                DBI::dbSendStatement(conn = conn,
                                     statement = sql_statement)
                DBI::dbDisconnect(conn)
        }
