#' Drop schema for a database
#' @param dbname name of postgres database
#' @param schema name of schema to drop
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbDisconnect
#' @export

drop_schema <-
        function(dbname, schema) {
                conn <- connect_to_local_postgres(dbname)
                
                sql_statement <- write_sql_to_drop_schema(schema = schema)
                DBI::dbSendStatement(conn = conn,
                                     statement = sql_statement)
                DBI::dbDisconnect(conn)
        }
