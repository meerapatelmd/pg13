#' Query local Postgess
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbDisconnect
#' @export

get_pg_query <- 
        function (sql_statement, dbname, schema = NULL, port = "5432") {
                conn <- connect_to_local_postgres(dbname = dbname,
                                                  schema = schema,
                                                  port = port)
                data <- DBI::dbGetQuery(conn = conn, statement = sql_statement)
                DBI::dbDisconnect(conn)
                return(data)
        }
