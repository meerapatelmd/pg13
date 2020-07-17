#' Create a conn object on a local database
#' @description If the database is for the local UMLS, a MySQL connection is made. Otherwise, the connect is made as a local postgres database.
#' @param dbname name of database
#' @param schema optional and applies to postgres only if used.
#' @export

local_conn <-
    function(dbname, schema = NULL) {
                    if (dbname != "umls") {
                        
                        conn <- connect_to_local_postgres(dbname = dbname, 
                                                          schema = schema)
                        
                    } else {
                        
                        conn <- connect_to_mysql5.5(dbname = dbname)
                    }
        
                    return(conn)
    }
