#' Get Full Table
#' @export


getTable <-
        function(conn,
                 schema,
                 tableName) {

                query(conn = conn,
                      buildQuery(schema = schema,
                           tableName = tableName))
        }
