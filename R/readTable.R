#' Get Full Table
#' @export


readTable <-
        function(conn,
                 schema,
                 tableName) {

                query(conn = conn,
                      buildQuery(schema = schema,
                           tableName = tableName))
        }
