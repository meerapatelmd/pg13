#' Create a Schema
#' @export

createSchema <-
        function(conn,
                 schema) {

                send(conn = conn,
                     renderCreateSchema(schema = schema))
        }
