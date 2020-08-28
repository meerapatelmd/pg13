#' @title
#' Grant All Privileges to a Schema
#' @description
#' Grant all privileges to a schema to either a group or a user.
#' @param conn PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param user PARAM_DESCRIPTION, Default: NULL
#' @param group PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @rdname grantSchema
#' @export

grantSchema <-
        function(conn,
                 schema,
                 user = NULL,
                 group = NULL) {

                sql_statement <-
                        renderGrantSchema(schema = schema,
                                          group = group,
                                          user = user)


                send(conn = conn,
                     sql_statement = sql_statement)

        }
