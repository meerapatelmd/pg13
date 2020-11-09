#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropCascade <-
        function(conn,
                 schema,
                 ...) {


                sql_statement <- SqlRender::render("DROP SCHEMA @schema CASCADE;", schema = schema)

                send(conn = conn,
                     sql_statement = sql_statement,
                     ...)

        }


#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropIfExists <-
        function(conn,
                 schema,
                 ...) {


                sql_statement <- SqlRender::render("DROP SCHEMA IF EXISTS @schema;", schema = schema)

                send(conn = conn,
                     sql_statement = sql_statement,
                     ...)

        }


#' Create a Schema
#' @export

createSchema <-
        function(conn,
                 schema) {

                send(conn = conn,
                     SqlRender::render(
                             "CREATE SCHEMA @schema;",
                             schema = schema
                     ))
        }


