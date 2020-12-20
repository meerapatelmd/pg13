#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export
#' @example inst/example/schema.R
#' @rdname drop_schema

drop_schema <-
    function(conn,
             conn_fun,
             schema,
             cascade = FALSE,
             if_exists = TRUE,
             verbose = TRUE,
             render_sql = TRUE,
             render_only = FALSE,
             ...) {


            if (if_exists) {

                if_exists_clause <- "IF EXISTS"

            } else {

                if_exists_clause <- NULL

            }


            if (cascade) {

                cascade_clause <- "CASCADE"

            } else {

                cascade_clause <- NULL

            }


            sql_statement <-
            SqlRender::render(
                "DROP SCHEMA @if_exists_clause @schema @cascade_clause;",
                schema = schema,
                if_exists_clause = if_exists_clause,
                cascade_clause = cascade_clause
            )

            send(conn = conn,
                 conn_fun = conn_fun,
                 sql_statement = sql_statement,
                 verbose = verbose,
                 render_sql = render_sql,
                 render_only = render_only,
                   ...)

    }


#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export
#' @example inst/example/schema.R
#' @rdname drop_cascade

drop_cascade <-
        function(conn,
                 conn_fun,
                 schema,
                 if_exists = TRUE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {

            drop_schema(
                conn = conn,
                conn_fun = conn_fun,
                schema = schema,
                cascade = TRUE,
                if_exists = if_exists,
                verbose = verbose,
                render_sql = render_sql,
                render_only = render_only,
                ...
            )

        }



#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export
#' @example inst/example/schema.R
#' @rdname drop_if_exists

drop_if_exists <-
        function(conn,
                 conn_fun,
                 schema,
                 cascade = FALSE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {


            drop_schema(
                conn = conn,
                conn_fun = conn_fun,
                schema = schema,
                cascade = cascade,
                if_exists = TRUE,
                verbose = verbose,
                render_sql = render_sql,
                render_only = render_only,
                ...
            )

        }


#' Create a Schema
#' @export
#' @rdname create_schema
#' @example inst/example/schema.R

create_schema <-
        function(conn,
                 conn_fun,
                 schema,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {

                send(conn = conn,
                     conn_fun = conn_fun,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only,
                     sql_statement =
                     SqlRender::render(
                             "CREATE SCHEMA @schema;",
                             schema = schema
                     ),
                     ...)
        }
