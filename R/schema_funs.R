#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

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

#' @title
#' Grant All Privileges to a Schema
#' @description
#' Grant all privileges to a schema to either a group or a user.
#' @param conn PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param user PARAM_DESCRIPTION, Default: NULL
#' @param group PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @rdname grant_schema
#' @export

grant_schema <-
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







#' @export

table_data_type_info <-
        function(conn,
                 conn_fun,
                 tableName,
                 sql_statement,
                 verbose = TRUE,
                 render_sql = TRUE,
                 warn_no_rows = TRUE,
                 render_only = FALSE,
                 ...) {


                sql_statement <-
                        SqlRender::render(
                                "
                                SELECT
                                        column_name as field,
                                        udt_name as data_type
                                FROM information_schema.columns
                                WHERE table_name = '@tableName'
                                ",
                                tableName = tableName
                        )

                query(conn = conn,
                      conn_fun = conn_fun,
                      sql_statement = sql_statement,
                      verbose = verbose,
                      render_sql = render_sql,
                      warn_no_rows = warn_no_rows,
                      render_only = render_only,
                      ... )

        }



#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

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







#' @title
#' Summarize a Schema
#'
#' @param conn                  Postgres connection
#' @param schema                Schema to summarize
#' @param resultTableName       Table that the final output will be written to.
#'
#' @return
#' New Table in the same schema
#'
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[tibble]{as_tibble}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}}
#'
#' @rdname summarize_schema
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom tibble as_tibble_col
#' @importFrom dplyr bind_rows mutate

summarize_schema <-
        function(conn,
                 schema,
                 resultTableName) {

                        tables <- ls_tables(conn = conn,
                                                      schema = schema)

        output <- list()
        for (i in (length(output)+1):length(Tables)) {

                table <- tables[i]

                table_cols <-
                        query(conn = conn,
                                    sql_statement = buildQuery(schema = schema,
                                                                     tableName = Table,
                                                                     n = 1,
                                                                     n_type = "limit")) %>%
                        colnames()

                total_rows <-
                        query(conn = conn,
                                    sql_statement = renderRowCount(
                                            schema = schema,
                                            tableName = Table)) %>%
                        unlist()

                output_j <- list()
                for (j in 1:length(tableCols)) {
                        table_col <- table_cols[j]

                        output_j[[j]] <-
                                query(conn = conn,
                                            sql_statement = buildQuery(fields = tableCol,
                                                                             distinct = TRUE,
                                                                             schema = schema,
                                                                             tableName = Table)) %>%
                                nrow()

                        names(output_j)[j] <- table_col

                        Sys.sleep(1)

                }

                output[[i]] <-
                        output_j  %>%
                        purrr::map(tibble::as_tibble_col, "DISTINCT_COUNT") %>%
                        dplyr::bind_rows(.id = "FIELD") %>%
                        dplyr::mutate(total_rows = total_rows)

                names(output)[i] <- table

        }

        final_output <- dplyr::bind_rows(output,
                                         .id = "TABLE")


        dropTable(conn = conn,
                        schema = schema,
                        tableName = resultTableName)

        writeTable(conn = conn,
                         schema = schema,
                         tableName = resultTableName,
                         final_output)
}





