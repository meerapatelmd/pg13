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
#' @rdname summarizeSchema
#' @export
#' @importFrom purrr map
#' @importFrom tibble as_tibble_col
#' @importFrom dplyr bind_rows mutate

summarizeSchema <-
        function(conn,
                 schema,
                 resultTableName) {

                        Tables <- lsTables(conn = conn,
                                                      schema = schema)

        output <- list()
        for (i in (length(output)+1):length(Tables)) {

                Table <- Tables[i]

                tableCols <-
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
                        tableCol <- tableCols[j]

                        output_j[[j]] <-
                                query(conn = conn,
                                            sql_statement = buildQuery(fields = tableCol,
                                                                             distinct = TRUE,
                                                                             schema = schema,
                                                                             tableName = Table)) %>%
                                nrow()

                        names(output_j)[j] <- tableCol

                        Sys.sleep(1)

                }

                output[[i]] <-
                        output_j  %>%
                        purrr::map(tibble::as_tibble_col, "DISTINCT_COUNT") %>%
                        dplyr::bind_rows(.id = "FIELD") %>%
                        dplyr::mutate(total_rows = total_rows)

                names(output)[i] <- Table

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
