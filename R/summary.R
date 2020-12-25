


summarize_table <-
        function(conn,
                 schema,
                 table,
                 verbose = TRUE,
                 render_sql = TRUE,
                 warn_no_rows = TRUE,
                 render_only = FALSE,
                 ...) {

                # schema <- "test_schema"
                # table <- "test_table"

                fields <-
                        ls_fields(conn = conn,
                                  schema = schema,
                                  table = table,
                                  verbose = verbose,
                                  render_sql = render_sql)

                table_rows <-
                        query(
                                conn = conn,
                                sql_statement =
                                        SqlRender::render("SELECT COUNT(*)\nFROM @schema.@tableName;",
                                                          schema = schema,
                                                          fields = fields,
                                                          tableName = table),
                                verbose = verbose,
                                render_sql = render_sql,
                                render_only = render_only,
                                warn_no_rows = warn_no_rows,
                                ...)


                output2 <- list()
                for (field in fields) {


                        sql_01_distinct_value_ct <-
                                SqlRender::render("WITH a AS (SELECT DISTINCT @field FROM @schema.@table)\nSELECT COUNT(*) FROM a;", field = field, schema = schema, table = table)


                        sql_02_distinct_values <-
                                SqlRender::render("SELECT DISTINCT @field FROM @schema.@table;", field = field, schema = schema, table = table)

                        sql_03_missingness_null <-
                                SqlRender::render("SELECT COUNT(@field) FROM @schema.@table WHERE @field IS NULL;", field = field, schema = schema, table = table)


                        sql_04_missingness_na <-
                                SqlRender::render("SELECT COUNT(@field) FROM @schema.@table WHERE @field IN ('NA');", field = field, schema = schema, table = table)

                        sql_statements <-
                                c(sql_01_distinct_value_ct,
                                  sql_02_distinct_values,
                                  sql_03_missingness_null,
                                  sql_04_missingness_na)

                        names(sql_statements) <-
                                c("DISTINCT_COUNT",
                                  "VALUESET",
                                  "NULL_COUNT",
                                  "NA_COUNT")


                        output <- list()
                        for (sql_statement in sql_statements) {

                                output[length(output)+1] <-
                                        query(
                                                conn = conn,
                                                sql_statement = sql_statement,
                                                verbose = verbose,
                                                render_sql = render_sql,
                                                render_only = render_only,
                                                warn_no_rows = warn_no_rows,
                                                ...)


                                names(output)[length(output)] <- names(sql_statement)

                        }



                }

        }




#' @title
#' Summarize a Schema's Row Counts
#' @rdname nrow_schema
#' @export
#'
#' @importFrom purrr map
#' @importFrom tibble as_tibble_col
#' @importFrom dplyr bind_rows mutate

nrow_schema <-
        function(conn,
                 schema,
                 verbose = TRUE,
                 render_sql = TRUE) {

                tables <- ls_tables(conn = conn,
                                    schema = schema)



                output <- list()

                for (table in tables) {

                        fields <-
                                ls_fields(conn = conn,
                                          schema = schema,
                                          table = table,
                                          verbose = verbose,
                                          render_sql = render_sql)

                        sql_01_table_rows <-
                                SqlRender::render("SELECT COUNT(@fields)\nFROM @schema.@tableName;",
                                                  schema = schema,
                                                  fields = fields,
                                                  tableName = table)


                        for (field in fields) {

                                sql_02_distinct_value_ct <-
                                        SqlRender::render("SELECT DISTINCT COUNT")






                        }

                }

                output <- list()
                for (i in (length(output)+1):length(tables)) {

                        table <- tables[i]

                        table_cols <-
                                query(conn = conn,
                                      sql_statement = buildQuery(schema = schema,
                                                                 tableName = table,
                                                                 n = 1,
                                                                 n_type = "limit")) %>%
                                colnames()

                        total_rows <-
                                query(conn = conn,
                                      sql_statement = renderRowCount(
                                              schema = schema,
                                              tableName = table)) %>%
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

                dplyr::bind_rows(output,
                                 .id = "TABLE")
        }





