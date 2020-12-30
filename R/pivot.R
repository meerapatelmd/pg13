#' @title
#' Pivot a Table
#'
#' @rdname pivot_table
#' @example inst/example/pivot.R
#' @export

pivot_table <-
        function(conn,
                 conn_fun,
                 schema,
                 table,
                 id_column,
                 names_from_column,
                 values_from_column,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE
                 ) {


                # Get `names_from` values
                new_field_names <-
                query(conn = conn,
                      conn_fun = conn_fun,
                      sql_statement =
                                SqlRender::render(
                                        "
                                        SELECT DISTINCT @names_from_column
                                        FROM @schema.@table
                                        ",
                                        names_from_column = names_from_column,
                                        schema = schema,
                                        table = table
                                ),
                      verbose = verbose,
                      render_sql = render_sql,
                      render_only = render_only) %>%
                        unlist() %>%
                        sort()


                # <---! Value Type of the "Valuees From" column
                values_from_data_type <-
                        query(conn = conn,
                              conn_fun = conn_fun,
                                sql_statement =
                                        SqlRender::render(
                                                "
                                                SELECT
                                                        udt_name as data_type
                                                FROM information_schema.columns
                                                WHERE table_name = '@tableName'
                                                        AND column_name = '@values_from_column'
                                                ",
                                                tableName = table,
                                                values_from_column = values_from_column),
                              verbose = verbose,
                              render_sql = render_sql,
                              render_only = render_only) %>%
                        unlist()


                new_field_names_ddl <-
                        sprintf("%s %s", new_field_names, values_from_data_type)



                sql_statement <-
                        SqlRender::render(
                        "
                        SELECT *
                                FROM crosstab( 'select @id_column, @names_from_column, @values_from_column from @schema.@table ORDER BY 1,2')
                        AS final_result(@id_column VARCHAR, @new_field_names_ddl)
                        ",
                        id_column = id_column,
                        new_field_names_ddl = new_field_names_ddl,
                        values_from_column = values_from_column,
                        names_from_column = names_from_column,
                        schema = schema,
                        table = table
                        )

                query(conn = conn,
                      conn_fun = conn_fun,
                      sql_statement = sql_statement,
                      verbose = verbose,
                      render_sql = render_sql,
                      render_only = render_only)
        }
