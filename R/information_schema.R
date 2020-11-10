





dataTypeInfo <-
        function(conn,
                 schema,
                 tableName,
                 render_sql = TRUE) {


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

                query(
                        conn = conn,
                        sql_statement = sql_statement,
                        render_sql = render_sql)

        }
