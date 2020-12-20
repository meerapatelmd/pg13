summarize_field <-
        function(conn,
                 conn_fun,
                 schema,
                 table,
                 field,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 warn_no_rows = TRUE,
                 ...) {


                field <- tolower(field)
                if (!field_exists(
                                conn = conn,
                                schema = schema,
                                table = table,
                                field = field)) {

                        stop(sprintf("field '%s' does not exist in %s.%s", field, schema, table))

                }

                sql_statements <-
                        list(DISTINCT_COUNT = sprintf("WITH valueset AS (SELECT DISTINCT %s FROM %s.%s) SELECT COUNT(*) FROM valueset;", field, schema, table),
                             VALUESET = sprintf("SELECT DISTINCT %s FROM %s.%s;", field, schema, table),
                             NULL_COUNT = sprintf("SELECT COUNT(%s) FROM %s.%s WHERE %s IS NULL;", field, schema, table, field)
                             )

                output <- list()
                for (i in seq_along(sql_statements)) {

                        sql_statement <- sql_statements[[i]]

                        output[[i]] <-
                                query(conn = conn,
                                      conn_fun = conn_fun,
                                      sql_statement = sql_statement,
                                      verbose = verbose,
                                      render_sql = render_sql,
                                      warn_no_rows = warn_no_rows,
                                      render_only = render_only,
                                      ...)

                        names(output)[i] <- names(sql_statements)[i]

                }

                output

        }


summarize_fields <-
        function(conn,
                 conn_fun,
                 schema,
                 table,
                 fields,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 warn_no_rows = TRUE,
                 ...) {

                output <- list()
                for (field in fields) {

                        output[[length(output)+1]] <-
                                summarize_field(conn = conn,
                                                conn_fun = conn_fun,
                                                schema = schema,
                                                table = table,
                                                field = field,
                                                verbose = verbose,
                                                render_sql = render_sql,
                                                render_only = render_only,
                                                warn_no_rows = warn_no_rows,
                                                ...)

                        names(output)[length(output)] <- field

                }

                output

        }


summarize_table <-
        function(
                conn,
                conn_fun,
                schema,
                table,
                verbose = TRUE,
                render_sql = TRUE,
                render_only = FALSE,
                warn_no_rows = TRUE,
                ...
        ) {

                fields <- ls_fields(conn = conn,
                                    schema = schema,
                                    table = table,
                                    verbose = verbose,
                                    render_sql = render_sql)

                summarize_fields(conn = conn,
                                 conn_fun = conn_fun,
                                 schema = schema,
                                 table = table,
                                 fields = fields,
                                 verbose = verbose,
                                 render_sql = render_sql,
                                 render_only = render_only,
                                 warn_no_rows = warn_no_rows,
                                 ...)
        }


summarize_schema <-
        function( conn,
                  conn_fun,
                  schema,
                  verbose = TRUE,
                  render_sql = TRUE,
                  render_only = FALSE,
                  warn_no_rows = TRUE,
                  ...) {


                tables <- ls_tables(conn = conn,
                                    schema = schema,
                                    verbose = verbose,
                                    render_sql = render_sql)

                output <- list()
                for (i in seq_along(tables)) {

                        table <- tables[i]

                        output[[i]] <-
                                summarize_table(conn = conn,
                                                schema = schema,
                                                table = table,
                                                verbose = verbose,
                                                render_sql = render_sql,
                                                render_only = render_only,
                                                warn_no_rows = warn_no_rows,
                                                ...)

                        names(output)[i] <- table
                }

                output
        }
