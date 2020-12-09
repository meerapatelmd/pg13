create_log <-
        function(datetime_field,
                 conn,
                 schema,
                 log_table,
                 verbose = TRUE,
                 render_sql = TRUE) {

                sql_statement <-
                        SqlRender::render(
                                "CREATE TABLE IF NOT EXISTS @schema.@log_table (
                                                @datetime_field timestamp without time zone,
                                                activity text,
                                                status   varchar(25)
                                );",
                                schema = schema,
                                log_table = log_table,
                                datetime_field = datetime
                        )

                pg13::send(conn = conn,
                           sql_statement = sql_statement,
                           verbose = verbose,
                           render_sql = render_sql)

        }

log_start_ff <-
        function(datetime_field,
                 schema,
                 log_table) {


                function(activity,
                         conn,
                         verbose = TRUE,
                         render_sql = TRUE) {

                        create_log(datetime_field = datetime_field,
                                   conn = conn,
                                   schema = schema,
                                   log_table = log_table,
                                   verbose = verbose,
                                   render_sql = render_sql)


                        output <-
                        tibble::tibble(activity = activity,
                                       status = "start") %>%
                                dplyr::mutate({{ as.symbol(datetime_field) }} := Sys.time()) %>%
                                dplyr::select(dplyr::all_of(datetime_field),
                                             activity,
                                             status)

                        appendTable(conn = conn,
                                    schema = schema,
                                    tableName = log_table,
                                    data = output)

                }

        }


log_stop_ff <-
        function(datetime_field,
                 schema,
                 log_table) {


                function(activity,
                         conn,
                         verbose = TRUE,
                         render_sql = TRUE) {

                        create_log(datetime_field = datetime_field,
                                   conn = conn,
                                   schema = schema,
                                   log_table = log_table,
                                   verbose = verbose,
                                   render_sql = render_sql)


                        output <-
                                tibble::tibble(activity = activity,
                                               status = "stop") %>%
                                dplyr::mutate({{ as.symbol(datetime_field) }} := Sys.time()) %>%
                                dplyr::select(dplyr::all_of(datetime_field),
                                              activity,
                                              status)

                        appendTable(conn = conn,
                                    schema = schema,
                                    tableName = log_table,
                                    data = output)

                }

        }
