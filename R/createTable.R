



#' @export

createTable <-
        function(conn,
                 conn_fun,
                 schema,
                 tableName,
                 if_not_exists = TRUE,
                 ...,
                 verbose = TRUE,
                 render_sql = TRUE) {


                ddl <- list(...)

                fields <- names(ddl)

                ddl <- mapply(paste, fields, ddl, collapse = " ")
                ddl <- paste(ddl, collapse = ",\n")

                if (if_not_exists) {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                        CREATE TABLE IF NOT EXISTS @schema.@tableName (
                                                @ddl
                                        );
                                        ",
                                        schema = schema,
                                        tableName = tableName,
                                        ddl = ddl
                                )

                } else {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                        CREATE TABLE @schema.@tableName (
                                                @ddl
                                        );
                                        ",
                                        schema = schema,
                                        tableName = tableName,
                                        ddl = ddl
                                )


                }

                pg13::send(
                        conn = conn,
                        sql_statement = sql_statement,
                        verbose = verbose,
                        render_sql = render_sql
                )

        }
