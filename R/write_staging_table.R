#' @title
#' Write a Staging Table.
#'
#' @description
#' A `Staging Table`` is one that is named automatically in the "V{Timestamp}" format and is dropped on exit in the parent frame from which the function is being called.
#'
#' @export
#' @rdname write_staging_table
#' @inheritParams args

write_staging_table <-
        function(conn,
                 conn_fun,
                 schema,
                 data,
                 drop_existing = FALSE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 ...) {

                        table_name <-
                        sprintf("V%s",
                                stringr::str_remove_all(as.character(Sys.time()),
                                                pattern = "[^0-9]"))
                        on.exit(return(table_name))


                write_table(conn = conn,
                           conn_fun = conn_fun,
                           schema = schema,
                           tableName = table_name,
                           data = data,
                           drop_existing = drop_existing,
                           verbose = verbose,
                           render_sql = render_sql,
                           ... = ...)



        }
