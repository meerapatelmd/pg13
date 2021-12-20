#' @title
#' List Extensions
#' @description
#' List all the installed extensions.
#' @return
#' Vector of extension names
#' @rdname ls_extensions
#' @export


ls_extensions <-
        function(conn,
                 conn_fun = "pg13::local_connect()",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 checks = "") {

                query(
                        conn = conn,
                        conn_fun = conn_fun,
                        sql_statement = "SELECT extname FROM pg_extension;",
                        verbose = verbose,
                        render_sql = render_sql,
                        render_only = render_only,
                        checks = checks
                ) %>%
                        unlist() %>%
                        unname()


        }
