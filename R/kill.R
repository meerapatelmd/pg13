#' @title
#' Kill All Connections
#'
#' @rdname kill
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @importFrom SqlRender render


kill <-
        function(conn,
                 verbose = TRUE,
                 render_sql = TRUE) {

                send(conn = conn,
                     sql_statement =
                             SqlRender::render(
                                        "SELECT pg_terminate_backend(pg_stat_activity.pid)
                                        FROM pg_stat_activity
                                        WHERE pg_stat_activity.datname = '@dbname'
                                        AND pid <> pg_backend_pid();",
                                        dbname = getConnDB(conn = conn)
                                        ),
                     verbose = verbose,
                     render_sql = render_sql
                )
        }