#' @title
#' Kill All Connections
#'
#' @rdname kill
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @importFrom SqlRender render
#' @export


kill <-
  function(conn,
           conn_fun,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE) {
    send(
      conn = conn,
      conn_fun = conn_fun,
      sql_statement =
        SqlRender::render(
          "SELECT pg_terminate_backend(pg_stat_activity.pid)
                                        FROM pg_stat_activity
                                        WHERE pg_stat_activity.datname = '@dbname'
                                        AND pid <> pg_backend_pid();",
          dbname = getConnDB(conn = conn)
        ),
      verbose = verbose,
      render_sql = render_sql,
      render_only = render_only
    )
  }
