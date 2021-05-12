#' @title
#' Read View
#' @rdname read_view
#' @export
#' @family view functions
#' @family read functions

read_view <-
  function(conn,
           conn_fun,
           schema,
           view,
           verbose = TRUE,
           render_sql = TRUE) {
    query(
      conn = conn,
      conn_fun = conn_fun,
      sql_statement = SqlRender::render('SELECT * FROM @schema."@viewName";', schema = schema, viewName = view),
      verbose = verbose,
      render_sql = render_sql
    )
  }

#' @title
#' Refresh a Materialized View
#' @rdname refresh_mview
#' @export
#' @family materialized view functions

refresh_mview <-
  function(conn,
           schema,
           mview,
           verbose = TRUE,
           render_sql = TRUE) {
    send(
      conn = conn,
      sql_statement = SqlRender::render('REFRESH MATERIALIZED VIEW @schema."@matViewName"', schema = schema, matViewName = mview),
      verbose = verbose,
      render_sql = render_sql
    )
  }
