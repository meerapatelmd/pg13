#' @title
#' Peek
#' @description
#' Get a quick glance of a handful of rows in a table.
#' @inheritParams args
#' @param limit Number of rows to randomly return. Default: 5.
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname peek
#' @export
#' @importFrom SqlRender render



peek <-
  function(conn,
           conn_fun,
           schema,
           table,
           limit = 5,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE) {
    query(
      conn = conn,
      conn_fun = conn_fun,
      sql_statement =
        SqlRender::render(
          "
                                        SELECT *
                                        FROM @schema.@table
                                        ORDER BY RANDOM()
                                        LIMIT @limit;
                                        ",
          schema = schema,
          table = table,
          limit = limit
        ),
      verbose = verbose,
      render_sql = render_sql,
      render_only = render_only
    )
  }
