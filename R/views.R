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
                 viewName,
                 verbose = TRUE,
                 render_sql = TRUE) {

                query(conn = conn,
                      conn_fun = conn_fun,
                      sql_statement = SqlRender::render('SELECT * FROM @schema."@viewName";', schema = schema, viewName = viewName),
                      verbose = verbose,
                      render_sql = render_sql)

        }

#' @title
#' Refresh a Materialized View
#' @rdname refresh_mat_view
#' @export
#' @family materialized view functions

refresh_mat_view <-
        function(conn,
                 schema,
                 matViewName,
                 verbose = TRUE,
                 render_sql = TRUE) {


                send(conn = conn,
                     sql_statement = SqlRender::render('REFRESH MATERIALIZED VIEW @schema."@matViewName"', schema = schema, matViewName = matViewName),
                     verbose = verbose,
                     render_sql = render_sql)

        }
