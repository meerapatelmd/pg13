#' @export

read_view <-
        function(conn,
                 schema,
                 viewName,
                 verbose = TRUE,
                 render_sql = TRUE) {

                query(conn = conn,
                      sql_statement = SqlRender::render('SELECT * FROM @schema."@viewName";', schema = schema, viewName = viewName),
                      verbose = verbose,
                      render_sql = render_sql)

        }

#' @export

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
