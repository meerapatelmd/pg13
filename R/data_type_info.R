#' @export
#' @rdname data_type_info
#' @title
#' Get Field Datatypes
#' @importFrom SqlRender::render
#' @example inst/example/data_type_info.R

data_type_info <-
        function(conn,
                 conn_fun,
                 table,
                 sql_statement,
                 verbose = TRUE,
                 render_sql = TRUE,
                 warn_no_rows = TRUE,
                 render_only = FALSE,
                 ...) {


                sql_statement <-
                        SqlRender::render(
                                "
                                SELECT
                                        column_name as field,
                                        udt_name as data_type
                                FROM information_schema.columns
                                WHERE table_name = '@tableName'
                                ",
                                tableName = table
                        )

                query(conn = conn,
                      conn_fun = conn_fun,
                      sql_statement = sql_statement,
                      verbose = verbose,
                      render_sql = render_sql,
                      warn_no_rows = warn_no_rows,
                      render_only = render_only,
                      ... )

        }
