#' @title
#' Get Connection Database
#'
#' @description
#' Get the name of the database that the connection object is connected to.
#'
#' @export
#' @rdname get_conn_db

get_conn_db <-
        function(conn) {
                conn@jConnection$getCatalog()

        }





#' @export
#' @rdname query_field_type
#' @title
#' Get Field Datatypes
#' @importFrom SqlRender render
#' @example inst/example/data_type_info.R

query_field_type <-
        function(conn,
                 conn_fun,
                 table,
                 sql_statement,
                 verbose = TRUE,
                 render_sql = TRUE,
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
                      render_only = render_only,
                      ... )

        }





