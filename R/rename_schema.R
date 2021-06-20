#' @title
#' Rename a Schema
#'
#' @inheritParams args
#' @param schema Schema to rename.
#' @param new_schema_name New schema name.
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname rename_schema
#' @export
#' @importFrom SqlRender render
rename_schema <-
        function(conn,
                 conn_fun,
                 schema,
                 new_schema_name,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

        send(
        conn = conn,
        conn_fun = conn_fun,
        sql_statement =
                SqlRender::render("ALTER SCHEMA @schema RENAME TO @new_schema_name;",
                                  schema = schema,
                                  new_schema_name = new_schema_name),
        verbose = verbose,
        render_sql = render_sql,
        render_only = render_only
        )
}
