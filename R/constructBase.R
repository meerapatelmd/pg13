#' Construct a base SQL query
#' @description Construct the SELECT {fields} FROM {schema}.{table} base query. The SQL Statement is not terminated for the addition of other parameters generated using "construct" functions.
#' @import SqlRender
#' @export

constructBase <-
    function(fields = "*",
             schema = "public",
             tableName) {

            base <- system.file(package='pg13')
            path <- paste0(base, "/sql_constr")

            SqlRender::render(SqlRender::readSql(paste0(path, "/base.sql")),
                              fields = fields,
                              schema = schema,
                              tableName = tableName)

    }
