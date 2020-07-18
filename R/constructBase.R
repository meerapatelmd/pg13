#' Construct a base SQL query
#' @description Construct the SELECT {fields} FROM {schema}.{table} base query. The SQL Statement is not terminated for the addition of other parameters generated using "construct" functions.
#' @import SqlRender
#' @export

constructBase <-
    function(fields = "*",
             schema = "public",
             tableName) {


            SqlRender::render(SqlRender::readSql("inst/sql_constr/base.sql"),
                              fields = fields,
                              schema = schema,
                              tableName = tableName)

    }
