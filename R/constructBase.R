#' Construct a base SQL query
#' @description Construct the SELECT {fields} FROM {schema}.{table} base query. The SQL Statement is not terminated for the addition of other parameters generated using "construct" functions.
#' @param distinct if TRUE, the DISTINCT fields will be selected for.
#' @import SqlRender
#' @export

constructBase <-
    function(fields = "*",
             distinct = FALSE,
             schema = "public",
             tableName) {

            base <- system.file(package='pg13')
            path <- paste0(base, "/sql_constr")

            if (distinct) {

                    SqlRender::render(SqlRender::readSql(paste0(path, "/distinctBase.sql")),
                                      fields = fields,
                                      schema = schema,
                                      tableName = tableName)


            } else {

                    SqlRender::render(SqlRender::readSql(paste0(path, "/base.sql")),
                                              fields = fields,
                                              schema = schema,
                                              tableName = tableName)

            }

    }
