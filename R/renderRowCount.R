#' Render SQL for a Table Row Count
#' @import SqlRender
#' @param fields
#' @param distinct If TRUE, the distinct row count will be returned.
#' @param schema If NULL, defaults to "public"
#' @export

renderRowCount <-
    function(fields = "*",
             distinct = FALSE,
             schema = NULL,
             tableName) {

        if (is.null(schema)) {
            schema <- "public"
        }

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        if (distinct) {

                SqlRender::render(SqlRender::readSql(paste0(path, "/distinctRowCount.sql")),
                                  fields = fields,
                                  schema = schema,
                                  tableName = tableName)

        } else {

                SqlRender::render(SqlRender::readSql(paste0(path, "/rowCount.sql")),
                                  schema = schema,
                                  fields = fields,
                                  tableName = tableName)

        }

    }
