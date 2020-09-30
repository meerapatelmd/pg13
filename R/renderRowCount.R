#' Render SQL for a Table Row Count
#' @import SqlRender
#' @param fields Fields selected for. Defaults to "*".
#' @param distinct If TRUE, the distinct row count will be returned.
#' @param schema If NULL, defaults to "public"
#' @export

renderRowCount <-
    function(fields = "*",
             distinct = FALSE,
             schema,
             tableName) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        if (distinct) {

                SqlRender::render(
                                    "
                                    SELECT DISTINCT COUNT(@fields)
                                    FROM @schema.@tableName
                                    ;
                                    ",
                    schema = schema,
                    fields = fields,
                    tableName = tableName)

        } else {

                SqlRender::render(
                                "
                                SELECT COUNT(@fields)
                                FROM @schema.@tableName
                                ;
                                ",
                                  schema = schema,
                                  fields = fields,
                                  tableName = tableName)

        }

    }
