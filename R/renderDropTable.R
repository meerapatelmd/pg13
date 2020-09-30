#' Render SQL to Drop a Table
#' @description Drop a table if it exists
#' @import SqlRender
#' @export

renderDropTable <-
    function(schema,
             tableName,
             if_exists = TRUE) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")


        if (if_exists) {

            SqlRender::render("
                              DROP TABLE IF EXISTS @schema.@tableName;
                              ",
                              schema = schema,
                              tableName = tableName)

        } else {

            SqlRender::render("
                          DROP TABLE @schema.@tableName;
                          ",
                              schema = schema,
                              tableName = tableName)
        }

    }
