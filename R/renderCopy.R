#' Render SQL to copy a file to a table
#' @import SqlRender
#' @export

renderCopy <-
    function(schema,
             tableName,
             csvFilePath) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        SqlRender::render("
                          COPY @schema.@tableName FROM '@csvFilePath' WITH DELIMITER E'\\t' CSV HEADER QUOTE E'\\b';
                          ",
                          schema = schema,
                          tableName = tableName,
                          csvFilePath = csvFilePath)

    }
