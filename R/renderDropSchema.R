#' Render SQL to Drop a Schema
#' @description Drop a schema if it exists
#' @param cascade If TRUE, a DROP SCHEMA CASCADE is performed.
#' @import SqlRender
#' @export

renderDropSchema <-
    function(schema,
             cascade = FALSE,
             if_exists = TRUE) {

        base <- system.file(package='pg13')
        path <- paste0(base, "/sql")

        if (cascade) {

            SqlRender::render("
                              DROP SCHEMA @schema CASCADE
                              ;",
                              schema = schema)

        }

        if (if_exists) {

                SqlRender::render("
                                  DROP SCHEMA IF EXISTS @schema
                                  ;",
                                  schema = schema)

        } else {

                SqlRender::render("
                                  DROP SCHEMA @schema
                                  ;",
                                  schema = schema)
        }

    }

