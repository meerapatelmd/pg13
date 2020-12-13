#' @title
#' Draft SQL to Create a Table
#' @param ... Named vector of field names and their corresponding data definition.
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[SqlRender]{render}}
#' @rdname createTable
#' @export
#' @importFrom rlang list2
#' @importFrom SqlRender render

draftCreateTable <-
        function(schema,
                 tableName,
                 if_not_exists = TRUE,
                 ...) {


                ddl <- rlang::list2(...)

                fields <- names(ddl)

                ddl <- mapply(paste, fields, ddl, collapse = " ")
                ddl <- paste(ddl, collapse = ",\n")

                if (if_not_exists) {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                        CREATE TABLE IF NOT EXISTS @schema.@tableName (
                                                @ddl
                                        );
                                        ",
                                        schema = schema,
                                        tableName = tableName,
                                        ddl = ddl
                                )

                } else {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                        CREATE TABLE @schema.@tableName (
                                                @ddl
                                        );
                                        ",
                                        schema = schema,
                                        tableName = tableName,
                                        ddl = ddl
                                )


                }

                sql_statement
        }










#' @title
#' Create a Table
#' @param ... Named vector of field names and their corresponding data definition.
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[pg13]{send}}
#' @rdname createTable
#' @export
#' @importFrom rlang list2
#' @importFrom SqlRender render
#' @importFrom pg13 send

createTable <-
        function(conn,
                 schema,
                 tableName,
                 if_not_exists = TRUE,
                 ...,
                 verbose = TRUE,
                 render_sql = TRUE) {


                sql_statement <-
                draftCreateTable(
                        schema = schema,
                        tableName = tableName,
                        if_not_exists = if_not_exists,
                        ...
                )


                send(
                        conn = conn,
                        sql_statement = sql_statement,
                        verbose = verbose,
                        render_sql = render_sql
                )

        }



draftCreateTableFromDF <-
        function(schema,
                 tableName,
                 data,
                 if_not_exists = TRUE) {

                makeDDL <-
                        function(data) {

                                r_types <- lapply(data, class)
                                r_types <- sapply(r_types, paste, collapse = ", ") # Necessary because there are 2 classes for some data types, such as Sys.time()'s POSIXct, POSIXt

                                field_names <- names(r_types)

                                ddl <- factor(r_types)
                                ddl <-
                                        forcats::fct_collapse(
                                                .f = ddl,
                                                bigint = "integer",
                                                `timestamp without time zone` = "POSIXct, POSIXt",
                                                date = "Date",
                                                float  = c("double", "numeric"),
                                                other_level = "text"
                                        ) %>%
                                        as.character()

                                names(ddl) <- field_names
                                ddl
                        }

                ddl <- makeDDL(data = data)

                draftCreateTable(schema = schema,
                                 tableName = tableName,
                                 if_not_exists = if_not_exists,
                                 ddl)

        }



createTableFromDF <-
        function(conn,
                 conn_fun,
                 schema,
                 tableName,
                 if_not_exists = TRUE,
                 data,
                 verbose = TRUE,
                 render_sql = TRUE) {

                sql_statement <- draftCreateTableFromDF(schema = schema,
                                                    tableName = tableName,
                                                    data = data,
                                                    if_not_exists = if_not_exists)


                send(
                        conn = conn,
                        sql_statement = sql_statement,
                        verbose = verbose,
                        render_sql = render_sql
                )

        }
