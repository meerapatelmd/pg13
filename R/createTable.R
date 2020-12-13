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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @seealso
#'  \code{\link[forcats]{fct_collapse}}
#' @rdname draftCreateTableFromDF
#' @export
#' @importFrom forcats fct_collapse

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
#' Create a Table with a Dataframe
#'
#' @description
#' Derive DDL using the data classes of each field in a dataframe. The map between the R data classes and the Postgresql data types can be found at \code{\link{renderCreateTableFromDF}}. The dataframe can then be appended to the table using \code{\link{appendTable}}. This method is favorable to a direct call to \code{\link{writeTable}} because in some cases, future appends to the table may not adhere to the data definitions created at the time of writing. For example, \code{\link{writeTable}} defaults to `VARCHAR(255)` for all character classes whereas future appends may contain text greater than 255 characters, resulting in error. This function rolls all character classes to `TEXT` data types instead.
#'
#' @rdname createTableFromDF
#' @export

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
