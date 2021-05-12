#' @title
#' Draft a First Degree Join
#'
#' @description
#' A `First Degree Join` is one where the
#' `JOIN ON` clause occurs on one column on the left and
#' one column on the right.
#'
#' @inheritParams args
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname draft_join1
#' @export
#' @importFrom SqlRender render
#' @example inst/example/join_first_degree.R

draft_join1 <-
        function(select_table_fields = "*",
                 select_join_on_fields = "*",
                 distinct = FALSE,
                 schema,
                 table,
                 column,
                 join_on_schema,
                 join_on_table,
                 join_on_column,
                 kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 where_in_field,
                 where_in_vector,
                 where_in_join_on_field,
                 where_in_join_on_vector,
                 where_not_in_field,
                 where_not_in_vector,
                 where_not_in_join_on_field,
                 where_not_in_join_on_vector,
                 where_is_null_field,
                 where_is_not_null_field,
                 where_is_null_join_on_field,
                 where_is_not_null_join_on_field,
                 case_insensitive) {


                kind <-
                        match.arg(arg = kind,
                                  choices = c("LEFT", "RIGHT", "INNER", "FULL"),
                                  several.ok = FALSE)

                # +++
                # Base Query
                # +++

                if (distinct) {

                        sql_statement <-
                                SqlRender::render(
                                        "SELECT DISTINCT @select_table_fields, @select_join_on_fields\nFROM @schema.@table a\n@kind JOIN @join_on_schema.@join_on_table b\nON a.@column = b.@join_on_column",
                                        select_table_fields = sprintf("a.%s", select_table_fields),
                                        select_join_on_fields = sprintf("b.%s", select_join_on_fields),
                                        schema = schema,
                                        table = table,
                                        kind = kind,
                                        join_on_schema = join_on_schema,
                                        join_on_table = join_on_table,
                                        column = column,
                                        join_on_column = join_on_column
                                )

                } else {

                        sql_statement <-
                                SqlRender::render(
                                        "SELECT @select_table_fields, @select_join_on_fields\nFROM @schema.@table a\n@kind JOIN @join_on_schema.@join_on_table b\nON a.@column = b.@join_on_column",
                                        select_table_fields = sprintf("a.%s", select_table_fields),
                                        select_join_on_fields = sprintf("b.%s", select_join_on_fields),
                                        schema = schema,
                                        table = table,
                                        kind = kind,
                                        join_on_schema = join_on_schema,
                                        join_on_table = join_on_table,
                                        column = column,
                                        join_on_column = join_on_column
                                )

                }

                # +++
                # Optional Where Filter
                # +++

                where <- list()
                if (!missing(where_in_field) && missing(where_in_vector)|
                    missing(where_in_field) && !missing(where_in_vector)) {

                       typewrite_alert_warning("both `where_in_field` & `where_in_vector` required. Ignoring filter...", wrap = TRUE)
                } else if (!missing(where_in_field) && !missing(where_in_vector)) {

                        if (is.character(where_in_vector)) {

                                where_in_vector <- s_quo(vector = where_in_vector)

                        }

                        where_ins <- list(field = where_in_field,
                                          vector = where_in_vector)

                        where[[length(where)+1]] <- where_ins
                        names(where)[length(where)] <- "where_ins"


                }

                if (!missing(where_in_join_on_field) && missing(where_in_join_on_vector)|
                    missing(where_in_join_on_field) && !missing(where_in_join_on_vector)) {

                       typewrite_alert_warning("both `where_in_join_on_field` & `where_in_join_on_vector` required. Ignoring filter...", wrap = TRUE)
                } else if (!missing(where_in_join_on_field) && !missing(where_in_join_on_vector)) {

                        if (is.character(where_in_join_on_vector)) {

                                where_in_join_on_vector <- s_quo(vector = where_in_join_on_vector)

                        }

                        where_ins_join_on <- list(field = where_in_join_on_field,
                                                  vector = where_in_join_on_vector)


                        where[[length(where)+1]] <- where_ins_join_on
                        names(where)[length(where)] <- "where_ins_join_on"

                }

                if (!missing(where_not_in_field) && missing(where_not_in_vector)|
                    missing(where_not_in_field) && !missing(where_not_in_vector)) {

                       typewrite_alert_warning("both `where_not_in_field` & `where_not_in_vector` required. Ignoring filter...", wrap = TRUE)
                } else if (!missing(where_not_in_field) && !missing(where_not_in_vector)) {

                        if (is.character(where_not_in_vector)) {

                                where_not_in_vector <- s_quo(vector = where_not_in_vector)

                        }

                        where_not_ins <- list(field = where_not_in_field,
                                              vector = where_not_in_vector)

                        where[[length(where)+1]] <- where_not_ins
                        names(where)[length(where)] <- "where_not_ins"

                }


                if (!missing(where_not_in_join_on_field) && missing(where_not_in_join_on_vector)|
                    missing(where_not_in_join_on_field) && !missing(where_not_in_join_on_vector)) {

                       typewrite_alert_warning("both `where_not_in_join_on_field` & `where_not_in_join_on_vector` required. Ignoring filter...", wrap = TRUE)
                } else if (!missing(where_not_in_join_on_field) && !missing(where_not_in_join_on_vector)) {


                        if (is.character(where_not_in_join_on_vector)) {

                                where_not_in_join_on_vector <- s_quo(vector = where_not_in_join_on_vector)

                        }

                        where_not_ins_join_on <- list(field = where_not_in_join_on_field,
                                                      vector = where_not_in_join_on_vector)


                        where[[length(where)+1]] <- where_not_ins_join_on
                        names(where)[length(where)] <- "where_not_ins_join_on"

                }


                if (!missing(where_is_null_field)) {

                        where[[length(where)+1]] <- SqlRender::render("a.@field IS NULL", field = where_is_null_field)
                        names(where)[length(where)] <- "where_is_null_field"

                }

                if (!missing(where_is_not_null_field)) {

                        where[[length(where)+1]] <- SqlRender::render("a.@field IS NOT NULL", field = where_is_not_null_field)
                        names(where)[length(where)] <- "where_is_not_null_field"

                }

                if (!missing(where_is_null_join_on_field)) {

                        where[[length(where)+1]] <- SqlRender::render("b.@field IS NULL", field = where_is_null_join_on_field)
                        names(where)[length(where)] <- "where_is_null_join_on_field"

                }

                if (!missing(where_is_not_null_join_on_field)) {

                        where[[length(where)+1]] <- SqlRender::render("b.@field IS NOT NULL", field = where_is_not_null_join_on_field)
                        names(where)[length(where)] <- "where_is_not_null_join_on_field"

                }



                if (length(where) > 0) {

                        sql_statement <- sprintf("%s\nWHERE\n", sql_statement)

                        where_clauses <- vector()
                        if ("where_ins" %in% names(where)) {

                                field <- where$where_ins$field
                                vector <- where$where_ins$vector


                                if (case_insensitive && is.character(vector)) {

                                        where_in_clause_a <-
                                                SqlRender::render(
                                                        "LOWER(a.@field::varchar) IN (@vector)",
                                                        field = field,
                                                        vector = tolower(vector)
                                                )

                                } else {
                                        where_in_clause_a <-
                                                SqlRender::render(
                                                        "a.@field IN (@vector)",
                                                        field = field,
                                                        vector = vector
                                                )
                                }

                                where_clauses <-
                                        c(where_clauses,
                                          where_in_clause_a)

                        }

                        if ("where_ins_join_on" %in% names(where)) {

                                field <- where$where_ins_join_on$field
                                vector <- where$where_ins_join_on$vector

                                if (case_insensitive && is.character(vector)) {

                                        where_in_clause_b <-
                                                SqlRender::render(
                                                        "LOWER(b.@field::varchar) IN (@vector)",
                                                        field = field,
                                                        vector = tolower(vector)
                                                )

                                } else {

                                        where_in_clause_b <-
                                                SqlRender::render(
                                                        "b.@field IN (@vector)",
                                                        field = field,
                                                        vector = vector
                                                )
                                }

                                where_clauses <-
                                        c(where_clauses,
                                          where_in_clause_b)

                        }

                        if ("where_not_ins" %in% names(where)) {

                                field <- where$where_not_ins$field
                                vector <- where$where_not_ins$vector

                                if (case_insensitive && is.character(vector)) {

                                        where_not_in_clause_a <-
                                                SqlRender::render(
                                                        "LOWER(a.@field::varchar) NOT IN (@vector)",
                                                        field = field,
                                                        vector = tolower(vector)
                                                )

                                } else {
                                        where_not_in_clause_a <-
                                                SqlRender::render(
                                                        "a.@field NOT IN (@vector)",
                                                        field = field,
                                                        vector = vector
                                                )
                                }

                                where_clauses <-
                                        c(where_clauses,
                                          where_not_in_clause_a)

                        }

                        if ("where_not_ins_join_on" %in% names(where)) {

                                field <- where$where_not_ins_join_on$field
                                vector <- where$where_not_ins_join_on$vector


                                if (case_insensitive && is.character(vector)) {
                                        where_not_in_clause_b <-
                                                SqlRender::render(
                                                        "LOWER(b.@field::varchar) NOT IN (@vector)",
                                                        field = field,
                                                        vector = tolower(vector)
                                                )

                                } else {
                                        where_not_in_clause_b <-
                                                SqlRender::render(
                                                        "b.@field NOT IN (@vector)",
                                                        field = field,
                                                        vector = vector
                                                )
                                }

                                where_clauses <-
                                        c(where_clauses,
                                          where_not_in_clause_b)

                        }



                        null_field_args <- c("where_is_null_field",
                                             "where_is_null_join_on_field",
                                             "where_is_not_null_field",
                                             "where_is_not_null_join_on_field")

                        if (any(null_field_args %in% names(where))) {

                                null_where_clauses <- unlist(where[names(where) %in% c("where_is_null_field",
                                                                                       "where_is_null_join_on_field",
                                                                                       "where_is_not_null_field",
                                                                                       "where_is_not_null_join_on_field")])

                                final_where <- paste(c(null_where_clauses, where_clauses), collapse = " AND \n")

                        } else {

                                final_where <- paste(where_clauses, collapse = " AND \n")
                        }

                        sql_statement <- paste0(sql_statement, final_where)
                }

                sql_statement

        }


#' @title
#' 1st Degree Join
#'
#' @export
#' @rdname join1
#' @example inst/example/join_first_degree.R

join1 <-
        function(conn,
                 conn_fun,
                 write_schema,
                 data,
                 column,
                 select_table_fields = "*",
                 select_join_on_fields = "*",
                 join_on_schema,
                 join_on_table,
                 join_on_column,
                 kind = c("LEFT", "RIGHT", "INNER", "FULL"),
                 where_in_field,
                 where_in_vector,
                 where_in_join_on_field,
                 where_in_join_on_vector,
                 where_not_in_field,
                 where_not_in_vector,
                 where_not_in_join_on_field,
                 where_not_in_join_on_vector,
                 where_is_null_field,
                 where_is_not_null_field,
                 where_is_null_join_on_field,
                 where_is_not_null_join_on_field,
                 case_insensitive,
                 distinct = FALSE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                if (!missing(conn_fun)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(dc(conn = conn,
                                   verbose = verbose),
                                add = TRUE,
                                after = TRUE)

                }


                staging_table <-
                write_staging_table(conn = conn,
                                    schema = write_schema,
                                    data = data,
                                    drop_existing = TRUE,
                                    drop_on_exit = TRUE,
                                    verbose = verbose,
                                    render_sql = render_sql)

                sql_statement <-
                        draft_join1(select_table_fields = select_table_fields,
                                        select_join_on_fields = select_join_on_fields,
                                        distinct = distinct,
                                        schema = write_schema,
                                        table = staging_table,
                                        column = column,
                                        join_on_schema = join_on_schema,
                                        join_on_table = join_on_table,
                                        join_on_column = join_on_column,
                                        kind = kind,
                                        where_in_field = where_in_field,
                                        where_in_vector = where_in_vector,
                                        where_in_join_on_field = where_in_join_on_field,
                                        where_in_join_on_vector = where_in_join_on_vector,
                                        where_not_in_field = where_not_in_field,
                                        where_not_in_vector = where_not_in_vector,
                                        where_not_in_join_on_field = where_not_in_join_on_field,
                                        where_not_in_join_on_vector = where_not_in_join_on_vector,
                                        where_is_null_field = where_is_null_field,
                                        where_is_not_null_field = where_is_not_null_field,
                                        where_is_null_join_on_field = where_is_null_join_on_field,
                                        where_is_not_null_join_on_field = where_is_not_null_join_on_field,
                                        case_insensitive = case_insensitive)


                query(conn = conn,
                      sql_statement = sql_statement,
                      verbose = verbose,
                      render_sql = render_sql,
                      render_only = render_only)
        }
