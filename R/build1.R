#' Build a JOIN SQL Query
#' @description A SQL query is built using the given arguments. Currently, only 1 whereIn and whereNot in parameters can be set.
#' @return SQL statement as a character string.
#' @import purrr
#' @import stringr
#' @export

build_join_query <-
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
             case_insensitive,
             limit,
             random) {



                    if (!missing(random) && distinct) {

                        cli::cli_alert_warning("Cannot return DISTINCT and RANDOM. Overriding to DISTINCT == FALSE.")

                        distinct <- FALSE
                    }

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

                            cli::cli_alert_warning("both `where_in_field` & `where_in_vector` required. Ignoring filter...", wrap = TRUE)
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

                        cli::cli_alert_warning("both `where_in_join_on_field` & `where_in_join_on_vector` required. Ignoring filter...", wrap = TRUE)
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

                    cli::cli_alert_warning("both `where_not_in_field` & `where_not_in_vector` required. Ignoring filter...", wrap = TRUE)
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

                cli::cli_alert_warning("both `where_not_in_join_on_field` & `where_not_in_join_on_vector` required. Ignoring filter...", wrap = TRUE)
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

                    # +++
                    # Add Limit or Random
                    # +++

                    if (!missing(limit)) {
                        sql_statement <-
                            sprintf("%s\n%s",
                                        sql_statement,
                                        SqlRender::render("LIMIT @limit", limit = limit)
                            )
                    } else if (!missing(random)) {

                        sql_statement <-
                            sprintf("%s\n%s",
                                    sql_statement,
                                    SqlRender::render("ORDER BY RANDOM()\nLIMIT @limit", limit = random)
                            )

                    }

                    sql_statement

    }


build_query <-
    function(fields = "*",
             distinct = FALSE,
             schema,
             table,
             where_in_field,
             where_in_vector,
             where_not_in_field,
             where_not_in_vector,
             where_is_null_field,
             where_is_not_null_field,
             case_insensitive,
             limit,
             random) {



        if (!missing(random) && distinct) {

            cli::cli_alert_warning("Cannot return DISTINCT and RANDOM. Overriding to DISTINCT == FALSE.")

            distinct <- FALSE
        }


        # +++
        # Base Query
        # +++

        if (distinct) {

            sql_statement <-
                SqlRender::render(
                    "SELECT DISTINCT @fields\nFROM @schema.@table\n",
                    fields = fields,
                    schema = schema,
                    table = table
                )

        } else {

            sql_statement <-
                SqlRender::render(
                    "SELECT @fields\nFROM @schema.@table",
                    fields = fields,
                    schema = schema,
                    table = table
                )

        }

        # +++
        # Optional Where Filter
        # +++

        where <- list()
        if (!missing(where_in_field) && missing(where_in_vector)|
            missing(where_in_field) && !missing(where_in_vector)) {

            cli::cli_alert_warning("both `where_in_field` & `where_in_vector` required. Ignoring filter...", wrap = TRUE)
        } else if (!missing(where_in_field) && !missing(where_in_vector)) {

            if (is.character(where_in_vector)) {

                where_in_vector <- s_quo(vector = where_in_vector)

            }

            where_ins <- list(field = where_in_field,
                              vector = where_in_vector)

            where[[length(where)+1]] <- where_ins
            names(where)[length(where)] <- "where_ins"


        }


        if (!missing(where_not_in_field) && missing(where_not_in_vector)|
            missing(where_not_in_field) && !missing(where_not_in_vector)) {

            cli::cli_alert_warning("both `where_not_in_field` & `where_not_in_vector` required. Ignoring filter...", wrap = TRUE)
        } else if (!missing(where_not_in_field) && !missing(where_not_in_vector)) {

            if (is.character(where_not_in_vector)) {

                where_not_in_vector <- s_quo(vector = where_not_in_vector)

            }

            where_not_ins <- list(field = where_not_in_field,
                                  vector = where_not_in_vector)

            where[[length(where)+1]] <- where_not_ins
            names(where)[length(where)] <- "where_not_ins"

        }


        if (!missing(where_is_null_field)) {

            where[[length(where)+1]] <- SqlRender::render("a.@field IS NULL", field = where_is_null_field)
            names(where)[length(where)] <- "where_is_null_field"

        }

        if (!missing(where_is_not_null_field)) {

            where[[length(where)+1]] <- SqlRender::render("a.@field IS NOT NULL", field = where_is_not_null_field)
            names(where)[length(where)] <- "where_is_not_null_field"

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


            null_field_args <- c("where_is_null_field",
                                 "where_is_not_null_field")

            if (any(null_field_args %in% names(where))) {

                null_where_clauses <- unlist(where[names(where) %in% c("where_is_null_field",
                                                                       "where_is_not_null_field")])

                final_where <- paste(c(null_where_clauses, where_clauses), collapse = " AND \n")

            } else {

                final_where <- paste(where_clauses, collapse = " AND \n")
            }

            sql_statement <- paste0(sql_statement, final_where)
        }

        # +++
        # Add Limit or Random
        # +++

        if (!missing(limit)) {
            sql_statement <-
                sprintf("%s\n%s",
                        sql_statement,
                        SqlRender::render("LIMIT @limit", limit = limit)
                )
        } else if (!missing(random)) {

            sql_statement <-
                sprintf("%s\n%s",
                        sql_statement,
                        SqlRender::render("ORDER BY RANDOM()\nLIMIT @limit", limit = random)
                )

        }

        sql_statement

    }






#' Writes a Like SQL Query
#' @return a SQL Query as a character string.
#' @export

build_query_like <-
        function(fields = "*",
                 distinct = FALSE,
                 schema,
                 table,
                 whereLikeField,
                 whereLikeValue,
                 case_insensitive = TRUE,
                 limit_n = NULL) {


                sql_construct <-
                constructBase(fields = fields,
                              distinct = distinct,
                              schema = schema,
                              table = table)

                if (case_insensitive) {
                        sql_construct <-
                                c(sql_construct,
                                  constructWhereLowerLike(field = whereLikeField,
                                                          term = tolower(whereLikeValue))
                                ) %>%
                                paste(collapse = " ")
                } else {
                        sql_construct <-
                                c(sql_construct,
                                  constructWhereLike(field = whereLikeField,
                                                          term = whereLikeValue)
                                ) %>%
                                paste(collapse = " ")

                }
                sql_construct %>%
                        stringr::str_replace_all(pattern = "[\n]{2,}",
                                                 replacement = "\n") %>%
                    terminateBuild()


        }






#' Writes a SQL Query Loop
#' @description This function writes a SQL Query that loops over the words in a string.
#' @return a SQL Query as a character string.
#' @export

build_query_string <-
        function(fields = "*",
                 distinct = FALSE,
                 schema,
                 table,
                 whereLikeField,
                 string,
                 split,
                 case_insensitive = TRUE,
                 limit_n = NULL) {


                sql_construct <-
                constructBase(fields = fields,
                              distinct = distinct,
                              schema = schema,
                              table = table)

                args <- strsplit(string, split = split) %>%
                                        unlist()


                if (case_insensitive) {

                            args <- tolower(args)

                            for (i in 1:length(Args)) {

                                        if (i == 1) {
                                                    sql_construct <-
                                                        c(sql_construct,
                                                          constructWhereLowerLike(field = whereLikeField,
                                                                                  term = Args[1])
                                                        ) %>%
                                                        paste(collapse = " ")
                                        } else {


                                                sql_construct <-
                                                    concatWhereConstructs(sql_construct,
                                                                               where_sql_construct_2 = constructWhereLowerLike(field = whereLikeField, term = Args[i]))


                                        }
                            }

                } else {


                    for (i in 1:length(Args)) {

                                if (i == 1) {
                                            sql_construct <-
                                                c(sql_construct,
                                                  constructWhereLike(field = whereLikeField,
                                                                          term = Args[1])
                                                ) %>%
                                                paste(collapse = " ")
                                } else {


                                            sql_construct <-
                                                concatWhereConstructs(sql_construct,
                                                                      where_sql_construct_2 = constructWhereLike(field = whereLikeField, term = Args[i]))


                                }
                    }
                }

                if (!is.null(limit_n)) {

                            sql_construct <-
                                    c(sql_construct,
                                      constructLimit(n = limit_n)) %>%
                                        paste(collapse = " ")


                }

                sql_construct %>%
                        stringr::str_replace_all(pattern = "[\n]{2,}",
                                                 replacement = "\n") %>%
                        terminateBuild()


        }






