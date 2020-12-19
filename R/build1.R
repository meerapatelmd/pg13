#' Build a JOIN SQL Query
#' @description A SQL query is built using the given arguments. Currently, only 1 whereIn and whereNot in parameters can be set.
#' @return SQL statement as a character string.
#' @import purrr
#' @import stringr
#' @export

build_join_query <-
    function(fields = "*",
             distinct = FALSE,
             schema,
             tableName,
             column,
             joinOnSchema,
             joinOnTableName,
             joinOnColumn,
             joinType = "LEFT",
             whereInField = NULL,
             whereInVector = NULL,
             whereNotInField = NULL,
             whereNotInVector = NULL,
             caseInsensitive = TRUE,
             n = NULL,
             n_type = c("limit", "random")) {

                    ######
                    # QA to make sure all whereIn and n  arguments have been supplied in pairs
                    #####
                    where_ins <- list(where_in_field, where_in_vector) %>%
                                        purrr::set_names(c("field", "vector")) %>%
                                        purrr::keep(~!is.null(.))
                    where_not_ins <- list(where_not_in_field, where_not_in_vector) %>%
                                        purrr::set_names(c("field", "vector")) %>%
                                        purrr::keep(~!is.null(.))


                    list(whereIns, whereNotIns) %>%
                        purrr::map2(list("whereIn", "whereNotIn"),
                                   function(x,y) if (!(length(x) %in% c(0,2))) {stop('both "', y, '" arguments must be supplied')})

                    ######
                    # QA to make sure all n arugments have been supplied
                    #####

                    if (length(n) == 1 & length(n_type) != 1) {

                            n_type <- "limit"

                            warning('"n_type" set to "limit"')

                    }

                    #####
                    # Start
                    #####
                    sql_construct  <- construct_base(fields = fields,
                                                    distinct = distinct,
                                                    schema = schema,
                                                    tableName = tableName)

                    # Add join
                    sql_construct <-
                            c(sql_construct,
                              constructJoin(schema = schema,
                                            tableName = tableName,
                                            column = column,
                                            joinType = joinType,
                                            joinOnSchema = joinOnSchema,
                                            joinOnTableName = joinOnTableName,
                                            joinOnColumn = joinOnColumn)) %>%
                            paste(collapse = " ")



                    if (caseInsensitive) {


                        # If WhereIn arguments are not null include it in build
                        if (length(whereIns) == 2) {

                            sql_construct <-
                                paste(sql_construct,
                                      constructWhereLowerIn(field = whereIns$field,
                                                       vector = tolower(whereIns$vector)),
                                      collapse = " ")

                            # If WhereNotIn arguments are supplied on top of the WhereIn, add them to the query by modifying the constructWhereNotIn output by replacing the second "WHERE" with "AND"
                            if (length(whereNotIns) == 2) {


                                sql_construct <-
                                    paste(sql_construct,
                                          "AND",
                                          constructWhereLowerNotIn(field = whereNotIns$field,
                                                              vector = tolower(whereNotIns$vector)) %>%
                                              stringr::str_remove_all("WHERE") %>%
                                              trimws(),
                                          collapse = " ")


                            }

                        } else {

                            # Building a query if only whereNotIn arguments were supplied
                            if (length(whereNotIns) == 2) {


                                sql_construct <-
                                    paste(sql_construct,
                                          constructWhereLowerNotIn(field = whereNotIns$field,
                                                              vector = tolower(whereNotIns$vector)),
                                          collapse = " ")


                            }



                        }

                        # If n arguments are not null include it in build, as either a limit or random sample of size n
                        if (!is.null(n)) {

                            if (n_type == "limit") {

                                sql_construct <-
                                    paste(sql_construct,
                                          constructLimit(n = n),
                                          collapse = " ")

                            } else if (n_type == "random") {

                                sql_construct <-
                                    paste(sql_construct,
                                          constructRandom(n = n),
                                          collapse = " ")

                            } else {

                                warning('"n_type" not recognized and "n" removed from build')


                            }

                        }









                    } else {



                                    # If WhereIn arguments are not null include it in build
                                    if (length(whereIns) == 2) {

                                            sql_construct <-
                                                    paste(sql_construct,
                                                          constructWhereIn(field = whereIns$field,
                                                                            vector = whereIns$vector),
                                                          collapse = " ")

                                            # If WhereNotIn arguments are supplied on top of the WhereIn, add them to the query by modifying the constructWhereNotIn output by replacing the second "WHERE" with "AND"
                                            if (length(whereNotIns) == 2) {


                                                            sql_construct <-
                                                                paste(sql_construct,
                                                                      "AND",
                                                                      constructWhereNotIn(field = whereNotIns$field,
                                                                                       vector = whereNotIns$vector) %>%
                                                                          stringr::str_remove_all("WHERE") %>%
                                                                          trimws(),
                                                                      collapse = " ")


                                            }

                                    } else {

                                                # Building a query if only whereNotIn arguments were supplied
                                                if (length(whereNotIns) == 2) {


                                                    sql_construct <-
                                                        paste(sql_construct,
                                                              constructWhereNotIn(field = whereNotIns$field,
                                                                                  vector = whereNotIns$vector),
                                                              collapse = " ")


                                                }



                                    }

                                    # If n arguments are not null include it in build, as either a limit or random sample of size n
                                    if (!is.null(n)) {

                                                if (n_type == "limit") {

                                                    sql_construct <-
                                                                paste(sql_construct,
                                                                      constructLimit(n = n),
                                                                      collapse = " ")

                                                } else if (n_type == "random") {

                                                    sql_construct <-
                                                        paste(sql_construct,
                                                              constructRandom(n = n),
                                                              collapse = " ")

                                                } else {

                                                    warning('"n_type" not recognized and "n" removed from build')


                                                }

                                    }

                    }

                    #Add a semicolon to finish the query
                    sql_construct %>%
                            stringr::str_replace_all(pattern = "[\n]{2,}",
                                                     replacement = "\n") %>%
                            terminateBuild()


    }






#' Build a SQL Query
#' @description A SQL query is built using the given arguments. Currently, only 1 whereIn and whereNot in parameters can be set.
#' @return SQL statement as a character string.
#' @import purrr
#' @import stringr
#' @export

build_query <-
    function(fields = "*",
             distinct = FALSE,
             schema,
             tableName,
             whereInField = NULL,
             whereInVector = NULL,
             whereNotInField = NULL,
             whereNotInVector = NULL,
             caseInsensitive = TRUE,
             n = NULL,
             n_type = c("limit", "random")) {

                    ######
                    # QA to make sure all whereIn and n  arguments have been supplied in pairs
                    #####
                    where_ins <- list(where_in_field, where_in_vector) %>%
                                        purrr::set_names(c("field", "vector")) %>%
                                        purrr::keep(~!is.null(.))
                    where_not_ins <- list(where_not_in_field, where_not_in_vector) %>%
                                        purrr::set_names(c("field", "vector")) %>%
                                        purrr::keep(~!is.null(.))


                    list(whereIns, whereNotIns) %>%
                        purrr::map2(list("whereIn", "whereNotIn"),
                                   function(x,y) if (!(length(x) %in% c(0,2))) {stop('both "', y, '" arguments must be supplied')})

                    ######
                    # QA to make sure all n arugments have been supplied
                    #####

                    if (length(n) == 1 & length(n_type) != 1) {

                            n_type <- "limit"

                            warning('"n_type" set to "limit"')

                    }

                    #####
                    # Start
                    #####
                    sql_construct  <- construct_base(fields = fields,
                                                    distinct = distinct,
                                                    schema = schema,
                                                    tableName = tableName)


                    if (caseInsensitive) {


                        # If WhereIn arguments are not null include it in build
                        if (length(whereIns) == 2) {

                            sql_construct <-
                                paste(sql_construct,
                                      constructWhereLowerIn(field = whereIns$field,
                                                       vector = tolower(whereIns$vector)),
                                      collapse = " ")

                            # If WhereNotIn arguments are supplied on top of the WhereIn, add them to the query by modifying the constructWhereNotIn output by replacing the second "WHERE" with "AND"
                            if (length(whereNotIns) == 2) {


                                sql_construct <-
                                    paste(sql_construct,
                                          "AND",
                                          constructWhereLowerNotIn(field = whereNotIns$field,
                                                              vector = tolower(whereNotIns$vector)) %>%
                                              stringr::str_remove_all("WHERE") %>%
                                              trimws(),
                                          collapse = " ")


                            }

                        } else {

                            # Building a query if only whereNotIn arguments were supplied
                            if (length(whereNotIns) == 2) {


                                sql_construct <-
                                    paste(sql_construct,
                                          constructWhereLowerNotIn(field = whereNotIns$field,
                                                              vector = tolower(whereNotIns$vector)),
                                          collapse = " ")


                            }



                        }

                        # If n arguments are not null include it in build, as either a limit or random sample of size n
                        if (!is.null(n)) {

                            if (n_type == "limit") {

                                sql_construct <-
                                    paste(sql_construct,
                                          constructLimit(n = n),
                                          collapse = " ")

                            } else if (n_type == "random") {

                                sql_construct <-
                                    paste(sql_construct,
                                          constructRandom(n = n),
                                          collapse = " ")

                            } else {

                                warning('"n_type" not recognized and "n" removed from build')


                            }

                        }









                    } else {



                                    # If WhereIn arguments are not null include it in build
                                    if (length(whereIns) == 2) {

                                            sql_construct <-
                                                    paste(sql_construct,
                                                          constructWhereIn(field = whereIns$field,
                                                                            vector = whereIns$vector),
                                                          collapse = " ")

                                            # If WhereNotIn arguments are supplied on top of the WhereIn, add them to the query by modifying the constructWhereNotIn output by replacing the second "WHERE" with "AND"
                                            if (length(whereNotIns) == 2) {


                                                            sql_construct <-
                                                                paste(sql_construct,
                                                                      "AND",
                                                                      constructWhereNotIn(field = whereNotIns$field,
                                                                                       vector = whereNotIns$vector) %>%
                                                                          stringr::str_remove_all("WHERE") %>%
                                                                          trimws(),
                                                                      collapse = " ")


                                            }

                                    } else {

                                                # Building a query if only whereNotIn arguments were supplied
                                                if (length(whereNotIns) == 2) {


                                                    sql_construct <-
                                                        paste(sql_construct,
                                                              constructWhereNotIn(field = whereNotIns$field,
                                                                                  vector = whereNotIns$vector),
                                                              collapse = " ")


                                                }



                                    }

                                    # If n arguments are not null include it in build, as either a limit or random sample of size n
                                    if (!is.null(n)) {

                                                if (n_type == "limit") {

                                                    sql_construct <-
                                                                paste(sql_construct,
                                                                      constructLimit(n = n),
                                                                      collapse = " ")

                                                } else if (n_type == "random") {

                                                    sql_construct <-
                                                        paste(sql_construct,
                                                              constructRandom(n = n),
                                                              collapse = " ")

                                                } else {

                                                    warning('"n_type" not recognized and "n" removed from build')


                                                }

                                    }

                    }

                    #Add a semicolon to finish the query
                    sql_construct %>%
                            stringr::str_replace_all(pattern = "[\n]{2,}",
                                                     replacement = "\n") %>%
                            terminateBuild()


    }






#' Writes a Like SQL Query
#' @return a SQL Query as a character string.
#' @export

build_query_like <-
        function(fields = "*",
                 distinct = FALSE,
                 schema,
                 tableName,
                 whereLikeField,
                 whereLikeValue,
                 caseInsensitive = TRUE,
                 limit_n = NULL) {


                sql_construct <-
                constructBase(fields = fields,
                              distinct = distinct,
                              schema = schema,
                              tableName = tableName)

                if (caseInsensitive) {
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
                 tableName,
                 whereLikeField,
                 string,
                 split,
                 caseInsensitive = TRUE,
                 limit_n = NULL) {


                sql_construct <-
                constructBase(fields = fields,
                              distinct = distinct,
                              schema = schema,
                              tableName = tableName)

                args <- strsplit(string, split = split) %>%
                                        unlist()


                if (caseInsensitive) {

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






