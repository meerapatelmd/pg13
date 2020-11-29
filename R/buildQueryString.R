#' Writes a SQL Query Loop
#' @description This function writes a SQL Query that loops over the words in a string.
#' @return a SQL Query as a character string.
#' @export

buildQueryString <-
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

                Args <- strsplit(string, split = split) %>%
                                        unlist()


                if (caseInsensitive) {

                            Args <- tolower(Args)

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

