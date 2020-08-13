#' Writes a Like SQL Query
#' @return a SQL Query as a character string.
#' @export

buildQueryLike <-
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

