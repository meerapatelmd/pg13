#' Concatenate 2 WHERE constructs
#' @description When 2 WHERE constructs are included in the SQL Statement, they are concatenated and the 2nd "WHERE" is replaced with an "AND".
#' @param AND If TRUE, the WHERE constructs are concatenated with "AND". If FALSE, the concatenation is performed with an "OR".
#' @import stringr
#' @export

concatWhereConstructs <-
    function(sql_construct,
             where_sql_construct_2,
             ...,
             AND = TRUE) {


            .Deprecated()

            if (missing(...)) {

                            if (AND) {

                                    c(sql_construct,
                                      stringr::str_replace(where_sql_construct_2,
                                                           "WHERE",
                                                           "AND")) %>%
                                                paste(collapse = " ")
                            } else {

                                    c(sql_construct,
                                      stringr::str_replace(where_sql_construct_2,
                                                           "WHERE",
                                                           "OR")) %>%
                                            paste(collapse = " ")

                            }
            } else {

                    Args <- list(where_sql_construct_2,
                                 ...) %>%
                                unlist()

                    if (AND) {

                            c(sql_construct,
                            stringr::str_replace_all(Args,
                                                     "WHERE",
                                                     "AND")) %>%
                                    paste(collapse = " ")


                    } else {

                            c(sql_construct,
                              stringr::str_replace_all(Args,
                                                       "WHERE",
                                                       "OR")) %>%
                                    paste(collapse = " ")



                    }

            }

    }
