#' Concatenate 2 WHERE constructs
#' @description When 2 WHERE constructs are included in the SQL Statement, they are concatenated and the 2nd "WHERE" is replaced with an "AND".
#' @import stringr
#' @export

concatWhereConstructs <-
    function(sql_construct, where_sql_construct_2) {

                    c(sql_construct,
                      stringr::str_replace(where_sql_construct_2,
                                           "WHERE",
                                           "AND")) %>%
                                paste(collapse = " ")

    }
