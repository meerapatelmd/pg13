#' Build a Query based on Constructs
#' @description A SQL query is built from construction of parts from the construct primitive functions in this package.
#' @import purrr
#' @import stringr
#' @export

buildQuery <-
    function(fields = "*",
             schema,
             tableName,
             whereInField = NULL,
             whereInVector = NULL,
             whereNotInField = NULL,
             whereNotInVector = NULL,
             n = NULL,
             n_type = c("limit", "random")) {

                    ######
                    # QA to make sure all whereIn and n  arguments have been supplied in pairs
                    #####
                    whereIns <- list(whereInField, whereInVector) %>%
                                        purrr::set_names(c("field", "vector")) %>%
                                        purrr::keep(~!is.null(.))
                    whereNotIns <- list(whereNotInField, whereNotInVector) %>%
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
                    sql_construct  <- constructBase(fields = fields,
                                              schema = schema,
                                              tableName = tableName)



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

                    #Add a semicolon to finish the query
                    sql_construct %>%
                        terminateBuild()


    }

