#' Build a Query based on Constructed Parts


chariot::query_athena(
buildQuery(fields = c("concept_id", "concept_name"),
           schema = "public",
           tableName = "concept",
           whereNotInField = "concept_class_id",
           whereNotInVector = c("Component")))

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
                    # QA to make sure all whereIn and whereNotIn arguments have been supplied in pairs
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


                                if (length(whereNotIns) == 2) {


                                    sql_construct <-
                                        paste(sql_construct,
                                              constructWhereNotIn(field = whereNotIns$field,
                                                                  vector = whereNotIns$vector),
                                              collapse = " ")


                                }



                    }

                    sql_construct












    }

