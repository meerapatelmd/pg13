#' @title
#' Search a Table for a Value
#'
#' @description
#' If the Field to query is unknown, this function can be used to search and query all Fields or a subset of Fields for a given value. The `value` is converted to character and the Field is cast to varchar in the query and as a result, the query times may be long which may require providing a subset of Fields as an argument rather than searching the entire Table in many cases.
#'
#'
#'
#' @export


searchTable <-
        function(conn,
                 schema,
                 tableName,
                 values,
                 case_insensitive = TRUE,
                 ...) {

                # Prepare Value argument
                values <- as.character(values)

                if (case_insensitive) {
                        values <- tolower(values)
                }

                values <- sQuo(values)

                if (missing(...)) {
                        Fields <- lsFields(conn = conn,
                                           schema = schema,
                                           tableName = tableName)
                } else {
                        Fields <- unlist(as.list(...))
                }

                        output <- list()

                        if (case_insensitive) {

                                for (i in 1:length(Fields)) {

                                        Field <- Fields[i]

                                        sql_statement <-
                                        SqlRender::render(
                                                "
                                                SELECT *
                                                FROM @schema.@tableName t
                                                WHERE LOWER(t.@Field::varchar) IN (@values)
                                                ;
                                                ",
                                                schema = schema,
                                                tableName = tableName,
                                                Field = Field,
                                                values = values
                                        )


                                        print(sql_statement)


                                        x <-
                                                query(conn = conn,
                                                      sql_statement = sql_statement)
                                        if (!is.null(x)) {
                                                output[[i]] <- x
                                                names(output)[i] <- Field
                                        }

                                }

                        } else {

                                for (i in 1:length(Fields)) {

                                Field <- Fields[i]

                                sql_statement <-
                                        SqlRender::render(
                                                "
                                                SELECT *
                                                FROM @schema.@tableName t
                                                WHERE t.@Field::varchar IN (@values)
                                                ;
                                                ",
                                                schema = schema,
                                                tableName = tableName,
                                                Field = Field,
                                                values = values
                                        )

                                x <-
                                        query(conn = conn,
                                              sql_statement = sql_statement)
                                if (!is.null(x)) {
                                        output[[i]] <- x
                                        names(output)[i] <- Field
                                }


                                }
                        }



                output
        }



