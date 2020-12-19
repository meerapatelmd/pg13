#' @title
#' 2nd Generation Join
#'
#' @export
#' @rdname join2


join2 <-
        function(conn,
                 write_schema,
                 kind = c("left", "right", "inner", "full"),
                 data,
                 schema,
                 tableName,
                 ...,
                 cast_to_varchar = TRUE,
                 case_insensitive = TRUE,
                 verbose = TRUE,
                 render_sql = TRUE) {


                kind <-
                match.arg(kind,
                          choices = c("left", "right", "inner", "full"),
                          several.ok = FALSE)

                tableName <-
                        writeVTable(
                                writeSchema = writeSchema,
                                data = data,
                                verbose = verbose
                        )

                on.exit(dropWriteTable(
                        writeSchema = writeSchema,
                        tableName = tableName,
                        verbose = verbose
                ))


                join_clause <-
                        draftJoinOn(
                                ...,
                                cast_to_varchar = cast_to_varchar,
                                case_insensitive = case_insensitive
                        )



                sql_statement <-
                        SqlRender::render(
                                "
                              SELECT *
                              FROM @writeSchema.@tableName t
                              @join_type JOIN @omopSchema.@omopTable omop
                              ON @join_clause
                              ;
                              ",
                                omopSchema = omopSchema,
                                omopTable = omopTable,
                                join_type = join_type,
                                writeSchema = writeSchema,
                                tableName = tableName,
                                join_clause = join_clause)


                resultset <-
                        qOMOP(sql_statement = sql_statement,
                              verbose = verbose,
                              render_sql = render_sql,
                              skip_cache = TRUE
                        )
                resultset
        }



#' @export
#' @rdname draft_join_on

draft_join_on <-
        function(...,
                 cast_to_varchar,
                 case_insensitive) {

                Args <- rlang::list2(...)

                lhs <- names(Args)
                rhs <- unname(Args)

                output <- list()
                for (i in seq_along(lhs)) {

                        if (cast_to_varchar) {


                                if (case_insensitive) {

                                        output[[i]] <-
                                                SqlRender::render(
                                                        "
                                LOWER(@lh::varchar) = LOWER(@rh::varchar)
                                ",
                                                        lh = lhs[[i]],
                                                        rh = rhs[[i]]
                                                )

                                } else {

                                        output[[i]] <-
                                                SqlRender::render(
                                                        "
                                                @lh::varchar = @rh::varchar
                                                ",
                                                        lh = lhs[[i]],
                                                        rh = rhs[[i]]
                                                )


                                }


                        } else {


                                if (case_insensitive) {

                                        output[[i]] <-
                                                SqlRender::render(
                                                        "
                                LOWER(@lh) = LOWER(@rh)
                                ",
                                                        lh = lhs[[i]],
                                                        rh = rhs[[i]]
                                                )

                                } else {

                                        output[[i]] <-
                                                SqlRender::render(
                                                        "
                                                @lh = @rh
                                                ",
                                                        lh = lhs[[i]],
                                                        rh = rhs[[i]]
                                                )


                                }





                        }
                }

                output %>%
                        unlist() %>%
                        trimws(which = "both") %>%
                        paste(collapse = " AND ")


        }


#' @title
#' Second Generation Join Function Factory
#'
#' @description
#' Customize the join2 function
#'
#' @rdname join2_ff
#' @export

join2_ff <-
        function(writeSchema,
                 omopSchema,
                 omopTable,
                 join_type) {

                function(data,
                         omopSchema,
                         omopTable,
                         ...,
                         writeSchema,
                         cast_to_varchar = TRUE,
                         case_insensitive = TRUE,
                         render_sql = TRUE,
                         verbose = TRUE) {



                        join2(
                                data = data,
                                omopSchema = omopSchema,
                                omopTable = omopTable,
                                ...,
                                writeSchema = writeSchema,
                                join_type = join_type,
                                cast_to_varchar = cast_to_varchar,
                                case_insensitive = case_insensitive,
                                render_sql = render_sql,
                                verbose = verbose
                        )

                }
        }


#' @title
#' Second Generation Left Join
#'
#'
#' @rdname lj2
#' @export

lj2 <- join2_ff(join_type = "LEFT")


#' @title
#' Second Generation Inner Join
#'
#' @rdname ij2
#' @export

ij2 <- join2_ff(join_type = "INNER")
