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
