#' @title
#' Second Generation Join
#'
#' @description
#' The second generation join is a join where the added feature of joining on multiple columns with "AND" logic is implemented. Another change is that all the join columns can optionally be cast to varchar to simplify the sql render. See \code{\link{join}} for the first generation join.
#'
#' @export
#' @param data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION, Default: NULL
#' @param omopSchema PARAM_DESCRIPTION
#' @param omopTable PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param writeSchema PARAM_DESCRIPTION
#' @param join_type PARAM_DESCRIPTION, Default: 'LEFT'
#' @param case_insensitive PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @name join2
#' @importFrom SqlRender render
NULL
#' .Join <-
#'         function(conn,
#'                  data,
#'                  column,
#'                  writeSchema,
#'                  rSchema,
#'                  rTable,
#'                  rColumn,
#'                  ...,
#'                  join_type = "LEFT",
#'                  case_insensitive = TRUE,
#'                  render_sql = TRUE,
#'                  verbose = TRUE) {
#'
#'
#'                 ArgA <- rlang::list2({{ column }} := rColumn)
#'                 ArgB <- rlang::list2(...)
#'                 Args <- c(ArgA,
#'                           ArgB)
#'
#'
#'                 draftJoinOn <-
#'                         function(...,
#'                                  case_insensitive) {
#'
#'                                 Args <- rlang::list2(...)
#'
#'                                 lhs <- names(Args)
#'                                 rhs <- unname(Args)
#'
#'
#'                                 output <- list()
#'                                 for (i in seq_along(lhs)) {
#'
#'                                         if (case_insensitive) {
#'
#'                                                 output[[i]] <-
#'                                                         SqlRender::render(
#'                                                                 "
#'                                 LOWER(@lh::varchar) = LOWER(@rh::varchar)
#'                                 ",
#'                                                                 lh = lhs[[i]],
#'                                                                 rh = rhs[[i]]
#'                                                         )
#'
#'                                         } else {
#'
#'                                                 output[[i]] <-
#'                                                         SqlRender::render(
#'                                                                 "
#'                                                 @lh::varchar = @rh::varchar
#'                                                 ",
#'                                                                 lh = lhs[[i]],
#'                                                                 rh = rhs[[i]]
#'                                                         )
#'
#'
#'                                         }
#'
#'
#'                                         } else {
#'
#'
#'                                                 if (case_insensitive) {
#'
#'                                                         output[[i]] <-
#'                                                                 SqlRender::render(
#'                                                                         "
#'                                 LOWER(@lh) = LOWER(@rh)
#'                                 ",
#'                                                                         lh = lhs[[i]],
#'                                                                         rh = rhs[[i]]
#'                                                                 )
#'
#'                                                 } else {
#'
#'                                                         output[[i]] <-
#'                                                                 SqlRender::render(
#'                                                                         "
#'                                                 @lh = @rh
#'                                                 ",
#'                                                                         lh = lhs[[i]],
#'                                                                         rh = rhs[[i]]
#'                                                                 )
#'
#'
#'                                                 }
#'
#'
#'
#'
#'
#'                                         }
#'                                 }
#'
#'                                 output %>%
#'                                         unlist() %>%
#'                                         trimws(which = "both") %>%
#'                                         paste(collapse = " AND ")
#'
#'
#'                         }
#'
#'
#'
#'
#'                 tableName <-
#'                         writeVTable(
#'                                 writeSchema = writeSchema,
#'                                 data = data,
#'                                 verbose = verbose
#'                         )
#'
#'
#'                 join_clause <-
#'                         draftJoinOn(
#'                                 ...,
#'                                 cast_to_varchar = cast_to_varchar,
#'                                 case_insensitive = case_insensitive
#'                         )
#'
#'
#'
#'                 sql_statement <-
#'                         SqlRender::render(
#'                                 "
#'                               SELECT *
#'                               FROM @writeSchema.@tableName t
#'                               @join_type JOIN @omopSchema.@omopTable omop
#'                               ON @join_clause
#'                               ;
#'                               ",
#'                                 omopSchema = omopSchema,
#'                                 omopTable = omopTable,
#'                                 join_type = join_type,
#'                                 writeSchema = writeSchema,
#'                                 tableName = tableName,
#'                                 join_clause = join_clause)
#'
#'
#'                 resultset <-
#'                         qOMOP(sql_statement = sql_statement,
#'                               verbose = verbose,
#'                               render_sql = render_sql,
#'                               skip_cache = TRUE
#'                         )
#'
#'
#'                 resultset



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
