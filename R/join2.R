#' @title
#' 2nd Degree Join
#' @description
#' A `Second Degree Join` is one where the `JOIN ON` clause occurs on more than one column on the left and one column for each on the right.
#'
#' @export
#' @rdname join2


join2 <-
  function(conn,
           write_schema,
           kind = c("left", "right", "inner", "full"),
           data,
           schema,
           table_name,
           ...,
           cast_to_varchar = TRUE,
           case_insensitive = TRUE,
           verbose = TRUE,
           render_sql = TRUE) {
    kind <-
      match.arg(kind,
        choices = c("left", "right", "inner", "full"),
        several.ok = FALSE
      )

    table_name <-
      writeVTable(
        write_schema = write_schema,
        data = data,
        verbose = verbose
      )

    on.exit(dropWriteTable(
      write_schema = write_schema,
      table_name = table_name,
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
                              FROM @write_schema.@table_name t
                              @join_type JOIN @omopSchema.@omopTable omop
                              ON @join_clause
                              ;
                              ",
        omopSchema = omopSchema,
        omopTable = omopTable,
        join_type = join_type,
        write_schema = write_schema,
        table_name = table_name,
        join_clause = join_clause
      )


    resultset <-
      qOMOP(
        sql_statement = sql_statement,
        verbose = verbose,
        render_sql = render_sql,
        skip_cache = TRUE
      )
    resultset
  }



#' @title
#' Draft a Join On
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
