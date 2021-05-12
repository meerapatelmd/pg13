#' @title
#' Search a Table for a Value
#'
#' @description
#' Loop a query for a set of one or more values in a table across all the existing fields or optionally, a subset of the fields. Both the values and the table fields are ensured compatibility by 1. Converting each value in the `values` argument to the character class and 2. Casting each table field as varchar in the query.
#'
#' @inheritParams base_args
#' @param case_insensitive  If TRUE, both sides of the query are converted to lowercase.
#' @param values            Vector of length 1 or greater to search for.
#' @param ...               (Optional) Character strings of 1 or more fields in the table to search in.
#'
#' @importFrom rlang list2
#'
#' @rdname search_table
#' @family table functions
#' @family search functions
#' @example inst/example/table.R

search_table <-
  function(conn,
           conn_fun,
           schema,
           table,
           ...,
           values,
           case_insensitive = TRUE,
           verbose = TRUE,
           render_sql = TRUE) {
    if (!missing(conn_fun)) {
      conn <- eval(rlang::parse_expr(conn_fun))
      on.exit(dc(
        conn = conn,
        verbose = verbose
      ),
      add = TRUE,
      after = TRUE
      )
    }

    check_conn(conn = conn)

    # Format Values for SQL
    values <- as.character(values)

    if (case_insensitive) {
      values <- tolower(values)
    }
    values <- s_quo(values)



    # Get Fields vector to loop over for each SQL query
    if (missing(...)) {
      fields <- ls_fields(
        conn = conn,
        schema = schema,
        table = table,
        verbose = verbose,
        render_sql = FALSE
      )
    } else {
      fields <- unlist(rlang::list2(...))
    }


    sql_statements <- list()
    for (field in fields) {
      i <- 1 + length(sql_statements)

      if (case_insensitive) {
        sql_statements[[i]] <-
          SqlRender::render(
            "
                                                    SELECT *
                                                    FROM @schema.@table t
                                                    WHERE LOWER(t.@Field::varchar) IN (@values)
                                                    ;
                                                    ",
            schema = schema,
            table = table,
            Field = field,
            values = values
          )
      } else {
        sql_statements[[i]] <-
          SqlRender::render(
            "
                                                SELECT *
                                                FROM @schema.@table t
                                                WHERE t.@Field::varchar IN (@values)
                                                ;
                                                ",
            schema = schema,
            table = table,
            Field = field,
            values = values
          )
      }
    }


    resultsets <- list()
    for (i in seq_along(sql_statements)) {
      resultsets[[i]] <-
        suppressWarnings(
          query(
            conn = conn,
            sql_statement = sql_statements[[i]],
            verbose = verbose,
            render_sql = render_sql
          )
        )
    }

    names(resultsets) <- fields

    metrics <-
      resultsets %>%
      purrr::map(~ tibble::as_tibble_col(x = nrow(.), column_name = "Rows")) %>%
      dplyr::bind_rows(.id = "Field")


    list(
      ROWS = metrics,
      RESULTSETS = resultsets %>%
        purrr::keep(~ nrow(.) > 0)
    )
  }
