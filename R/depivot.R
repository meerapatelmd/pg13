#' @title
#' Depivot a Table
#'
#' @rdname depivot_table
#' @example inst/example/depivot.R
#' @export



depivot_table <-
  function(conn,
           conn_fun,
           schema,
           table,
           names_to_columns,
           names_to,
           values_to_column,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE) {
    fields <- ls_fields(
      conn = conn,
      conn_fun = conn_fun,
      schema = schema,
      table = table,
      verbose = verbose,
      render_sql = render_sql
    )

    # <---! Classify Fields based on being depivoted or not --->
    id_columns <- fields[!(fields %in% names_to_columns)]

    output <- list()
    for (names_to_column in names_to_columns) {
      if (length(id_columns) > 0) {
        sql_statement <-
          SqlRender::render(

            "
                                        SELECT
                                                @id_columns,
                                                '@names_to_column' AS @names_to,
                                                @names_to_column AS @values_to_column
                                        FROM @schema.@table
                                        ",
            id_columns = id_columns,
            names_to_column = names_to_column,
            names_to = names_to,
            values_to_column = values_to_column,
            schema = schema,
            table = table
          )
      } else {
        sql_statement <-
          SqlRender::render(

            "
                                        SELECT
                                                '@names_to_column' AS @names_to,
                                                @names_to_column AS @values_to_column
                                        FROM @schema.@table
                                        ",
            names_to_column = names_to_column,
            names_to = names_to,
            values_to_column = values_to_column,
            schema = schema,
            table = table
          )
      }


      output[[1 + length(output)]] <-
        query(
          conn = conn,
          conn_fun = conn_fun,
          sql_statement = sql_statement,
          verbose = verbose,
          render_sql = render_sql,
          render_only = render_only
        )
    }

    dplyr::bind_rows(output)
  }
