#' @title
#' Read an Entire Table
#'
#' @description
#' Shortcut for a `SELECT *` SQL statement.
#'
#' @inheritParams base_args
#'
#' @export
#' @example inst/example/table.R
#' @family table functions
#' @family read functions
read_table <-
  function(conn,
           conn_fun,
           schema,
           table,
           verbose = TRUE,
           render_sql = TRUE,
           warn_no_rows = TRUE,
           render_only = FALSE,
           log_file = "",
           append_log = TRUE,
           sep_log = "\n") {
    sql_statement <- sprintf("SELECT * FROM %s.%s;", schema, table)


    if (verbose) {
      typewrite_activity(sprintf("Reading %s.%s...", schema, table),
                         log_file = log_file,
                         append = append_log,
                         sep = sep_log)
    }

    resultset <-
      query(
        conn = conn,
        conn_fun = conn_fun,
        sql_statement = sql_statement,
        verbose = verbose,
        render_sql = render_sql,
        warn_no_rows = warn_no_rows,
        render_only = render_only,
        log_file = log_file,
        append_log = append_log,
        sep_log = sep_log
      )

    if (verbose) {
      typewrite_activity(sprintf("Reading %s.%s...complete", schema, table),
                         log_file = log_file,
                         append = append_log,
                         sep = sep_log)
    }


    # <---! Already checked in the query function --->
    # check_inflow(data = resultset)
    resultset
  }
