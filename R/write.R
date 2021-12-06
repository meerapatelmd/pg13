#' @title
#' Append a Table
#'
#' @description
#' Like the writeTable function, this function is a wrapper
#' around a DatabaseConnector function rather than one where
#' a SQL statement is rendered using the SqlRender package.
#' This function performs the additional step of converting
#' all inputs to the data.frame class, especially in cases
#' where the input is a tibble.
#'
#' @inheritParams base_args
#' @param           ...     Additional arguments passed to `DatabaseConnector::dbAppendTable()`
#'
#' @rdname append_table
#' @family table functions
#'
#' @importFrom DatabaseConnector dbAppendTable
#'
#' @export
#' @example inst/example/table.R
#' @family table functions
#' @family write functions

append_table <-
  function(conn,
           conn_fun = "pg13::local_connect()",
           checks = c("conn_status", "conn_type", "rows"),
           schema,
           table,
           data,
           verbose = TRUE,
           render_sql = TRUE,
           log_file = "",
           append_log = TRUE,
           sep_log = "\n") {
    if (missing(conn)) {
      conn <- eval(rlang::parse_expr(conn_fun))
      on.exit(dc(
        conn = conn,
        verbose = verbose
      ),
      add = TRUE,
      after = TRUE
      )
    }

    # +++
    # Checks
    # +++
    if ("conn_status" %in% checks) {
      check_conn_status(conn = conn,
                        log_file = log_file,
                        append = append_log,
                        sep = sep_log)
    }

    if ("conn_type" %in% checks) {
      check_conn_type(conn = conn,
                      log_file = log_file,
                      append = append_log,
                      sep = sep_log)
    }
    if ("rows" %in% checks) {
      check_rows(data = data,
                 log_file = log_file,
                 append = append_log,
                 sep = sep_log)
    }

    schema_table <- sprintf("%s.%s", schema, table)


    if (render_sql) {
      typewrite_sql(sql_statement = "N/A",
                    log_file = log_file,
                    append = append_log,
                    sep = sep_log)
    }

    if (verbose) {
      typewrite_activity(sprintf("Appending %s...", schema_table),
                         log_file = log_file,
                         append = append_log,
                         sep = sep_log)
    }

    DatabaseConnector::dbAppendTable(
      conn = conn,
      name = schema_table,
      value = as.data.frame(data)
    )

    if (verbose) {
      typewrite_activity(sprintf("Appending %s...complete", schema_table),
                         log_file = log_file,
                         append = append_log,
                         sep = sep_log)
    }
  }


#' @title
#' Write a Table
#'
#' @description
#' Unlike the dropTable and renameTable functions, this
#' function is a wrapper around the `DatabaseConnector::dbWriteTable()`
#' function rather than one where a SQL statement is
#' rendered using the SqlRender package. This function that
#' converts all inputs to a dataframe, especially in cases
#' where the input is a tibble, in which case an error would
#' be thrown when writing.
#'
#' @inheritParams base_args
#' @param       ...     Additional arguments passed to `DatabaseConnector::dbWriteTable()`
#'
#' @importFrom DatabaseConnector dbWriteTable
#'
#' @rdname write_table
#'
#' @export
#' @example inst/example/table.R
#' @family table functions
#' @family write functions

write_table <-
  function(conn,
           conn_fun = "pg13::local_connect()",
           checks = c("conn_status", "conn_type", "rows", "names"),
           schema,
           table_name,
           data,
           drop_existing = FALSE,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE,
           log_file = "",
           append_log = TRUE,
           sep_log = "\n") {

    if (missing(conn)) {
      conn <- eval(rlang::parse_expr(conn_fun))
      on.exit(dc(
        conn = conn,
        verbose = verbose
      ),
      add = TRUE,
      after = TRUE
      )
    }

    # +++
    # Checks
    # +++
    if ("conn_status" %in% checks) {
      check_conn_status(conn = conn,
                        log_file = log_file,
                        append = append_log,
                        sep = sep_log)
    }

    if ("conn_type" %in% checks) {
      check_conn_type(conn = conn,
                      log_file = log_file,
                      append = append_log,
                      sep = sep_log)
    }
    if ("rows" %in% checks) {
      check_rows(data = data,
                 log_file = log_file,
                 append = append_log,
                 sep = sep_log)
    }

    if ("names" %in% checks) {
      check_table_name(table_name = table_name,
                       log_file = log_file,
                       append = append_log,
                       sep = sep_log)
      check_field_names(field_names = colnames(data),
                        log_file = log_file,
                        append = append_log,
                        sep = sep_log)
    }

    if (drop_existing) {

      drop_table(
        conn = conn,
        conn_fun = conn_fun,
        schema = schema,
        table = table_name,
        if_exists = TRUE,
        verbose = verbose,
        render_sql = render_sql,
        render_only = render_only
      )

    }

    schema_table_name <- sprintf("%s.%s", schema, table_name)


    if (!render_only) {
      if (verbose) {
        typewrite_activity(sprintf("Writing %s...", schema_table_name),
                           log_file = log_file,
                           append = append_log,
                           sep = sep_log)
      }

      DatabaseConnector::dbWriteTable(
        conn = conn,
        name = schema_table_name,
        value = as.data.frame(data)
      )

      if (verbose) {

        typewrite_activity(sprintf("Writing %s...complete", schema_table_name),
                           log_file = log_file,
                           append = append_log,
                           sep = sep_log)
      }

    } else {

      if (verbose) {

        typewrite_activity(sprintf("No SQL to render for write_table()"),
                           log_file = log_file,
                           append = append_log,
                           sep = sep_log)

      }
    }
  }
