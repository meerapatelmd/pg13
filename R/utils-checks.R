#' @title
#' Check Connection Status
#' @keywords internal
#' @rdname check_conn_status

check_conn_status <-
  function(conn,
           log_file = "",
           sep = "\n",
           append = TRUE) {
    if (is.null(conn)) {
      typewrite_alert_danger(text = "NULL connection",
                             log_file = log_file,
                             sep = sep,
                             append = append
                             )
    } else if (is_conn_open(conn = conn)) {
      typewrite_alert_success(text = "Open connection",
                              log_file = log_file,
                              sep = sep,
                              append = append)
    } else {
      typewrite_alert_danger(text = "Closed connection",
                             log_file = log_file,
                             sep = sep,
                             append = append)
    }
  }


#' @title
#' Check if a Connection is JDBC
#' @keywords internal
#' @rdname check_conn_type

check_conn_type <-
  function(conn,
           log_file = "",
           sep = "\n",
           append = TRUE) {
    if (!.hasSlot(conn, name = "jConnection")) {
      typewrite_alert_danger(text = "Connection not JDBC",
                             log_file = log_file,
                             sep = sep,
                             append = append)
    } else {
      typewrite_alert_success(text = "JDBC connection",
                              log_file = log_file,
                              sep = sep,
                              append = append)
    }
  }


#' @title
#' Check that the incoming data has rows
#' @keywords internal
#' @rdname check_rows

check_rows <-
  function(data,
           log_file = "",
           sep = "\n",
           append = TRUE) {
    if (nrow(data) == 0) {
      typewrite_alert_danger(text = "Returned data has 0 rows.",
                             log_file = log_file,
                             sep = sep,
                             append = append)
    } else if (nrow(data) == 1) {
      typewrite_alert_success(text = sprintf("Returned data has %s row.", nrow(data)),
                              log_file = log_file,
                              sep = sep,
                              append = append)
    } else {
      typewrite_alert_success(text = sprintf("Returned data has %s rows.", nrow(data)),
                              log_file = log_file,
                              sep = sep,
                              append = append)
    }
  }


#' @title
#' Check that the table has any rows
#' @export
#' @rdname check_source_rows

check_source_rows <-
  function(sql_statement,
           conn,
           log_file = "",
           sep = "\n",
           append = TRUE) {

    parse_tablenames <-
      function(sql_statement) {

        sql_tokens <-
        strsplit(sql_statement,
                 split = " ") %>%
          unlist() %>%
          trimws("both") %>%
          tolower()

        from_join_indexes <-
          grep(pattern = "^from$|^join$",
               x = sql_tokens)
        next_word_indexes <-
          from_join_indexes+1

        sql_tokens[next_word_indexes] %>%
          grep(pattern = "^[A-Za-z]{1,}.*?[.]{1}[A-Za-z].*$",
               value = TRUE) %>%
          stringr::str_remove_all(pattern = "[;]{1}") %>%
          unique()


      }


    table_paths <- parse_tablenames(sql_statement = sql_statement)

    output <-
      vector(mode = "list",
             length = length(table_paths))
    names(output) <- table_paths

    for (table_path in table_paths) {

      output[[table_path]] <-
        tryCatch(
          with_timeout(
              query(conn = conn,
                          checks = "",
                          sql_statement =
                            glue::glue("SELECT COUNT(*) FROM {table_path} LIMIT 1;")),
              cpu = 30,
              elapsed = 30),
          error = function(e) tibble::tibble(count = NA_character_))



    }

    output <-
      output %>%
      dplyr::bind_rows(.id = "table_path") %>%
      dplyr::filter(count == 0)

    if (nrow(output) == 0) {
      typewrite_alert_success(text = glue::glue("The following detected source tables all had greater than 0 rows: {glue::glue_collapse(table_paths, sep = ', ')}"),
                             log_file = log_file,
                             sep = sep,
                             append = append)
    } else {

      typewrite_activity(glue::glue("Source tables detected from the SQL string: {glue::glue_collapse(table_paths, sep = ', ')}"),
                              log_file = log_file,
                              sep = sep,
                              append = append)

      typewrite_alert_danger(text = glue::glue("Of the tables above, the following has a count of 0: {glue::glue_collapse(output$table_path, sep = ', ')}"),
                              log_file = log_file,
                              sep = sep,
                              append = append)
    }
  }

#' @title
#' Check a field name
#' @description
#' Check that a field name is not a reserved SQL word.
#' @keywords internal
#' @rdname check_field_name


check_field_name <-
  function(field_name,
           log_file = "",
           sep = "\n",
           append = TRUE) {
    name <- toupper(field_name)

    if (name %in% reserved_words()) {
      typewrite_alert_danger(text = sprintf("Field name '%s' is a reserved word", tolower(name)),
                             log_file = log_file,
                             sep = sep,
                             append = append)
    }
  }


#' @title
#' Check Table Name
#' @description
#' Check that a table name is not a reserved SQL word.
#' @keywords internal
#' @rdname check_table_name


check_table_name <-
  function(table_name,
           log_file = "",
           sep = "\n",
           append = TRUE) {
    name <- toupper(table_name)

    if (name %in% reserved_words()) {
      typewrite_alert_danger(text = sprintf("Table name '%s' is a reserved word", table_name),
                             log_file = log_file,
                             sep = sep,
                             append = append)
    }
  }


#' @title
#' Check a multiple field names
#' @description
#' Check that more than 1 field name is not a reserved SQL word.
#' @keywords internal
#' @rdname check_field_names


check_field_names <-
  function(field_names,
           log_file = "",
           sep = "\n",
           append = TRUE) {
    for (i in seq_along(field_names)) {
      check_field_name(field_name = field_names[i],
                       log_file = log_file,
                       sep = sep,
                       append = append)
    }
  }
