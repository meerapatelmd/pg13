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
