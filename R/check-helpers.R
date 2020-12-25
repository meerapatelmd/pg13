#' @noRd

typewrite_alert_danger <-
        function(text) {
                cat(sprintf("[%s]", as.character(Sys.time())), "\t")
                cli::cli_alert_danger(text = text)
        }


#' @noRd

typewrite_alert_success <-
        function(text) {
                cat(sprintf("[%s]", as.character(Sys.time())), "\t")
                cli::cli_alert_success(text = text)
        }


#' @title
#' Check Connection Status
#' @keywords internal
#' @rdname check_conn_status

check_conn_status <-
        function(conn) {

                if (is.null(conn)) {

                         typewrite_alert_danger(text = "NULL connection")

                } else if (is_conn_open(conn = conn)) {

                        typewrite_alert_success(text = "Open connection")

                } else {

                        typewrite_alert_danger(text = "Closed connection")
                }
        }


#' @title
#' Check if a Connection is JDBC
#' @keywords internal
#' @rdname check_conn_type

check_conn_type <-
        function(conn) {
                if (!.hasSlot(conn, name = "jConnection")) {
                        typewrite_alert_danger(text = "Connection not JDBC")
                } else {
                       typewrite_alert_success(text = "JDBC connection")
                }
        }


#' @title
#' Check that the data has rows
#' @keywords internal
#' @rdname check_data_rows

check_data_rows <-
        function(data) {


                if (nrow(data) == 0) {

                        typewrite_alert_danger(text = "'data' has 0 rows")

                } else {


                        typewrite_alert_success(text = "data has more than 0 rows")
                }
        }

#' @title
#' Check that the outgoing data has rows
#' @keywords internal
#' @rdname check_output_rows

check_output_rows <-
        function(data) {


                if (nrow(data) == 0) {

                        typewrite_alert_danger(text = sprintf("Data '%s' has 0 rows", deparse(substitute(data))))

                } else {


                        typewrite_alert_success(text = sprintf("Data '%s' has more than 0 rows", deparse(substitute(data))))
                }
        }

#' @title
#' Check that the incoming data has rows
#' @keywords internal
#' @rdname check_input_rows

check_input_rows <-
        function(data) {

                if (nrow(data) == 0) {

                        typewrite_alert_danger(text = "Returned data has 0 rows")

                } else {


                        typewrite_alert_success(text = "Returned data has more than 0 rows")
                }
        }


#' @title
#' Check a field name
#' @description
#' Check that a field name is not a reserved SQL word.
#' @keywords internal
#' @rdname check_field_name


check_field_name <-
        function(field_name) {

                name <- toupper(field_name)

                if (name %in% reserved_words()) {

                        typewrite_alert_danger(text = sprintf("Field name '%s' is a reserved word", tolower(name)))

                } else {

                        typewrite_alert_success(text = sprintf("Field name '%s' is not a reserved word", tolower(name)))

                }

        }


#' @title
#' Check Table Name
#' @description
#' Check that a table name is not a reserved SQL word.
#' @keywords internal
#' @rdname check_table_name


check_table_name <-
        function(table_name) {

                name <- toupper(table_name)

                if (name %in% reserved_words()) {

                        typewrite_alert_danger(text = sprintf("Table name '%s' is a reserved word", table_name))

                } else {

                        typewrite_alert_success(text = sprintf("Table name '%s' is not a reserved word", table_name))

                }

        }


#' @title
#' Check a multiple field names
#' @description
#' Check that more than 1 field name is not a reserved SQL word.
#' @keywords internal
#' @rdname check_field_names


check_field_names <-
        function(field_names) {

                for (i in seq_along(field_names)) {

                        check_field_name(field_name = field_names[i])
                }
        }
