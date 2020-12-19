#' @keywords internal
#' @rdname check_conn

check_conn <-
        function(conn) {

                cat("\t\t\t")

                if (is.null(conn)) {

                        cli::cli_alert_danger(text = "NULL connection")

                } else if (is_conn_open(conn = conn)) {

                        cli::cli_alert_success(text = "Open connection")

                } else {

                        cli::cli_alert_danger(text = "Closed connection")
                }
        }


#' @keywords internal
#' @rdname check_conn_type

check_conn_type <-
        function(conn) {
                if (!.hasSlot(conn, name = "jConnection")) {
                        cli::cli_alert_danger(text = "Connection not JDBC")
                } else {
                        cli::cli_alert_success(text = "JDBC connection")
                }
        }


#' @keywords internal
#' @rdname check_data_rows

check_data_rows <-
        function(data) {

                cat("\t\t\t")

                if (nrow(data) == 0) {

                        cli::cli_alert_danger(text = "'data' has 0 rows")

                } else {


                        cli::cli_alert_success(text = "data")
                }
        }


#' @keywords internal
#' @rdname check_reserve_names


check_reserved_names <-
        function(string) {


                cat("\t\t\t")
                string <- toupper(string)

                if (string %in% reserved_words()) {

                        cli::cli_alert_danger(text = sprintf("'%s' is a reserved word",
                                                              string[string %in% reserved_words()]))

                } else {

                        cli::cli_alert_success(text = string)

                }


        }
