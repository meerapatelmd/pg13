#' @title
#' Check a Connection Object
#' @export
#' @rdname check_conn

check_conn <-
        function(conn) {

                check_conn_status(conn = conn)
                check_conn_type(conn = conn)
        }

#' @title
#' Check Outgoing Data
#' @export
#' @rdname check_outflow

check_outflow <-
        function(data,
                 table_name) {

                check_output_rows(data = data)

                if (!missing(table_name)) {

                        check_table_name(table_name = table_name)

                }

                field_names <- colnames(data)

                check_field_names(field_names = field_names)
        }

#' @title
#' Check Incoming Data
#' @export
#' @rdname check_inflow

check_inflow <-
        function(data) {

              check_input_rows(data = data)
        }
