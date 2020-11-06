.onLoad <-
        function(lib,pkg) {
                safely.print <<- purrr::quietly(print)
                is_conn_open <<-
                        function(conn, verbose = TRUE) {

                                output <- safely.print(conn)

                                if (is.null(output$error)) {
                                        TRUE
                                } else {
                                        FALSE
                                }

                        }

        }
