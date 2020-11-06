.onLoad <-
        function(lib,pkg) {

                safely.print <<- purrr::safely(print)

                is_conn_open <<-
                        function(conn) {

                                output <- safely.print(conn)

                                if (is.null(output$error)) {
                                        TRUE
                                } else {
                                        FALSE
                                }

                        }

        }
