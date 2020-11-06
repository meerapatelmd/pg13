.onLoad <-
        function(lib,pkg) {
                safely.show <<- purrr::quietly(methods::show)
                is_conn_open <<-
                        function(conn, verbose = TRUE) {

                                output <- safely.show(conn)

                                if (is.null(output$error)) {
                                        TRUE
                                } else {
                                        FALSE
                                }

                        }

        }
