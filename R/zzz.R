.onLoad <-
        function(lib,pkg) {

                safely.print <<- purrr::safely(print,
                                               quiet = TRUE)


                is_conn_open <<-
                        function(conn) {

                                output <- suppressMessages(safely.print(conn))

                                if (is.null(output$error)) {
                                        TRUE
                                } else {
                                        FALSE
                                }

                        }

        }
