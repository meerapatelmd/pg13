.onLoad <-
        function(lib,pkg) {

                safely.print <<- purrr::safely(print,
                                               quiet = TRUE)
                quietly.safely.print <<- purrr::quietly(safely.print)


                is_conn_open <<-
                        function(conn) {

                                output <- quietly.safely.print(conn)

                                if (is.null(output$result$error)) {
                                        TRUE
                                } else {
                                        FALSE
                                }

                        }

        }
