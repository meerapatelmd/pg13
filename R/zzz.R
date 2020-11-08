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


                quietly.connDB <<- purrr::quietly(connDB)

                connect <<-
                        function(user,
                                 password,
                                 port,
                                 server,
                                 verbose = TRUE) {

                                conn <- quietly.connDB(user = user,
                                                      password = password,
                                                      port = port,
                                                      server = server)

                                if (verbose) {

                                        secretary::typewrite(conn$output)

                                }

                                conn$result

                        }


                connect_ff <<-
                        function(user,
                                 password,
                                 port,
                                 server) {

                                function(verbose) {

                                        connect(
                                                user = user,
                                                password = password,
                                                port = port,
                                                server = server
                                        )
                                }

                        }


        }
