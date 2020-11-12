#' @title
#' Query Function Factory
#'
#' @description
#' Write a query function that automatically connects and disconnects on exit based on the connection details parameters given.
#'
#' @return
#' Query function
#'
#' @rdname q_ff
#' @export
#' @importFrom secretary typewrite

q_ff <-
        function(user,
                 password,
                 port,
                 server,
                 cache_dirs = NULL) {


        if (!is.null(cache_dirs)) {
        function(
                sql_statement,
                cache_only = FALSE,
                skip_cache = FALSE,
                override_cache = TRUE,
                verbose = TRUE,
                render_sql = TRUE,
                hrs_expired = 16) {


                conn <- connect(user = user,
                                password = password,
                                port = port,
                                server = server,
                                verbose = verbose)

                on.exit(expr = dc(conn = conn,
                                  verbose = verbose)
                )



                if (skip_cache) {

                        if (verbose) {
                                secretary::typewrite("Skipping cache")
                        }


                        resultset <-
                                query(
                                        conn = conn,
                                        sql_statement = sql_statement,
                                        render_sql = render_sql)


                } else {
                        if (override_cache) {

                                if (verbose) {
                                        secretary::typewrite("Overriding cache")
                                }

                                resultset <-
                                        query(
                                                conn = conn,
                                                sql_statement = sql_statement,
                                                render_sql = render_sql)


                                cache(object = resultset,
                                      sql_statement = sql_statement,
                                      dirs = cache_dirs)

                        } else {

                                if (verbose) {

                                        secretary::typewrite("Loading cache")

                                }

                                resultset <- loadCache(sql_statement = sql_statement,
                                                       hrs_expired = hrs_expired,
                                                       dirs = cache_dirs)

                                if (!cache_only) {

                                        if (is.null(resultset)) {

                                                if (verbose) {

                                                        secretary::typewrite("Querying IDB")

                                                }


                                                resultset <-
                                                        query(
                                                                conn = conn,
                                                                sql_statement = sql_statement,
                                                                render_sql = render_sql)

                                                cache(object = resultset,
                                                      sql_statement = sql_statement,
                                                      dirs = cache_dirs)
                                        }
                                }

                        }
                }

                resultset

        }

        } else {

                function(
                        sql_statement,
                        verbose = TRUE,
                        render_sql = TRUE) {


                        conn <- connect(user = user,
                                        password = password,
                                        port = port,
                                        server = server,
                                        verbose = verbose)

                        on.exit(expr = dc(conn = conn,
                                          verbose = verbose)
                        )

                        query(
                                conn = conn,
                                sql_statement = sql_statement,
                                render_sql = render_sql)





        }

        }
        }

