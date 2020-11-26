#' @title
#' Query Function Factory
#'
#' @description
#' Write a query function that automatically connects and disconnects on exit based on the connection details parameters given.
#'
#' @return
#' Query function
#'
#' @rdname q_creds_ff
#' @export
#' @importFrom secretary typewrite

q_creds_ff <-
        function(user,
                 password,
                 port,
                 server) {

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

                do.call(
                        on.exit,
                        list(substitute(dc(conn = conn,
                                           verbose = verbose)),
                             add = TRUE,
                             after = FALSE),
                        envir = parent.frame()
                )


        }
        }

