#' @title
#' PG13 Environment S4 Class
#'
#' @description
#' This an optional environment class that stores all the connections using this package.
#'
#' @export
#' @rdname pg13_env
#' @family pg13 environment functions
#' @family pg13 class functions


pg13_env <- setClass("pg_env",
                   contains = c("environment"),
                   slots = c(update_datetime = "POSIXct"))


#' @title
#' Methods: PG13 environment
#' @export
#' @rdname pg13_env_methods
#' @family pg13 environment functions
#' @family pg13 class functions

setMethod(f = "[[<-",
          signature = c("pg_env", "character", "missing"),
          function(x, i, value) {
                  ev <- as(x, "environment")
                  ev[[i]] <- value  #update the object in the environment
                  x@update_datetime <- Sys.time() # and the update time
                  x})


#' @title
#' Open a Connection in the `pg13_connection_env`
#' @description
#' Connect to a database and assign it to the `pg13_connection_env`. This is a method to keep mulitple database connections organized during a crosswalk project. Existing connection objects will not be overwritten If the connection object of the provided name already exists. If the `pg13_connection_env` does not exist in the Global Environment, it will also be created at this point beforehand.
#'
#' @param conn_name Name of the connection object in the `pg13_connection_env` environment
#' @param conn_fun Function call for the connection as a character string such as "local_connect()".
#' @seealso
#'  \code{\link[secretary]{c("typewrite", "typewrite")}},\code{\link[secretary]{italicize}}
#'  \code{\link[rlang]{parse_expr}}
#' @rdname open_conn
#' @export
#' @importFrom secretary typewrite italicize
#' @importFrom rlang parse_expr
#' @family pg13 environment functions
#' @family pg13 class connection functions

open_conn <-
        function(conn_fun,
                 conn_name,
                 verbose = TRUE) {


                if (!("pg13_connection_env" %in% ls(envir = globalenv()))) {

                        pg13_connection_env <<-
                                pg13_env(update_datetime = Sys.time())

                }


                if (conn_name %in% ls(envir = pg13_connection_env)) {

                       if (is_conn_open(pg13_connection_env[[conn_name]])) {

                               typewrite_alert_danger(sprintf("Open connection '%s' already exists.", conn_name))


                       } else {


                               typewrite_alert_danger("Closed connection '%s' already exists.")
                               secretary::typewrite(secretary::italicize("Reconnecting..."))
                               on.exit(secretary::typewrite(secretary::italicize("Reconnecting...complete")))


                               assign(x = conn_name,
                                      value = eval(rlang::parse_expr(conn_fun)),
                                      envir = pg13_connection_env)

                       }



                } else {


                        assign(x = conn_name,
                               value = eval(rlang::parse_expr(conn_fun)),
                               envir = pg13_connection_env)


                }


        }


#' @title
#' Close Connections in `pg13_connection_env`
#' @description
#' Close the specified list of connection objects in the `pg13_connection_env` or close all connections, a feature is handy for making sure all connections are closed at the end of a session.
#' @param ... Names of the connections to close.
#' @param all Close all connections in the `pg13_connection_env` environment?
#' @seealso
#'  \code{\link[rlang]{list2}}
#' @rdname close_conn
#' @export
#' @importFrom rlang list2
#' @family pg13 environment functions
#' @family pg13 class connection functions

close_conn <-
        function(...,
                 all = FALSE,
                 verbose = TRUE) {

                if (all) {

                        conns <- ls(envir = pg13_connection_env)

                } else {

                        conns <- rlang::list2(...)

                }

                for (i in seq_along(conns)) {

                        dc(pg13_connection_env[[conns[[i]]]],
                           verbose = verbose,
                           remove = FALSE)

                }

        }


#' @title
#' See the Connections in `pg13_connection_env`
#' @description
#' See the connection objects in the `pg13_connection_env` along with their open status.
#' @seealso
#'  \code{\link[rlang]{list2}}
#' @rdname see_conn
#' @export
#' @importFrom rlang list2
#' @family pg13 environment functions
#' @family pg13 class connection functions

see_conn <-
        function() {
                output <- list()
                conn_names <- names(pg13_connection_env)

                for (i in seq_along(conn_names)) {

                        output[[i]] <-
                                structure(
                                        conn_names[i],
                                        is_open = is_conn_open(pg13_connection_env[[conn_names[i]]])
                                )
                }
                output
        }
