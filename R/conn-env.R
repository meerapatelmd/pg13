#' @title
#' PG13 Environment S4 Class
#'
#' @description
#' This an optional environment class that stores all the connections using this package.
#'
#' @export
#' @rdname pg13_env


pg13_env <- setClass("pg_env",
                   contains = c("environment"),
                   slots = c(update_datetime = "POSIXct"))


#' @export
#' @rdname pg13_env_methods

setMethod(f = "[[<-",
          signature = c("pg_env", "character", "missing"),
          function(x, i, value) {
                  ev <- as(x, "environment")
                  ev[[i]] <- value  #update the object in the environment
                  x@update_datetime <- Sys.time() # and the update time
                  x})


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn_name Name of the connection object in the `pg13_connection_env` environment
#' @param conn_fun Function call for the connection as a character string.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[secretary]{c("typewrite", "typewrite")}},\code{\link[secretary]{italicize}}
#'  \code{\link[cli]{cli_alert}}
#'  \code{\link[rlang]{parse_expr}}
#' @rdname open_conn
#' @export
#' @importFrom secretary typewrite italicize
#' @importFrom cli cli_alert_danger cli_alert_info
#' @importFrom rlang parse_expr

open_conn <-
        function(conn_fun,
                 conn_name,
                 verbose = TRUE) {


                if (!("pg13_connection_env" %in% ls(envir = globalenv()))) {

                        pg13_connection_env <<-
                                pg13_env(update_datetime = Sys.time())

                }

                # output <- do.call(what = ls,
                #                 args = list(envir = pg13_connection_env),
                #                 envir = parent.frame())
                # print(output)
                # secretary::press_enter()

                if (conn_name %in% ls(envir = pg13_connection_env)) {

                       if (is_conn_open(pg13_connection_env[[conn_name]])) {

                               cli::cli_alert_danger(sprintf("Open connection '%s' already exists.", conn_name))


                       } else {


                               secretary::typewrite(cli::cli_alert_info("Closed connection '%s' already exists."))
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


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param all Close all connections in the `pg13_connection_env` environment?
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{list2}}
#' @rdname close_conn
#' @export
#' @importFrom rlang list2

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



show_conn <-
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
