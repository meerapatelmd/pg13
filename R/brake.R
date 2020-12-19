#' @title
#' Create a Brake Function
#'
#' @description
#' Author new functions that stops and provides a custom message when a predicate evaluates to true.
#'
#' @param predicate             Evaluating function.
#' @param stop_message          Message to receive with the `predicate` evaluates to true.
#' @param arguments             (option) character vector of arguments for the new function. If missing, the new function will not have any arguments. Arguments in the `predicate` should also be considered. For example, if the predicate is "is.logical(x)", the `arguments` value should be "x".
#'
#' @return
#' Assigned function
#'
#' @seealso
#'  \code{\link[rlang]{parse_expr}},\code{\link[rlang]{missing_arg}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{set_names}}
#'
#' @rdname brake_ff
#' @family ff functions
#' @family brake functions
#'
#' @export
#'
#' @importFrom rlang parse_expr missing_arg
#' @importFrom purrr map set_names

brake_ff <-
        function(predicate,
                 stop_message,
                 arguments) {


                x <-
                function() {

                        if (eval(rlang::parse_expr(predicate))) {

                                stop(stop_message)

                        }

                }


                if (!missing(arguments)) {

                                output <-
                                        arguments %>%
                                        purrr::map(~ rlang::missing_arg()) %>%
                                        purrr::set_names(arguments)

                                formals(x) <- output

                }

                x
        }


#' @title
#' Brake if the Connection is Closed
#'
#' @description
#' Stop an operation if the supplied Connection Class object is closed.
#'
#' @param  conn    Connection object
#'
#' @return
#' Halted execution of a function or other process
#'
#' @details
#' This function is derived from the \code{\link{brake_ff}}.
#'
#' @seealso
#'  \code{\link{brake_ff}}
#' @rdname brake_closed_conn
#' @export


brake_closed_conn <-
        brake_ff(
                predicate = "!is_conn_open(conn)",
                stop_message = "connection is not open",
                arguments = "conn"
        )

