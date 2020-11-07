#' @title
#' Create a Flag Function
#'
#' @description
#' Author new functions that warns and provides a custom message when a predicate evaluates to true.
#'
#' @param predicate             Evaluating function.
#' @param warn_message          Message to receive with the `predicate` evaluates to true.
#' @param arguments             (option) character vector of arguments for the new function. If missing, the new function will not have any arguments. Arguments in the `predicate` should also be considered. For example, if the predicate is "is.logical(x)", the `arguments` value should be "x".
#' @param ...                   Additional arguments passed to the base warning function
#'
#' @return
#' Assigned function
#'
#' @seealso
#'  \code{\link[rlang]{parse_expr}},\code{\link[rlang]{missing_arg}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{set_names}}
#'
#' @rdname flag_ff
#' @family ff functions
#' @family flag functions
#'
#' @export
#'
#' @importFrom rlang parse_expr missing_arg
#' @importFrom purrr map set_names

flag_ff <-
        function(predicate,
                 warn_message,
                 arguments,
                 ...) {


                x <-
                        function() {

                                if (eval(rlang::parse_expr(predicate))) {

                                        warning(warn_message,
                                                ...)

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
#' Flag if Data Has 0 Rows
#'
#' @description
#' Receive a warning in the console if data has 0 rows.
#'
#' @rdname flag_no_rows
#' @export


flag_no_rows <-
        flag_ff(
                predicate = "nrow(data)==0",
                warn_message = "data has 0 rows",
                arguments = "data"
        )
