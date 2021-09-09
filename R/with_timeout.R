#' @title
#' Wrap to include a timeout to evaluation.
#' @param expr Expression to evaluate.
#' @inheritParams base::setTimeLimit
#' @rdname with_timeout
#' @export

with_timeout <-
        function(expr, cpu, elapsed){

        expr <- substitute(expr)
        envir <- parent.frame()
        setTimeLimit(cpu = cpu, elapsed = elapsed, transient = TRUE)
        on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
        eval(expr, envir = envir)

}
