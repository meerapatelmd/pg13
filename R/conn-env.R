#' @title
#' PG13 Environment S4 Class
#'
#' @description
#' This an optional environment class that stores all the connections using this package.
#'
#' @export pg13_env


pg13_env <- setClass("pg_env",
                   contains = c("environment"),
                   slots = c(update_datetime = "POSIXct"))




setMethod(f = "[[<-",
          signature = c("pg_env", "character", "missing"),
          function(x, i, value) {
                  ev <- as(x, "environment")
                  ev[[i]] <- value  #update the object in the environment
                  x@update_datetime <- Sys.time() # and the update time
                  x})


pg13_connection_env <- pg13_env(update_datetime = Sys.time())
