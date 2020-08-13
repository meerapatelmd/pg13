#' Query a List of SQL Statements
#' @description
#' Iteratively query over a list of SQL Statements such as the object returned by \code{\link{parseSQL}}.
#' @import progress
#' @param conn Connection object
#' @param sqlList list object of queries
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbGetQuery function
#' @export

queryList <-
    function(conn,
             sqlList,
             verbose = TRUE,
             progressBar = TRUE,
             ...) {

            if (!is.list(sqlList)) {

                    stop("'sqlList' must be a list")

            }

            if (progressBar) {

                    pb <- progress::progress_bar$new(total = length(sqlList),
                                                     format = "[:bar] :elapsedfull :current/:total (:percent)",
                                                     clear = FALSE)
                    pb$tick(0)
                    Sys.sleep(.2)

            }

            output <- list()
            for (i in 1:length(sqlList)) {

                    sql <- sqlList[[i]]

                    if (verbose) {
                            secretary::typewrite(sql)
                    }

                    if (progressBar) {
                            pb$tick()
                            Sys.sleep(.2)
                    }

                    output[[i]] <-
                    query(conn = conn,
                          sql_statement = sql,
                          ...)

            }

            output
    }
