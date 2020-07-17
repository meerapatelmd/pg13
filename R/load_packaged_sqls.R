#' Load all packaged SQLs
#' @export

load_package_sqls <-
        function(package) {
                base <- system.file('sql', package=package)
                sqls <- dir(base, "*sql", f=TRUE)
                return(sqls)
        }
