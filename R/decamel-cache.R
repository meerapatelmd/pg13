#' @title
#' Cache a Resultset
#'
#' @description
#' Cache a resultset with the SQL Statement as the key to a "fantasia" cache directory using a wrapper around the \code{\link[R.cache]{saveCache}} function.
#'
#' @param object        R object to cache.
#' @param sql_statement SQL Statement from which a hexadecimal hash code will be generated and appended to the cache filename.
#' @param dirs          Argument passed to \code{\link[R.cache]{saveCache}}.
#'
#' @return
#' Invisibly returns the path to the cache file.
#'
#' @importFrom R.cache saveCache
#'
#' @rdname cache
#' @family cache functions
#' @export

cache <-
        function(object,
                 sql_statement,
                 dirs) {

                key <- list(sql_statement)
                x <- r.cache::save_cache(object = object,
                                        key = key,
                                        dirs = dirs)
                invisible(x)
        }


#' @title
#' Load a Cached Resultset from a SQL Query
#'
#' @description
#' Load a resultset from a "fantasia" cache directory with the SQL Statement as the key if it was previously cached or is within expiration period as indicated by the `hrs_expired` argument. This function is a wrapper around the \code{\link[R.cache]{loadCache}} function.
#'
#' @details
#' An expiration period is incorporated by first determining the cache path and its mtime from the file information. If the cache file exists, the resultset will be returned if the the difference between the current time and the mtime of the cache file is less than or equal to the `hrs_expired` value.
#'
#' @param sql_statement SQL Statement from which a hexadecimal hash code was be generated and appended to the cache filename. See \code{\link{cache}} for more information.
#' @param dirs Argument passed to \code{\link[R.cache]{findCache}} and \code{\link[R.cache]{loadCache}}
#'
#' @return
#' Resultset if the cached file exists and is not expired and NULL otherwise.
#'
#' @rdname load_cache
#' @family cache functions
#' @importFrom R.cache findCache loadCache
#' @export

load_cache <-
        function(sql_statement,
                 hrs_expired = 8,
                 dirs) {

                key <- list(sql_statement)
                # Get path to cache file if it exists

                cache_file_path <-
                        R.cache::findCache(key = key,
                                           dirs = dirs)


                if (!is.null(cache_file_path)) {

                        is_expired <-
                                difftime(time1 = Sys.time(),
                                         time2 = file.info(cache_file_path)$mtime,
                                         units = "hours") > hrs_expired

                        if (is_expired) {

                                NULL

                        } else {

                                R.cache::loadCache(key = key,
                                                   dirs = dirs)

                        }

                } else {

                        R.cache::loadCache(key = key,
                                           dirs = dirs)
                }

        }
