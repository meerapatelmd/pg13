#' Clear a Cache
#' @description This function will clear a cache subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

clearCache <-
        function(db) {

                R.cache::clearCache(path = getCachePath(db))


        }
