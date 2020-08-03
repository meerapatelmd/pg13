#' Load a Cached Query Resultset
#' @description This function will load the cached resultset from a SQL query to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

loadCachedQuery <-
        function(sqlQuery,
                 db) {

                R.cache::loadCache(key = list(sqlQuery),
                                   dirs = db)
        }
