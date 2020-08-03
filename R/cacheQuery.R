#' Cache a Query Resultset
#' @description This function will cache the resultset from a SQL query to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

cacheQuery <-
        function(.data,
                 sqlQuery,
                 db) {

                R.cache::saveCache(object = .data,
                                   key = list(sqlQuery),
                                   dirs = db)
        }
