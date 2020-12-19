#' Cache a Resultset from a Join
#' @description This function will cache the resultset from a SQL query created by buildJoinQuery to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param ... Arguments passed to the buildJoinQuery function
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

cache_join <-
        function(.data,
                 ...,
                 db,
                 schema) {

                R.cache::saveCache(object = .data,
                                   key = list(...),
                                   dirs = paste0(db, "/", schema))
        }





#' Cache a Query Resultset
#' @description This function will cache the resultset from a SQL query to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

cache_query <-
        function(.data,
                 sqlQuery,
                 db) {

                R.cache::saveCache(object = .data,
                                   key = list(sqlQuery),
                                   dirs = db)
        }





#' Clear a Cache
#' @description This function will clear a cache subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

clear_cache <-
        function(db) {

                R.cache::clearCache(path = getCachePath(db))


        }





#' Load a Cached Join Resultset
#' @description This function will load the cached resultset from a SQL query created by the buildJoinQuery function to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param ... Arguments passed to the buildJoinQuery function
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

load_cached_join <-
        function(...,
                 db,
                 schema) {

                R.cache::loadCache(key = list(...),
                                   dirs = paste0(db, "/", schema))
        }





#' Load a Cached Query Resultset
#' @description This function will load the cached resultset from a SQL query to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

load_cached_query <-
        function(sqlQuery,
                 db) {

                R.cache::loadCache(key = list(sqlQuery),
                                   dirs = db)
        }





