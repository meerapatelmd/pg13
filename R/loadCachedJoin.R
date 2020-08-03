#' Load a Cached Join Resultset
#' @description This function will load the cached resultset from a SQL query created by the buildJoinQuery function to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param ... Arguments passed to the buildJoinQuery function
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

loadCachedJoin <-
        function(...,
                 db,
                 schema) {

                R.cache::loadCache(key = list(...),
                                   dirs = paste0(db, "/", schema))
        }
