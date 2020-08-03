#' Cache a Resultset from a Join
#' @description This function will cache the resultset from a SQL query created by buildJoinQuery to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param ... Arguments passed to the buildJoinQuery function
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

cacheJoin <-
        function(.data,
                 ...,
                 db,
                 schema) {

                R.cache::saveCache(object = .data,
                                   key = list(...),
                                   dirs = paste0(db, "/", schema))
        }
