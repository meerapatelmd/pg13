#' Execute SQL
#' @description This function differs from the send() and query() functions in that it provides additional features such as a progress bar and time estimations.
#' @import DatabaseConnector
#' @export

execute <-
        function(conn,
                 sql_statement,
                 profile = FALSE,
                 progressBar = TRUE,
                 reportOverallTime = TRUE,
                 ...) {
                DatabaseConnector::executeSql(connection = conn,
                                              sql = sql_statement,
                                              profile = profile,
                                              progressBar = progressBar,
                                              reportOverallTime = reportOverallTime,
                                              ...)
        }
