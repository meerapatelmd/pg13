#' Execute SQL
#' @description This function differs from the send() and query() functions in that it provides additional features such as a progress bar and time estimations.
#' @importFrom DatabaseConnector executeSql
#' @export

execute <-
        function(conn,
                 sql_statement,
                 profile = TRUE,
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
