#' Typewrite SQL
#' @importFrom secretary typewrite greenTxt
#' @importFrom stringr str_replace_all
#' @noRd

typewrite_sql <-
        function (sql_statement)
        {
                sql_statement <- stringr::str_replace_all(sql_statement,
                                                          "[\r\n\t]{1,}|\\s{2,}", " ")
                sql_statement <- trimws(sql_statement)
                secretary::typewrite(secretary::greenTxt("SQL:"), sql_statement)
        }

#' Typewrite Activity
#' @importFrom secretary typewrite greenTxt
#' @importFrom stringr str_replace_all
#' @noRd

typewrite_activity <-
        function (activity)
        {
                secretary::typewrite(secretary::yellowTxt(activity))
        }

#' @noRd

typewrite_alert_danger <-
        function(text) {
                cat(sprintf("[%s]", as.character(Sys.time())), "\t")
                cli::cli_alert_danger(text = text)
        }


#' @noRd

typewrite_alert_success <-
        function(text) {
                cat(sprintf("[%s]", as.character(Sys.time())), "\t")
                cli::cli_alert_success(text = text)
        }
