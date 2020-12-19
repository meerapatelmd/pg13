#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



#' Wrap with Single Quotes
#'
#' @name sQuo
#' @rdname s_quo
#' @keywords internal
#' @export

s_quo <-
        function(vector) {
                vector <- as.character(vector)
                paste0("'", vector, "'")
        }


#' @title
#' Affix the System Date to a String
#'
#' @description
#' Date is affixed at the end of a string in "YYYY_mm_dd" Format
#'
#' @importFrom stringr str_replace_all
#'
#' @export

affix_date <-
        function(string) {
                paste0(name, "_", stringr::str_replace_all(as.character(Sys.Date()), "[-]{1}", "_"))
        }


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
