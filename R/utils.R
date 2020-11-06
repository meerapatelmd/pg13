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
#' @rdname sQuo
#' @keywords internal
#' @export

sQuo <-
        function(vector) {
                vector <- as.character(vector)
                paste0("'", vector, "'")
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
