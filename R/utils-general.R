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
