#' Append Current Date to a String
#' @description Date is appended in "YYYY_mm_dd" Format
#' @import stringr
#' @export

appendDate <-
        function(name) {
                paste0(name, "_", stringr::str_replace_all(as.character(Sys.Date()), "[-]{1}", "_"))
        }
