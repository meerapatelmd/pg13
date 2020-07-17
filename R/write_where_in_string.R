#' Take a vector and create a string for WHERE IN SQL
#' @description This function takes a vector of 1 or more values and creates a bracketed, comma-separated string of length 1 that can be inserted into the SQL statement for filtering.
#' @param where_in_vector vector that will filter the resultset
#' @param as_character If TRUE, includes single quotes around each value. If FALSE, the vector is printed as-is.
#' @export


write_where_in_string <-
        function(where_in_vector, as_character = TRUE, formatted = FALSE) {
            
            if (formatted == TRUE) {
                if (as_character == TRUE) {
                    x <- paste0("'", where_in_vector, "'")
                    x <- paste(x, collapse = "\n,")
                    x <- paste0("(\n", x, "\n)")
                } else {
                    x <- paste(x, collapse = "\n,")
                    x <- paste0("(\n", x, "\n)")
                }
            } else {
                if (as_character == TRUE) {
                    x <- paste0("'", where_in_vector, "'")
                    x <- paste(x, collapse = ",")
                    x <- paste0("(", x, ")")
                } else {
                    x <- paste(x, collapse = ",")
                    x <- paste0("(", x, ")")
                }
            }

                return(x)
        }
