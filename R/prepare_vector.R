#' Convert vector to arguments
#' @param vector vector of arguments
#' @export

prepare_vector <-
        function(vector) {
                paste(vector, collapse = ",")
        }
