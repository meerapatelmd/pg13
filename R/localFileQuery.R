#' Query local Postgess from a file
#' @import DatabaseConnector
#' @export

localFileQuery <-
    function (file,
              dbname = "athena",
              port = 5432) {

        conn <- localConnect(dbname = dbname,
                             port = port)

        data <- fileQuery(conn = conn,
                          file = file)

        dc(conn = conn,
           remove = FALSE)

        return(data)
    }
