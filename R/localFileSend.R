#' Send Statement to a local Postgess from a file
#' @import DatabaseConnector
#' @export

localFileSend <-
    function (file,
              dbname = "athena",
              port = 5432) {

        conn <- localConnect(dbname = dbname,
                             port = port)

        data <- fileSend(conn = conn,
                          file = file)

        dc(conn = conn,
           remove = FALSE)

        return(data)
    }

