#' Connect to MySQL v5.5 via Unix Sock using credentials stored in a local .Renviron
#' @param dbname database name as character string
#' @importFrom RMySQL dbConnect
#' @importFrom RMySQL MySQL
#' @export

connect_to_mysql5.5 <-
        function(dbname) {
                username_var <- paste0(dbname, "_", "username")
                password_var <- paste0(dbname, "_", "password")

                RMySQL::dbConnect(RMySQL::MySQL(),
                                  dbname = dbname,
                                  username = Sys.getenv(username_var),
                                  password = Sys.getenv(password_var),
                                  unix.socket = "/opt/local/var/run/mysql55/mysqld.sock")
        }



