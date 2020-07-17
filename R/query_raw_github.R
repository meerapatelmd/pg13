#' Query raw github sql
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom readr read_file
#' @export

query_raw_github <- 
    function(conn,
             raw_github_link) {
        
                tmp_sql <- tempfile(fileext = ".txt")
                
                download.file(raw_github_link,
                              destfile = tmp_sql)
                
                
                DatabaseConnector::dbGetQuery(conn = conn,
                                              statement = readr::read_file(tmp_sql))
                
                
                unlink(tmp_sql)
        
    }