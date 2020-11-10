#' Terminate a SQL Statement with a semicolon
#' @noRd

terminateBuild <-
    function(sql_statement) {

                paste0(sql_statement, ";")

    }


#' Get SourceFile Path
#' @description This function provides the path for files installed within a given package's library.
#' @param instSubdir Name of subdirectory in the inst/ folder
#' @param FileName Name of file in subdirectory
#' @param package Package name
#' @noRd


sourceFilePath <-
        function(instSubdir,
                 FileName,
                 package) {
                paste0(system.file(package = package), "/", instSubdir, "/", FileName)
        }
