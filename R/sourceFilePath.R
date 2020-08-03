#' Get SourceFile Path
#' @desciption This function provides the path for files installed within a given package's library.
#' @param instSubdir Name of subdirectory in the inst/ folder
#' @param FileName Name of file in subdirectory
#' @param package Package name
#' @export


sourceFilePath <-
        function(instSubdir,
                 FileName,
                 package) {
                paste0(system.file(package = package), "/", instSubdir, "/", FileName)
        }
