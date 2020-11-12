
.writeErrorReport <-
        function(message,
                 sql,
                 errorFile,
                 halt = FALSE) {

                .systemInfo <- function() {
                        si <- sessionInfo()
                        lines <- c()
                        lines <- c(lines, "R version:")
                        lines <- c(lines, si$R.version$version.string)
                        lines <- c(lines, "")
                        lines <- c(lines, "Platform:")
                        lines <- c(lines, si$R.version$platform)
                        lines <- c(lines, "")
                        lines <- c(lines, "Attached base packages:")
                        lines <- c(lines, paste("-", si$basePkgs))
                        lines <- c(lines, "")
                        lines <- c(lines, "Other attached packages:")
                        for (pkg in si$otherPkgs) lines <- c(lines,
                                                             paste("- ", pkg$Package, " (", pkg$Version, ")", sep = ""))
                        return(paste(lines, collapse = "\n"))
                }

                report <- c("Timestamp:\n", as.character(Sys.time()), "\n\nError:\n", message, "\n\nSQL:\n", sql, "\n\n", .systemInfo())

                readr::write_lines(report,
                                   path = errorFile,
                                   append = TRUE)

                if (halt) {
                        stop(paste("Error executing SQL:",
                                   message,
                                   paste("\nError is documented in ", errorFile),
                                   sep = "\n"), call. = FALSE)
                } else {
                        warning(paste("\nError executing SQL:",
                                   message,
                                   paste("Error is documented in ", errorFile),
                                   sep = "\n"), call. = FALSE)
                }
        }
