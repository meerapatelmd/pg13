
.write_error_report <-
        function(message,
                 sql,
                 errorFile,
                 halt = FALSE) {

                .system_info <- function() {
                        si <- session_info()
                        lines <- c()
                        lines <- c(lines, "r version:")
                        lines <- c(lines, si$r.version$version.string)
                        lines <- c(lines, "")
                        lines <- c(lines, "platform:")
                        lines <- c(lines, si$r.version$platform)
                        lines <- c(lines, "")
                        lines <- c(lines, "attached base packages:")
                        lines <- c(lines, paste("-", si$base_pkgs))
                        lines <- c(lines, "")
                        lines <- c(lines, "other attached packages:")
                        for (pkg in si$other_pkgs) lines <- c(lines,
                                                             paste("- ", pkg$Package, " (", pkg$Version, ")", sep = ""))
                        return(paste(lines, collapse = "\n"))
                }

                report <- c("timestamp:\n", as.character(sys.time()), "\n\n_error:\n", message, "\n\n_sql:\n", sql, "\n\n", .system_info())

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
