#' Return SQL statements into the console that loads RRF in raw format into a corresponding table
#' @param rrf_filenames character vector of rrf filenames that will be loaded into tables of the same basename
#' @export

write_sql_statement_to_load_rrf <-
        function(rrf_filenames) {
                rrf_filenames <- paste0("'", rrf_filenames, "'")
                cat(
                        paste(
                                " LOAD DATA LOCAL INFILE", rrf_filenames,
                                "INTO TABLE", str_remove_all(rrf_filenames, "[']|[.]{1}RRF"), "\n",
                                "FIELDS TERMINATED BY '|' ESCAPED BY '' LINES TERMINATED BY '\\n';\n\n"
                        )
                )

        }