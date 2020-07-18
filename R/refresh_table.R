#' Refresh Table with New Data
#' @description A refresh is when a table needs to be overwritten and the table that is being overwritten is off-loaded to another table with today's date and the index of iterations for that day.
#' @import stringr
#' @import secretary
#' @import purrr
#' @export


refresh_table <-
            function(conn,
                     schema,
                     tableName,
                     .data) {

                tableNameHist <- toupper(paste0(tableName, "_", stringr::str_replace_all(rubix::dated(), "[-]", "_")))


                today_tables <-
                    grep(tableNameHist,
                         lsTables(conn = conn,
                                  schema = schema),
                         value = TRUE)

                n <- length(today_tables)


                # If this table has not been written yet today
                if (n == 0) {
                    secretary::typewrite_bold("No Off-Loaded Tables Today")
                    secretary::typewrite_bold("Next Table Name:", tableNameHist)
                    # If more than 1 table has been written today, the new table name would be the length of the list of today's table + 1
                } else {
                    secretary::typewrite_bold("Off-Loaded Tables Today:")
                    today_tables %>%
                        purrr::map(function(x) secretary::typewrite(x, tabs = 1))

                    tableNameHist <- paste0(tableNameHist, "_", (1+n))
                    secretary::typewrite_bold("Next Table Name:", tableNameHist)

                }

                secretary::press_enter()

                renameTable(conn = conn,
                            schema = schema,
                            tableName = tableName,
                            newTableName = tableNameHist)

                secretary::typewrite_bold(tableName, "renamed to", tableNameHist)

                writeTable(conn = conn,
                           tableName = tableName,
                           schema = schema,
                           .data = .data %>%
                                    as.data.frame())

                secretary::typewrite_bold("New", tableName, "written.")


            }
