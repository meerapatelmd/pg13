#' @title
#' Drop V Tables
#'
#' @description
#' Drop tables written by functions in this package that follow the "Stamped" naming convention of "V" followed by 14 integers representing the timestamp of the transaction. This function will clear any of the tables that strictly follows this pattern. An expiration period can optionally be applied where the date and time of the transaction is parsed from the table name and will be dropped only if the difference between the system time and timestamp in the table name is greater than the `time_diff_hours` argument.
#'
#' @return
#' If an expiration period is provided with a `time_diff_hours` greater than 0, a console message of the names of any tables following this convention remain in `schema`. If `time_diff_hours` is 0, all tables are dropped.
#'
#' @inheritParams dropWriteTable
#' @param time_diff_hours Numeric designating the period of time in hours after which the the table should be considered expired. If 0, all tables will be dropped.
#'
#' @importFrom stringr str_remove_all
#' @importFrom lubridate ymd_hms
#' @importFrom secretary typewrite
#' @importFrom purrr map
#' @export
#' @rdname drop_all_staging_tables

drop_all_staging_tables <-
        function(conn,
                 conn_fun,
                 schema,
                 time_diff_hours = 0,
                 verbose = TRUE,
                 render_sql = TRUE) {

                all_tables <- ls_tables(conn = conn,
                                       conn_fun = conn_fun,
                                       schema = schema,
                                       verbose = verbose,
                                       render_sql = render_sql)

                staging_tables <- grep("^V[0-9]{14}$",
                                all_tables,
                                ignore.case = TRUE,
                                value = TRUE)

                staging_datetime <- stringr::str_remove_all(staging_tables, "^V{1}")
                datetimes <- lubridate::ymd_hms(staging_datetime)

                if (verbose) {

                        secretary::typewrite(length(datetimes),
                                             "tables in",
                                             schema)

                }

                while (length(datetimes) > 0) {

                        if (difftime(Sys.time(), datetimes[1], "hours") > time_diff_hours) {

                                drop_table(conn = conn,
                                           conn_fun = conn_fun,
                                           schema = schema,
                                           tableName = staging_tables[1],
                                           if_exists = TRUE,
                                           verbose = verbose,
                                           render_sql = render_sql)
                        }

                        datetimes <- datetimes[-1]
                        staging_tables <- staging_tables[-1]
                }

        }
