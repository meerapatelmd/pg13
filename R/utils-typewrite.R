#' Typewrite SQL
#' @importFrom secretary typewrite blueTxt
#' @importFrom stringr str_replace_all
#' @noRd

typewrite_sql <-
        function (sql_statement,
                  style = c("inline", "chunk"))
        {

                style <-
                match.arg(style,
                          choices = c("inline", "chunk"),
                          several.ok = FALSE)

                if (style %in% "inline") {

                        # Flatten SQL Statement
                        sql_statement <- stringr::str_replace_all(sql_statement,
                                                                  "[\r\n\t]{1,}|\\s{2,}", " ")
                        sql_statement <- trimws(sql_statement)

                        secretary::typewrite(secretary::enbold(secretary::blueTxt("SQL:")),
                                             sql_statement)

                } else {

                secretary::typewrite(secretary::enbold(secretary::blueTxt("SQL:")))
                cat_sql_chunk(sql_statement = sql_statement,
                              tab_count = 4)

                }
        }


#' Cat SQL Chunk
#' @importFrom secretary typewrite blueTxt
#' @importFrom stringr str_replace_all
#' @noRd

cat_sql_chunk <-
        function (sql_statement,
                  tab_count = 4) {
                tabs <- rep("\t", tab_count)
                tabs <- paste(tabs, collapse = "")

                output <-
                  c(
                        sprintf("%s```sql", tabs),
                        sprintf("%s%s", tabs, sql_statement),
                        sprintf("%s```", tabs)
                  )

                cat(output, sep = "\n")
        }

#' Typewrite Activity
#' @importFrom secretary typewrite yellowTxt
#' @noRd

typewrite_activity <-
        function (activity)
        {
                secretary::typewrite(secretary::yellowTxt(activity))
        }

#' @noRd

typewrite_alert_danger <-
        function(text) {
                text <-
                        sprintf("! %s", text)
                secretary::typewrite(secretary::redTxt(secretary::enbold(text)))
        }


#' @noRd

typewrite_alert_success <-
        function(text) {
                text <-
                        sprintf("\u2713 %s", text)

                secretary::typewrite(secretary::greenTxt(secretary::enbold(text)))
        }
