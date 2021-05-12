#' @title Typewrite SQL
#'
#' @description
#' Wrapper around `cat` function in R that returns a
#' timestamped `SQL:` heading followed by the SQL statement.
#'
#' @param sql_statement SQL statement.
#'
#' @param style The _inline_ style where the statement is flattened
#' into a single line or a _chunk_ style where the statement is
#' taken in its native format and returned within a set of
#' ticks for R Markdown, Default: "inline".
#'
#' @seealso
#'  \code{\link[stringr]{str_replace}}
#'  \code{\link[secretary]{c("typewrite", "enbold", "blueTxt")}}
#'
#' @rdname typewrite_sql
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom secretary typewrite enbold blueTxt

typewrite_sql <-
  function(sql_statement,
           style = c("inline", "chunk")) {
    style <-
      match.arg(style,
        choices = c("inline", "chunk"),
        several.ok = FALSE
      )

    if (style %in% "inline") {

      # Flatten SQL Statement
      sql_statement <- stringr::str_replace_all(
        sql_statement,
        "[\r\n\t]{1,}|\\s{2,}", " "
      )
      sql_statement <- trimws(sql_statement)

      secretary::typewrite(
        secretary::enbold(secretary::blueTxt("SQL:")),
        sql_statement
      )
    } else {
      secretary::typewrite(secretary::enbold(secretary::blueTxt("SQL:")))
      cat_sql_chunk(
        sql_statement = sql_statement,
        tab_count = 4
      )
    }
  }



#' @title
#' Cat SQL Chunk
#' @description
#' Return a given SQL around ticks for R Markdown.
#' @param sql_statement SQL statement.
#' @param tab_count Number of tabs the chunk will be indented when printed in
#' the console, Default: 0.
#' @rdname cat_sql_chunk
#' @export
cat_sql_chunk <-
  function(sql_statement,
           tab_count = 0) {
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


#' @title
#' Typewrite An Activity
#'
#' @description
#' Return the provided text in yellow along with a
#' timestamp.
#'
#' @param activity Character string.
#' @seealso
#'  \code{\link[secretary]{c("typewrite", "yellowTxt")}}
#'
#' @rdname typewrite_activity
#' @export
#' @importFrom secretary typewrite yellowTxt

typewrite_activity <-
  function(activity) {
    secretary::typewrite(secretary::yellowTxt(activity))
  }


#' @title
#' Typewrite Alert Danger
#'
#' @description
#' Return a timestamped console message in bold red with
#' exclamation mark.
#'
#' @param text Character string.
#'
#' @seealso
#'  \code{\link[secretary]{c("typewrite", "redTxt", "enbold)}}
#'
#' @rdname typewrite_alert_danger
#' @export
#' @importFrom secretary typewrite redTxt enbold

typewrite_alert_danger <-
  function(text) {
    text <-
      sprintf("! %s", text)
    secretary::typewrite(secretary::redTxt(secretary::enbold(text)))
  }



#' @title
#' Typewrite Alert Success
#'
#' @description
#' Return a message in a bold green with a checkmark.
#'
#' @param text Character string.
#' @seealso
#'  \code{\link[secretary]{c("typewrite", "greenTxt", "enbold)}}
#' @rdname typewrite_alert_success
#' @export
#' @importFrom secretary typewrite greenTxt enbold
typewrite_alert_success <-
  function(text) {
    text <-
      sprintf("\u2713 %s", text)

    secretary::typewrite(secretary::greenTxt(secretary::enbold(text)))
  }
