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
#' @rdname lowLevel_typewrite_sql
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom secretary typewrite enbold blueTxt

lowLevel_typewrite_sql <-
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
#' Typewrite SQL Log
#'
#' @description
#' Wrapper around `cat` function in R that returns a
#' timestamped `SQL:` heading followed by the SQL statement
#' along with writing it to a log file.
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
#' @importFrom crayon strip_style

typewrite_sql <-
  function(sql_statement,
           style = c("inline", "chunk"),
           log_file = "",
           sep = "\n",
           append = TRUE) {

      console_msg <-
        utils::capture.output(
          lowLevel_typewrite_sql(sql_statement = sql_statement,
                        style = style)
        )
      if (log_file != "") {
      log_msg <-
        crayon::strip_style(console_msg)
      cat(log_msg,
          file = log_file,
          sep = sep,
          append = append)
    }
    cat(console_msg,
        sep = sep)
  }

#' @title
#' Concatenate a SQL Chunk
#' @description
#' Return a SQL statement given as a string surrounded by ticks for
#' R Markdown consumption in the console. Output will be written
#' verbatim to an Rmd file path if provided.
#' @param sql_statement SQL statement.
#' @param tab_count Number of tabs the chunk will be indented when printed in
#' the console, Default: 0.
#' @param sep Passed to `cat()`.
#' @param rmd_file Passed as `file` argument to `cat().` A file will not be written by default and an error will be
#' thrown if the extension is not _Rmd_. Use `cat_sql` to write files with a _sql_ extension.
#' @param append  Passed to `cat()`.
#' @rdname cat_sql_chunk
#' @export
#' @importFrom xfun file_ext

cat_sql_chunk <-
  function(sql_statement,
           tab_count = 0,
           sep = "\n",
           rmd_file = "",
           append = FALSE) {
    tabs <- rep("\t", tab_count)
    tabs <- paste(tabs, collapse = "")

    output <-
      c(
        sprintf("%s```sql", tabs),
        sprintf("%s%s", tabs, sql_statement),
        sprintf("%s```", tabs)
      )

    if (!(rmd_file %in% "")) {

      stopifnot(xfun::file_ext(rmd_file) == "Rmd")
      cat(output,
          sep = sep,
          file = rmd_file)
    }

    cat(output,
        sep = sep)
  }



#' @title
#' Concatenate SQL statement
#' @description
#' Return a SQL statement to the console. Output will be written
#' verbatim to an sql file path if provided.
#' @param sql_statement SQL statement.
#' @param tab_count Number of tabs the chunk will be indented when printed in
#' the console, Default: 0.
#' @param sep Passed to `cat()`.
#' @param sql_file Passed as `file` argument to `cat().` An error will be
#' thrown if the extension is not _sql_. If value is '""', then
#' no file is written.
#' @param append  Passed to `cat()`.
#' @rdname cat_sql
#' @export
#' @importFrom xfun file_ext

cat_sql <-
  function(sql_statement,
           tab_count = 0,
           sep = "\n",
           sql_file = "",
           append = FALSE) {
    tabs <- rep("\t", tab_count)
    tabs <- paste(tabs, collapse = "")

    output <-
        sprintf("%s%s", tabs, sql_statement)


    if (!(sql_file %in% "")) {

      stopifnot(xfun::file_ext(sql_file) == "sql")
      cat(output,
          sep = sep,
          file = sql_file)
    }

    cat(output,
        sep = sep)
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
#' @rdname lowLevel_typewrite_activity
#' @export
#' @importFrom secretary typewrite yellowTxt

lowLevel_typewrite_activity <-
  function(activity) {
    secretary::typewrite(secretary::yellowTxt(activity))
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
  function(activity,
           log_file = "",
           sep = "\n",
           append = TRUE) {
      console_msg <-
        utils::capture.output(
          lowLevel_typewrite_activity(activity = activity)
        )
  if (log_file != "") {
      log_msg <-
        crayon::strip_style(console_msg)
      cat(log_msg,
          file = log_file,
          sep = sep,
          append = append)
    }
    cat(console_msg,
        sep = sep)
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
#' @rdname lowLevel_typewrite_alert_danger
#' @export
#' @importFrom secretary typewrite redTxt enbold

lowLevel_typewrite_alert_danger <-
  function(text) {
    text <-
      sprintf("! %s", text)
    secretary::typewrite(secretary::redTxt(secretary::enbold(text)))
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
  function(text,
           log_file = "",
           sep = "\n",
           append = TRUE) {

    console_msg <-
      utils::capture.output(
        lowLevel_typewrite_alert_danger(text = text)
      )
    if (log_file != "") {
      log_msg <-
        crayon::strip_style(console_msg)
      cat(log_msg,
          file = log_file,
          sep = sep,
          append = append)
    }
    cat(console_msg,
        sep = sep)

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
#' @rdname lowLevel_typewrite_alert_success
#' @export
#' @importFrom secretary typewrite greenTxt enbold
lowLevel_typewrite_alert_success <-
  function(text) {
    text <-
      sprintf("\u2713 %s", text)

    secretary::typewrite(secretary::greenTxt(secretary::enbold(text)))
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
  function(text,
           log_file = "",
           sep = "\n",
           append = TRUE) {
    console_msg <-
      utils::capture.output(
        lowLevel_typewrite_alert_success(text = text)
      )
    if (log_file != "") {
      log_msg <-
        crayon::strip_style(console_msg)
      cat(log_msg,
          file = log_file,
          sep = sep,
          append = append)
    }
    cat(console_msg,
        sep = sep)
  }
