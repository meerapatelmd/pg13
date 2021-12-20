#' @title
#' Query
#'
#' @inheritParams args
#' @seealso
#'  \code{\link[rlang]{parse_expr}}
#' @rdname query
#' @export
#' @importFrom rlang parse_expr
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom SqlRender render

query <-
  function(conn,
           conn_fun = "pg13::local_connect()",
           checks = c("conn_status", "conn_type", "rows", "source_rows"),
           sql_statement,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE,
           log_file = "",
           append_log = TRUE,
           sep_log = "\n",
           sql_style = c("inline", "chunk"),
           rmd_file = "",
           sql_file = "",
           warn_no_rows = "deprecated",
           warnMissingParameters = TRUE,
           ...) {

    sql_style <-
      match.arg(arg = sql_style,
                choices = c("inline", "chunk"),
                several.ok = FALSE)

    if (!missing(...)) {
      sql_statement <-
        SqlRender::render(
          sql = sql_statement,
          warnOnMissingParameters = warnOnMissingParameters,
          ...
        )
    }

    if (render_only) {
      if (sql_style == "chunk") {
        cat_sql_chunk(
          sql_statement = sql_statement,
          rmd_file = rmd_file
        )
      } else {
        cat_sql(
          sql_statement = sql_statement,
          sql_file = sql_file
        )
      }

      invisible(sql_statement)

    } else {
      # Verbose has to be true if a log file has been provided because logging is conditional upon verbose
      if (verbose == FALSE & log_file != "") {
        secretary::typewrite_italic("Note: `verbose` set to TRUE because `log_file` was provided.\n")
        verbose <- TRUE
      }
      if (missing(conn)) {
        conn <- eval(rlang::parse_expr(conn_fun))
        on.exit(dc(
          conn = conn,
          verbose = verbose
        ),
        add = TRUE,
        after = TRUE
        )
      }

      # +++
      # Checks
      # +++

      if ("conn_status" %in% checks) {
        check_conn_status(conn = conn,
                          log_file = log_file,
                          append = append_log,
                          sep = sep_log)
      }

      if ("conn_type" %in% checks) {
        check_conn_type(conn = conn,
                        log_file = log_file,
                        append = append_log,
                        sep = sep_log)
      }

      if (render_sql) {
        typewrite_sql(
          sql_statement = sql_statement,
          style = sql_style,
          log_file = log_file,
          append = append_log,
          sep = sep_log
        )
      }


      if (verbose) {
        typewrite_activity("Querying...",
                           log_file = log_file,
                           append = append_log,
                           sep = sep_log)
      }

      resultset <- DatabaseConnector::dbGetQuery(conn,
        statement = sql_statement
      )


      if (verbose) {
        typewrite_activity("Querying...complete",
                           log_file = log_file,
                           append = append_log,
                           sep = sep_log)
      }

      if ("rows" %in% checks) {
        check_rows(data = resultset,
                   log_file = log_file,
                   append = append_log,
                   sep = sep_log)
      }

      if ("source_rows" %in% checks) {
        check_source_rows(
                  sql_statement = sql_statement,
                  conn = conn,
                   log_file = log_file,
                   append = append_log,
                   sep = sep_log)
      }
      resultset
    }
  }





#' @title
#' Query with more than 1 SQL Statements
#'
#' @description
#' Query multiple SQL Statements in a single function call. A single statement is permitted and differs from the \code{link{query}} and \code{link{send}} functions by having providing the `profile`, `progressBar`, `reportOverallTime`, `errorReportFile`, and `runAsBatch` arguments that are passed to the \code{link[DatabaseConnector]{executeSql}}. If this function is throwing an error, the function will conversely loop over the sql statements individually.
#'
#' @inheritParams DatabaseConnector::executeSql
#' @inheritParams args
#' @inheritParams query
#'
#' @importFrom DatabaseConnector executeSql
#'
#' @rdname execute_n
#'
#' @export

execute_n <-
  function(conn,
           conn_fun =  "pg13::local_connect()",
           sql_statements,
           checks = c("conn_status", "conn_type", "rows"),
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE,
           profile = FALSE,
           progressBar = TRUE,
           reportOverallTime = TRUE,
           errorReportFile = file.path(getwd(), "errorReportSql.txt"),
           runAsBatch = FALSE) {
    if (render_only) {
      sapply(sql_statements,
        FUN = typewrite_sql
      )
      invisible(sql_statements)
    } else {
      if (!missing(conn_fun)) {
        conn <- eval(rlang::parse_expr(conn_fun))
        on.exit(dc(conn = conn))
      }

      # +++
      # Checks
      # +++

      if ("conn_status" %in% checks) {
        check_conn_status(conn = conn)
      }

      if ("conn_type" %in% checks) {
        check_conn_type(conn = conn)
      }
      if ("rows" %in% checks) {
        check_rows(data = data)
      }
      if (is.list(sql_statements)) {
        sql_statements <- unlist(sql_statements)
      }

      sql_statement <- paste(sql_statements, collapse = ";\n")

      if (render_sql) {
        typewrite_sql(sql_statement = sql_statement)
        cat("\n\n")
      }

      if (verbose) {
        typewrite_activity("Executing...")
      }

      results <-
        tryCatch(
          DatabaseConnector::executeSql(
            connection = conn,
            sql = sql_statement,
            profile = profile,
            progressBar = progressBar,
            reportOverallTime = reportOverallTime,
            errorReportFile = errorReportFile,
            runAsBatch = runAsBatch
          ),
          error = function(e) "Error"
        )


      if (identical(results, "Error")) {
        typewrite_activity("Executing...failed.")
        typewrite_activity("Skipping over sql_statements causing errors.")

        errors <- vector()
        each_result <- list()
        for (i in seq_along(sql_statements)) {
          if (render_sql) {
            typewrite_sql(sql_statement = sql_statements[i])
            cat("\n\n")
          }

          if (verbose) {
            typewrite_activity("Executing...")
          }

          results2 <-
            tryCatch(
              DatabaseConnector::executeSql(
                connection = conn,
                sql = sql_statements[i],
                profile = profile,
                progressBar = progressBar,
                reportOverallTime = reportOverallTime,
                errorReportFile = errorReportFile,
                runAsBatch = runAsBatch
              ),
              error = function(e) "Error"
            )

          if (verbose) {
            typewrite_activity("Executing...complete")
          }

          if (identical(results2, "Error")) {
            errors <-
              c(
                errors,
                sql_statements[i]
              )
          } else {
            each_result[[length(each_result) + 1]] <-
              results2
            names(each_result)[length(each_result)] <- sql_statements[i]
          }
        }


        list(
          RESULTS = each_result,
          ERRORS = errors
        )
      } else {
        cat("\n\n")
        typewrite_activity("Executing...complete")
        results
      }
    }
  }



#' @title
#' Send a SQL Statement to Postgres
#' @rdname send
#' @export
#' @importFrom rlang parse_expr
#' @importFrom secretary typewrite
#' @importFrom DatabaseConnector dbSendStatement
#' @importFrom SqlRender render

send <-
  function(conn,
           conn_fun = "pg13::local_connect()",
           sql_statement,
           log_file = "",
           append_log = TRUE,
           sep_log = "\n",
           checks = c("conn_status", "conn_type"),
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE,
           sql_style = c("inline", "chunk"),
           rmd_file = "",
           sql_file = "",
           warnOnMissingParameters = TRUE,
           ...) {

    sql_style <-
      match.arg(arg = sql_style,
                choices = c("inline", "chunk"),
                several.ok = FALSE)

    if (!missing(...)) {
      sql_statement <-
        SqlRender::render(
          sql = sql_statement,
          warnOnMissingParameters = warnOnMissingParameters,
          ...
        )
    }
    if (render_only) {
      if (sql_style == "chunk") {
        cat_sql_chunk(
          sql_statement = sql_statement,
          rmd_file = rmd_file
        )
      } else {
        cat_sql(
          sql_statement = sql_statement,
          sql_file = sql_file
        )
      }

      invisible(sql_statement)
    } else {
      # Verbose has to be true if a log file has been provided because logging is conditional upon verbose
      if (verbose == FALSE & log_file != "") {
        secretary::typewrite_italic("`verbose` set to TRUE because `log_file` was provided.")
        verbose <- TRUE
      }
      if (missing(conn)) {
        conn <- eval(rlang::parse_expr(conn_fun))
        on.exit(dc(
          conn = conn,
          verbose = verbose
        ),
        add = TRUE,
        after = TRUE
        )
      }
      # +++
      # Checks
      # +++

      if ("conn_status" %in% checks) {
        check_conn_status(conn = conn)
      }

      if ("conn_type" %in% checks) {
        check_conn_type(conn = conn)
      }

      if (render_sql) {
        typewrite_sql(
          sql_statement = sql_statement,
          style = sql_style,
          log_file = log_file,
          append = append_log,
          sep = sep_log
        )
      }

      if (verbose) {
        typewrite_activity("Sending...",
                           log_file = log_file,
                           append = append_log,
                           sep = sep_log)
      }

      DatabaseConnector::dbSendStatement(
        conn = conn,
        statement = sql_statement
      )

      if (verbose) {
        typewrite_activity("Sending...complete",
                             log_file = log_file,
                             append = append_log,
                             sep = sep_log)
      }
    }
  }
