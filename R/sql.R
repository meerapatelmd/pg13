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

query <-
  function(conn,
           conn_fun,
           sql_statement,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE,
           sql_style = c("inline", "chunk"),
           warn_no_rows = "deprecated",
           ...) {
    if (render_only) {
      typewrite_sql(
        sql_statement = sql_statement,
        style = sql_style
      )
      invisible(sql_statement)
    } else {
      if (!missing(conn_fun)) {
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

      if (verbose) {
        check_conn(conn = conn)
      }


      if (render_sql) {
        typewrite_sql(
          sql_statement = sql_statement,
          style = sql_style
        )
      }


      if (verbose) {
        typewrite_activity("Querying...")
      }

      resultset <- DatabaseConnector::dbGetQuery(conn,
        statement = sql_statement,
        ...
      )


      if (verbose) {
        typewrite_activity("Querying...complete")
      }

      if (verbose) {
        check_inflow(data = resultset)
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
           conn_fun,
           sql_statements,
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

      if (verbose) {
        check_conn(conn = conn)
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

send <-
  function(conn,
           conn_fun,
           sql_statement,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE,
           sql_style = c("inline", "chunk"),
           ...) {
    if (render_only) {
      typewrite_sql(
        sql_statement = sql_statement,
        style = sql_style
      )
      invisible(sql_statement)
    } else {
      if (!missing(conn_fun)) {
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


      if (verbose) {
        check_conn(conn = conn)
      }

      if (render_sql) {
        typewrite_sql(
          sql_statement = sql_statement,
          style = sql_style
        )
      }

      if (verbose) {
        secretary::typewrite("Sending...")
      }

      DatabaseConnector::dbSendStatement(
        conn = conn,
        statement = sql_statement,
        ...
      )

      if (verbose) {
        secretary::typewrite("Sending...complete")
      }
    }
  }
