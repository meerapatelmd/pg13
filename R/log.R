#' @title
#' Create a Log Table
#'
#' @description
#' Create a `Log Table` if it does not already exist. A log table consists of a user-defined timestamp field, followed by `activity` and `status` fields.
#'
#' @inheritParams args
#' @param datetime_field Name of the datetime field. Defaults to `log_datetime`.
#' @param log_table Log table name
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname create_log
#' @export
#' @importFrom SqlRender render
#' @family logging functions
create_log <-
  function(datetime_field = "log_datetime",
           conn,
           conn_fun,
           schema,
           log_table,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE) {
    sql_statement <-
      SqlRender::render(
        "CREATE TABLE IF NOT EXISTS @schema.@log_table (
                                                @datetime_field timestamp without time zone,
                                                activity text,
                                                status   varchar(25)
                                );",
        schema = schema,
        log_table = log_table,
        datetime_field = datetime_field
      )

    send(
      conn = conn,
      conn_fun = conn_fun,
      sql_statement = sql_statement,
      verbose = verbose,
      render_sql = render_sql,
      render_only = render_only
    )
  }


#' @title
#' Log a Start Function Factory
#' @description
#' Create a function that will log a start of an activity to the provided `Log Table`.
#' @inheritParams create_log
#' @inheritParams args
#' @return
#' A function that logs the start of an activity.
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname log_start_ff
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select all_of
#' @family logging functions
log_start_ff <-
  function(datetime_field = "log_datetime",
           schema,
           log_table) {
    function(activity,
             conn,
             conn_fun,
             verbose = TRUE,
             render_sql = TRUE,
             render_only = FALSE) {
      create_log(
        datetime_field = datetime_field,
        conn = conn,
        conn_fun = conn_fun,
        schema = schema,
        log_table = log_table,
        verbose = verbose,
        render_sql = render_sql,
        render_only = render_only
      )


      output <-
        tibble::tibble(
          activity = activity,
          status = "start"
        ) %>%
        dplyr::mutate({{ datetime_field }} := Sys.time()) %>%
        dplyr::select(
          dplyr::all_of(datetime_field),
          activity,
          status
        )

      append_table(
        conn = conn,
        conn_fun = conn_fun,
        schema = schema,
        table = log_table,
        data = output,
        verbose = verbose,
        render_sql = render_sql
      )
    }
  }



#' @title
#' Log a Stop Function Factory
#' @description
#' Create a function that will log the stop of an activity to the provided `Log Table`.
#' @inheritParams create_log
#' @inheritParams args
#' @return
#' A function that logs the stop of an activity.
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname log_stop_ff
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select all_of
#' @family logging functions
log_stop_ff <-
  function(datetime_field = "log_datetime",
           schema,
           log_table) {
    function(activity,
             conn,
             conn_fun,
             verbose = TRUE,
             render_sql = TRUE,
             render_only = FALSE) {
      create_log(
        datetime_field = datetime_field,
        conn = conn,
        conn_fun = conn_fun,
        schema = schema,
        log_table = log_table,
        verbose = verbose,
        render_sql = render_sql,
        render_only = render_only
      )


      output <-
        tibble::tibble(
          activity = activity,
          status = "stop"
        ) %>%
        dplyr::mutate({{ datetime_field }} := Sys.time()) %>%
        dplyr::select(
          dplyr::all_of(datetime_field),
          activity,
          status
        )

      append_table(
        conn = conn,
        conn_fun = conn_fun,
        schema = schema,
        table = log_table,
        data = output,
        verbose = verbose,
        render_sql = render_sql
      )
    }
  }


#' @title
#' Log an Error Function Factory
#' @description
#' Create a function that will log an error for an activity to the provided `Log Table`.
#' @inheritParams create_log
#' @inheritParams args
#' @return
#' A function that logs an error of an activity.
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname log_error_ff
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select all_of
#' @family logging functions

log_error_ff <-
  function(datetime_field = "log_datetime",
           schema,
           log_table) {
    function(activity,
             conn,
             conn_fun,
             verbose = TRUE,
             render_sql = TRUE,
             render_only = FALSE) {
      create_log(
        datetime_field = datetime_field,
        conn = conn,
        conn_fun = conn_fun,
        schema = schema,
        log_table = log_table,
        verbose = verbose,
        render_sql = render_sql,
        render_only = render_only
      )


      output <-
        tibble::tibble(
          activity = activity,
          status = "error"
        ) %>%
        dplyr::mutate({{ datetime_field }} := Sys.time()) %>%
        dplyr::select(
          dplyr::all_of(datetime_field),
          activity,
          status
        )

      append_table(
        conn = conn,
        schema = schema,
        table = log_table,
        data = output,
        verbose = verbose,
        render_sql = render_sql
      )
    }
  }
