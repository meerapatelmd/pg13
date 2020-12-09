#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param datetime_field PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param log_table PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname create_log
#' @export
#' @importFrom SqlRender render

create_log <-
        function(datetime_field,
                 conn,
                 schema,
                 log_table,
                 verbose = TRUE,
                 render_sql = TRUE) {

                sql_statement <-
                        SqlRender::render(
                                "CREATE TABLE IF NOT EXISTS @schema.@log_table (
                                                @datetime_field timestamp without time zone,
                                                activity text,
                                                status   varchar(25)
                                );",
                                schema = schema,
                                log_table = log_table,
                                datetime_field = datetime
                        )

                send(conn = conn,
                           sql_statement = sql_statement,
                           verbose = verbose,
                           render_sql = render_sql)

        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param datetime_field PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param log_table PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname log_start_ff
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select all_of
log_start_ff <-
        function(datetime_field,
                 schema,
                 log_table) {


                function(activity,
                         conn,
                         verbose = TRUE,
                         render_sql = TRUE) {

                        create_log(datetime_field = datetime_field,
                                   conn = conn,
                                   schema = schema,
                                   log_table = log_table,
                                   verbose = verbose,
                                   render_sql = render_sql)


                        output <-
                        tibble::tibble(activity = activity,
                                       status = "start") %>%
                                dplyr::mutate({{ as.symbol(datetime_field) }} := Sys.time()) %>%
                                dplyr::select(dplyr::all_of(datetime_field),
                                             activity,
                                             status)

                        appendTable(conn = conn,
                                    schema = schema,
                                    tableName = log_table,
                                    data = output)

                }

        }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param datetime_field PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param log_table PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname log_stop_ff
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select all_of
log_stop_ff <-
        function(datetime_field,
                 schema,
                 log_table) {


                function(activity,
                         conn,
                         verbose = TRUE,
                         render_sql = TRUE) {

                        create_log(datetime_field = datetime_field,
                                   conn = conn,
                                   schema = schema,
                                   log_table = log_table,
                                   verbose = verbose,
                                   render_sql = render_sql)


                        output <-
                                tibble::tibble(activity = activity,
                                               status = "stop") %>%
                                dplyr::mutate({{ as.symbol(datetime_field) }} := Sys.time()) %>%
                                dplyr::select(dplyr::all_of(datetime_field),
                                              activity,
                                              status)

                        appendTable(conn = conn,
                                    schema = schema,
                                    tableName = log_table,
                                    data = output)

                }

        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param datetime_field PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param log_table PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname log_error_ff
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select all_of

log_error_ff <-
        function(datetime_field,
                 schema,
                 log_table) {


                function(activity,
                         conn,
                         verbose = TRUE,
                         render_sql = TRUE) {

                        create_log(datetime_field = datetime_field,
                                   conn = conn,
                                   schema = schema,
                                   log_table = log_table,
                                   verbose = verbose,
                                   render_sql = render_sql)


                        output <-
                                tibble::tibble(activity = activity,
                                               status = "error") %>%
                                dplyr::mutate({{ as.symbol(datetime_field) }} := Sys.time()) %>%
                                dplyr::select(dplyr::all_of(datetime_field),
                                              activity,
                                              status)

                        appendTable(conn = conn,
                                    schema = schema,
                                    tableName = log_table,
                                    data = output)

                }

        }
