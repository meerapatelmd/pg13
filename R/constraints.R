

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param conn_fun PARAM_DESCRIPTION, Default: 'pg13::local_connect()'
#' @param checks PARAM_DESCRIPTION, Default: c("conn_status", "conn_type", "rows")
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param render_only PARAM_DESCRIPTION, Default: FALSE
#' @param log_file PARAM_DESCRIPTION, Default: ''
#' @param append_log PARAM_DESCRIPTION, Default: TRUE
#' @param sep_log PARAM_DESCRIPTION, Default: ''
#' @param sql_style PARAM_DESCRIPTION, Default: c("inline", "chunk")
#' @param rmd_file PARAM_DESCRIPTION, Default: ''
#' @param sql_file PARAM_DESCRIPTION, Default: ''
#' @param warn_no_rows PARAM_DESCRIPTION, Default: 'deprecated'
#' @param warnMissingParameters PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#' @rdname count_constraint_types
#' @export
#' @importFrom glue glue
count_constraint_types <-
        function(schema,
                 conn,
                 conn_fun = "pg13::local_connect()",
                 checks = c("conn_status", "conn_type", "rows"),
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


                stopifnot(!missing(schema))


                sql_statement <-
                        glue::glue(
                                "
                                SELECT constraint_type, COUNT(*)
                                FROM information_schema.table_constraints
                                WHERE
                                  table_schema = '{schema}'
                                GROUP BY constraint_type;
                                "
                        )


                query(
                        conn = conn,
                        conn_fun = conn_fun,
                        checks = checks,
                        sql_statement = sql_statement,
                        verbose = verbose,
                        render_sql = render_sql,
                        render_only = render_only,
                        log_file = log_file,
                        append_log = append_log,
                        sep_log = sep_log,
                        sql_style = sql_style,
                        rmd_file = rmd_file,
                        sql_file = sql_file,
                        warn_no_rows = warn_no_rows,
                        warnMissingParameters = warnMissingParameters,
                        ...
                )
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param conn_fun PARAM_DESCRIPTION, Default: 'pg13::local_connect()'
#' @param checks PARAM_DESCRIPTION, Default: c("conn_status", "conn_type", "rows")
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param render_only PARAM_DESCRIPTION, Default: FALSE
#' @param log_file PARAM_DESCRIPTION, Default: ''
#' @param append_log PARAM_DESCRIPTION, Default: TRUE
#' @param sep_log PARAM_DESCRIPTION, Default: ''
#' @param sql_style PARAM_DESCRIPTION, Default: c("inline", "chunk")
#' @param rmd_file PARAM_DESCRIPTION, Default: ''
#' @param sql_file PARAM_DESCRIPTION, Default: ''
#' @param warn_no_rows PARAM_DESCRIPTION, Default: 'deprecated'
#' @param warnMissingParameters PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#' @rdname get_tc
#' @export
#' @importFrom glue glue
get_tc <-
        function(schema,
                 conn,
                 conn_fun = "pg13::local_connect()",
                 checks = c("conn_status", "conn_type", "rows"),
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


                stopifnot(!missing(schema))


                sql_statement <-
                        glue::glue(
                                "
                                SELECT *
                                FROM information_schema.table_constraints
                                WHERE
                                  table_schema = '{schema}';
                                "
                        )


                query(
                        conn = conn,
                        conn_fun = conn_fun,
                        checks = checks,
                        sql_statement = sql_statement,
                        verbose = verbose,
                        render_sql = render_sql,
                        render_only = render_only,
                        log_file = log_file,
                        append_log = append_log,
                        sep_log = sep_log,
                        sql_style = sql_style,
                        rmd_file = rmd_file,
                        sql_file = sql_file,
                        warn_no_rows = warn_no_rows,
                        warnMissingParameters = warnMissingParameters,
                        ...
                )
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param conn_fun PARAM_DESCRIPTION, Default: 'pg13::local_connect()'
#' @param checks PARAM_DESCRIPTION, Default: c("conn_status", "conn_type", "rows")
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param render_only PARAM_DESCRIPTION, Default: FALSE
#' @param log_file PARAM_DESCRIPTION, Default: ''
#' @param append_log PARAM_DESCRIPTION, Default: TRUE
#' @param sep_log PARAM_DESCRIPTION, Default: ''
#' @param sql_style PARAM_DESCRIPTION, Default: c("inline", "chunk")
#' @param rmd_file PARAM_DESCRIPTION, Default: ''
#' @param sql_file PARAM_DESCRIPTION, Default: ''
#' @param warn_no_rows PARAM_DESCRIPTION, Default: 'deprecated'
#' @param warnMissingParameters PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#' @rdname get_kcu
#' @export
#' @importFrom glue glue
get_kcu <-
        function(schema,
                 conn,
                 conn_fun = "pg13::local_connect()",
                 checks = c("conn_status", "conn_type", "rows"),
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


                stopifnot(!missing(schema))


                sql_statement <-
                        glue::glue(
                                "
                                SELECT *
                                FROM information_schema.key_column_usage
                                WHERE
                                  table_schema = '{schema}';
                                "
                        )


                query(
                        conn = conn,
                        conn_fun = conn_fun,
                        checks = checks,
                        sql_statement = sql_statement,
                        verbose = verbose,
                        render_sql = render_sql,
                        render_only = render_only,
                        log_file = log_file,
                        append_log = append_log,
                        sep_log = sep_log,
                        sql_style = sql_style,
                        rmd_file = rmd_file,
                        sql_file = sql_file,
                        warn_no_rows = warn_no_rows,
                        warnMissingParameters = warnMissingParameters,
                        ...
                )
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param conn_fun PARAM_DESCRIPTION, Default: 'pg13::local_connect()'
#' @param checks PARAM_DESCRIPTION, Default: c("conn_status", "conn_type", "rows")
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param render_sql PARAM_DESCRIPTION, Default: TRUE
#' @param render_only PARAM_DESCRIPTION, Default: FALSE
#' @param log_file PARAM_DESCRIPTION, Default: ''
#' @param append_log PARAM_DESCRIPTION, Default: TRUE
#' @param sep_log PARAM_DESCRIPTION, Default: ''
#' @param sql_style PARAM_DESCRIPTION, Default: c("inline", "chunk")
#' @param rmd_file PARAM_DESCRIPTION, Default: ''
#' @param sql_file PARAM_DESCRIPTION, Default: ''
#' @param warn_no_rows PARAM_DESCRIPTION, Default: 'deprecated'
#' @param warnMissingParameters PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate-joins}}
#' @rdname get_constraints
#' @export
#' @importFrom dplyr full_join
get_constraints <-
        function(schema,
                 conn,
                 conn_fun = "pg13::local_connect()",
                 checks = c("conn_status", "conn_type", "rows"),
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

                tc <-
                        get_tc(schema = schema,
                               conn = conn,
                               conn_fun = conn_fun,
                               checks = checks,
                               verbose = verbose,
                               render_sql = render_sql,
                               render_only = render_only,
                               log_file = log_file,
                               append_log = append_log,
                               sep_log = sep_log,
                               sql_style = sql_style,
                               rmd_file = rmd_file,
                               sql_file = sql_file,
                               warn_no_rows = warn_no_rows,
                               warnMissingParameters = warnMissingParameters,
                               ...)

                kcu <-
                        get_kcu(
                                schema = schema,
                                conn = conn,
                                conn_fun = conn_fun,
                                checks = checks,
                                verbose = verbose,
                                render_sql = render_sql,
                                render_only = render_only,
                                log_file = log_file,
                                append_log = append_log,
                                sep_log = sep_log,
                                sql_style = sql_style,
                                rmd_file = rmd_file,
                                sql_file = sql_file,
                                warn_no_rows = warn_no_rows,
                                warnMissingParameters = warnMissingParameters,
                                ...
                        )

                dplyr::full_join(tc,
                                 kcu,
                                 by = c("constraint_catalog", "constraint_schema", "constraint_name", "table_catalog", "table_schema", "table_name")) %>%
                        distinct()
        }
