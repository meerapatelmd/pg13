#' @title
#' Query Function Factory
#'
#' @description
#' Write a query function that automatically connects and disconnects on exit based on the connection details parameters given.
#'
#' @return
#' Query function
#'
#' @rdname e_creds_ff
#' @export
#' @importFrom secretary typewrite

execute_creds_ff <-
        function(user,
                 password,
                 port,
                 server) {

        function(
                sql_statement,
                verbose = TRUE,
                render_sql = TRUE,
                profile = FALSE,
                progressBar = TRUE,
                reportOverallTime = TRUE,
                errorReportFile = file.path(getwd(), "errorReportSql.txt"),
                runAsBatch = FALSE) {


                conn <- connect(user = user,
                                password = password,
                                port = port,
                                server = server,
                                verbose = verbose)

                on.exit(dc(conn = conn,
                           verbose = verbose),
                        add = TRUE,
                        after = FALSE)


                execute_n(conn = conn,
                          sql_statements = sql_statement,
                          verbose = verbose,
                          render_sql = render_sql,
                          profile = profile,
                          progressBar = progressBar,
                          reportOverallTime = reportOverallTime,
                          errorReportFile = errorReportFile,
                          runAsBatch = runAsBatch)


        }

        }


#' @title
#' Query Function Factory
#'
#' @description
#' Write a query function that automatically connects and disconnects on exit based on the connection details parameters given.
#'
#' @return
#' Query function
#'
#' @rdname q_creds_ff
#' @export
#' @importFrom secretary typewrite

q_creds_ff <-
        function(user,
                 password,
                 port,
                 server) {

                function(
                        sql_statement,
                        verbose = TRUE,
                        render_sql = TRUE,
                        warn_no_rows = FALSE,
                        ...) {


                        conn <- connect(user = user,
                                        password = password,
                                        port = port,
                                        server = server,
                                        verbose = verbose)

                        on.exit(dc(conn = conn,
                                   verbose = verbose),
                                add = TRUE,
                                after = FALSE)


                        query(conn = conn,
                              sql_statement = sql_statement,
                              verbose = verbose,
                              render_sql = render_sql,
                              warn_no_rows = warn_no_rows,
                              ...)


                }

        }


#' @title
#' Execute Function Factory using Connection Expression
#' @rdname execute_fun_ff
#' @export

execute_fun_ff <-
        function(connect_fun) {

                function(
                        sql_statement,
                        verbose = TRUE,
                        render_sql = TRUE,
                        profile = FALSE,
                        progressBar = TRUE,
                        reportOverallTime = TRUE,
                        errorReportFile = file.path(getwd(), "errorReportSql.txt"),
                        runAsBatch = FALSE) {


                        conn <- eval(rlang::parse_expr(x = connect_fun))

                        on.exit(dc(conn = conn,
                                   verbose = verbose),
                                add = TRUE,
                                after = FALSE)


                        execute_n(conn = conn,
                                  sql_statements = sql_statement,
                                  verbose = verbose,
                                  render_sql = render_sql,
                                  profile = profile,
                                  progressBar = progressBar,
                                  reportOverallTime = reportOverallTime,
                                  errorReportFile = errorReportFile,
                                  runAsBatch = runAsBatch)


                }

        }

#' @title
#' Query Function Factory using Connection Expression
#' @rdname q_fun_ff
#' @export

q_fun_ff <-
        function(connect_fun) {

                function(
                        sql_statement,
                        verbose = TRUE,
                        render_sql = TRUE,
                        warn_no_rows = FALSE,
                        ...) {


                        conn <- eval(rlang::parse_expr(x = connect_fun))

                        on.exit(dc(conn = conn,
                                   verbose = verbose),
                                add = TRUE,
                                after = FALSE)


                        query(conn = conn,
                              sql_statement = sql_statement,
                              verbose = verbose,
                              render_sql = render_sql,
                              warn_no_rows = warn_no_rows,
                              ...)


                }

        }


