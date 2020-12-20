#' @title
#' Grant All Privileges on all Tables
#' @description
#' Grant all privileges to a schema to either a group or a user.
#' @param users         Usernames
#' @param groups        Group names
#' @rdname grant_all_in_schema
#' @export

grant_all_in_schema <-
        function(conn,
                 schema,
                 users,
                 groups,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {

                if (missing(users) && missing(groups)) {

                        stop("`users` and/or `groups` required")

                }

                if (!missing(users)) {

                        sql_statements <- sprintf("GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA @schema TO %s;", users)

                        for (sql_statement in sql_statements) {

                                send(conn = conn,
                                     sql_statement = sql_statement,
                                     verbose = verbose,
                                     render_sql = render_sql,
                                     render_only = render_only,
                                     ...)
                        }
                }


                if (!missing(groups)) {

                        sql_statements <- sprintf("GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA @schema TO group %s;", groups)

                        for (sql_statement in sql_statements) {

                                send(conn = conn,
                                     sql_statement = sql_statement,
                                     verbose = verbose,
                                     render_sql = render_sql,
                                     render_only = render_only,
                                     ...)
                        }
                }

        }
