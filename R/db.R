#' @title
#' Create a New Database
#' @export
#' @rdname create_db

create_db <-
    function(conn,
             db_name,
             verbose = TRUE,
             render_sql = TRUE,
             render_only = FALSE,
             ...) {


            sql_statement <- sprintf("CREATE DATABASE %s;", tolower(db_name))

            send(conn = conn,
                 sql_statement = sql_statement,
                 verbose = verbose,
                 render_sql = render_sql,
                 render_only = render_only,
                 ...)

    }






#' @title
#' Rename a Database
#' @export
#' @rdname rename_db
#' @importFrom SqlRender render

rename_db <-
    function(conn,
             db,
             new_db_name,
             verbose = TRUE,
             render_sql = TRUE,
             render_only = FALSE,
             ...) {


            sql_statement <-
                    SqlRender::render("ALTER DATABASE @db RENAME TO @newDB;",
                                      db = db,
                                      newDB = new_db_name)
            send(conn = conn,
                 sql_statement = sql_statement,
                 verbose = verbose,
                 render_sql = render_sql,
                 render_only = render_only,
                 ...)

    }


#' @title
#' Drop a Database
#' @export
#' @rdname drop_db
#' @importFrom SqlRender render


drop_db <-
        function(conn,
                 db,
                 if_exists = FALSE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 ...) {

                if (if_exists) {

                        if_exists_clause <- "IF EXISTS"

                } else {

                        if_exists_clause <- NULL
                }

                sql_statement <-
                        SqlRender::render("DROP DATABASE @if_exists_clause @db;",
                                                if_exists_clause = if_exists_clause,
                                                db = db)

                send(conn = conn,
                     sql_statement = sql_statement,
                     verbose = verbose,
                     render_sql = render_sql,
                     render_only = render_only,
                     ...)

        }
