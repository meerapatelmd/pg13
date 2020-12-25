#' @title
#' List Fields
#'
#' @description
#' Fields for the given table are returned in lowercase.
#'
#' @importFrom DatabaseConnector dbListFields
#' @export
#'
#' @rdname ls_fields
#' @family list functions


ls_fields <-
    function(conn,
             schema,
             table,
             verbose = TRUE,
             render_sql = TRUE) {

            if (render_sql) {

                typewrite_sql("N/A")

            }


            if (verbose) {

                typewrite_activity("Listing Fields...")

            }

            resultset <- DatabaseConnector::dbListFields(conn = conn,
                                                        name = table,
                                                        schema = schema)

            if (verbose) {

                typewrite_activity("Listing Fields...complete")

            }

            tolower(resultset)

    }



#' @title
#' List Schemas
#'
#' @description
#' List all the schemas in a database in lowercase.
#'
#' @export
#' @family list functions
#' @rdname ls_schema

ls_schema <-
        function(conn,
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                query(conn = conn,
                      sql_statement = "SELECT nspname FROM pg_catalog.pg_namespace;",
                      verbose = verbose,
                      render_sql = render_sql,
                      render_only = render_only) %>%
                unlist() %>%
                unname() %>%
                tolower()

        }


#' @title
#' List Databases
#'
#' @description
#' List all the databases in a connection
#'
#' @export
#' @family list functions
#' @rdname ls_db

ls_db <-
    function(conn,
             verbose = TRUE,
             render_sql = TRUE,
             render_only = FALSE) {

        query(conn = conn,
              sql_statement = "SELECT datname FROM pg_database WHERE datistemplate = false;",
              verbose = verbose,
              render_sql = render_sql,
              render_only = render_only) %>%
            unlist() %>%
            unname() %>%
            tolower()

    }

#' @title
#' List Tables
#'
#' @inheritParams base_args
#'
#' @importFrom DatabaseConnector dbListTables
#'
#' @rdname ls_tables
#'
#' @export
#' @family list functions

ls_tables <-
    function(conn,
             conn_fun,
             schema,
             verbose = TRUE,
             render_sql = TRUE) {


            if (!missing(conn_fun)) {

                conn <- eval(rlang::parse_expr(conn_fun))
                on.exit(dc(conn = conn))

            }


            if (render_sql) {

                typewrite_sql("N/A")

            }


            if (verbose) {

                typewrite_activity("Listing Tables...")

            }


            resultset <- DatabaseConnector::dbListTables(conn = conn,
                                                        schema = schema)

            if (verbose) {

                typewrite_activity("Listing Tables...completed")

            }

            toupper(resultset)


    }

#' @title
#' List Users
#' @rdname ls_users
#' @export
#' @family list functions

ls_users <-
    function(conn,
             conn_fun,
             verbose = TRUE,
             render_sql = TRUE,
             render_only = FALSE,
             ...) {

        sql_statement <-
            "SELECT usename AS role_name,
  CASE
     WHEN usesuper AND usecreatedb THEN
	   CAST('superuser, create database' AS pg_catalog.text)
     WHEN usesuper THEN
	    CAST('superuser' AS pg_catalog.text)
     WHEN usecreatedb THEN
	    CAST('create database' AS pg_catalog.text)
     ELSE
	    CAST('' AS pg_catalog.text)
  END role_attributes
FROM pg_catalog.pg_user
ORDER BY role_name desc;"

        query(conn = conn,
              conn_fun = conn_fun,
              sql_statement = sql_statement,
              verbose = verbose,
              render_sql = render_sql,
              render_only = render_only,
              ...)


    }

