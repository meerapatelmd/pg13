#' @title
#' List Users
#' @rdname list_users
#' @export


list_users <-
        function(conn,
                 conn_fun,
                 verbose = TRUE,
                 render_sql = TRUE,
                 warn_no_rows = TRUE,
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
                      warn_no_rows = warn_no_rows,
                      render_only = render_only,
                      ...)


        }
