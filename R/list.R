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
             tableName,
             verbose = TRUE,
             render_sql = TRUE) {

            if (render_sql) {

                typewrite_sql("N/A")

            }


            if (verbose) {

                typewrite_activity("Listing Fields...")

            }

            resultset <- DatabaseConnector::dbListFields(conn = conn,
                                                        name = tableName,
                                                        schema = schema)

            if (verbose) {

                typewrite_activity("Listing Fields...complete")

            }

            tolower(resultset)

    }


#' @title
#' Does a field exist?
#'
#' @description
#' Logical that checks if a field exists in a table. The `field` argument is formatted into lowercase prior to being checked.
#'
#'
#' @inheritParams base_args
#' @param field Character string to check for in the given table.
#'
#' @rdname field_exists
#' @export
#' @family logical functions

field_exists <-
    function(conn,
             schema,
             tableName,
             field) {

        fields <- ls_fields(conn = conn,
                             schema = schema,
                             tableName = tableName,
                             verbose = FALSE,
                             render_sql = FALSE)

        if (tolower(field) %in% Fields) {

            TRUE

        } else {

            FALSE
        }
    }


#' @title
#' List Schemas
#'
#' @description
#' List all the schemas in a database in lowercase.
#'
#' @export
#' @family list functions

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
#' Does a schema exist?
#'
#' @description
#' Logical that checks if a schema exists in the database. The `schema` argument is in formatted in all lowercase prior to checking against what is present in the database.
#'
#'
#' @inheritParams base_args
#'
#' @rdname schema_exists
#' @export
#' @family logical functions


schema_exists <-
    function(conn,
             schema) {


                    schemas <-
                        ls_schema(conn = conn,
                                 verbose = FALSE,
                                 render_sql = FALSE)



                if (tolower(schema) %in% schemas) {

                    TRUE

                } else {

                    FALSE
                }


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
#' Does a table exist?
#'
#' @inheritParams base_args
#'
#' @rdname table_exists
#'
#' @export
#' @family logical functions

table_exists <-
    function(conn,
             schema,
             tableName) {


        tables <- ls_tables(conn = conn,
                           schema = schema,
                           verbose = FALSE,
                           render_sql = FALSE)

        if (toupper(tableName) %in% Tables) {

            TRUE

        } else {

            FALSE
        }
    }
