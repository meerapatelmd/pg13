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
                 table,
                 field) {

                fields <- ls_fields(conn = conn,
                                    schema = schema,
                                    table = table,
                                    verbose = FALSE,
                                    render_sql = FALSE)

                if (tolower(field) %in% fields) {

                        TRUE

                } else {

                        FALSE
                }
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
                 table_name) {


                tables <- ls_tables(conn = conn,
                                    schema = schema,
                                    verbose = FALSE,
                                    render_sql = FALSE)

                if (toupper(table_name) %in% tables) {

                        TRUE

                } else {

                        FALSE
                }
        }


#' @title
#' Does a database exist?
#'
#' @inheritParams base_args
#'
#' @rdname db_exists
#'
#' @export
#' @family logical functions

db_exists <-
        function(conn,
                 db_name) {


                dbs <- ls_db(conn = conn,
                                    verbose = FALSE,
                                    render_sql = FALSE)

                if (tolower(db_name) %in% dbs) {

                        TRUE

                } else {

                        FALSE
                }
        }
