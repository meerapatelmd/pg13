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
