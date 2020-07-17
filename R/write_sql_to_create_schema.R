#' Write SQL to create a schema
#' @export

write_sql_to_create_schema <-
        function(schema) {
                sql_statement <- paste0("CREATE SCHEMA ", schema, ";")
                return(sql_statement)
        }