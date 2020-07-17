#' Write SQL to drop a schema
#' @export

write_sql_to_drop_schema <-
        function(schema) {
                sql_statement <- paste0("DROP SCHEMA ", schema, " IF EXISTS;")
                return(sql_statement)
        }