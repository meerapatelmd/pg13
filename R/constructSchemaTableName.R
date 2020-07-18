#' Construct schemaTableName
#' @description construct schemaTableName from the schema and tableName
#' @export

constructSchemaTableName <-
        function(schema,
                 tableName) {


            SqlRender::render(SqlRender::readSql("inst/sql_constr/schemaTableName.sql"),
                              schema = schema,
                              tableName = tableName)

        }
