#' Construct schemaTableName
#' @description construct schemaTableName from the schema and tableName
#' @export

constructSchemaTableName <-
        function(schema,
                 tableName) {

            base <- system.file(package='pg13')
            path <- paste0(base, "/sql_constr")


            SqlRender::render(SqlRender::readSql(paste0(path, "/schemaTableName.sql")),
                              schema = schema,
                              tableName = tableName)

        }
