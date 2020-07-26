#' Construct a join part of a SQL Statement
#' @description Construct {join type} JOIN {schema.table.column2} ON {schema.table.column2} = {schema.table.column1} part of the sql statement
#' @import SqlRender
#' @export

constructJoin <-
    function(schema,
             tableName,
             column,
             joinType = "LEFT",
             joinOnSchema,
             joinOnTableName,
             joinOnColumn) {

            base <- system.file(package='pg13')
            path <- paste0(base, "/sql_constr")

            SqlRender::render(SqlRender::readSql(paste0(path, "/join.sql")),
                              schema1 = schema,
                              tableName1 = tableName,
                              column1 = column,
                              joinType = joinType,
                              schema2 = joinOnSchema,
                              tableName2 = joinOnTableName,
                              column2 = joinOnColumn)

    }
