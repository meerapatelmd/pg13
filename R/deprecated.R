#' Append Current Date to a String
#' @description Date is appended in "YYYY_mm_dd" Format
#' @import stringr
#' @export

appendDate <-
        function(name) {
                .Deprecated(new = "affix_date")
                paste0(name, "_", stringr::str_replace_all(as.character(Sys.Date()), "[-]{1}", "_"))
        }

#' Construct a base SQL query
#' @description (Deprecated)  Construct the SELECT {fields} FROM {schema}.{table} base query. The SQL Statement is not terminated for the addition of other parameters generated using "construct" functions.
#' @param distinct if TRUE, the DISTINCT fields will be selected for.
#' @import SqlRender
#' @export

constructBase <-
        function(fields = "*",
                 distinct = FALSE,
                 schema = "public",
                 tableName) {
                .Deprecated()

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")

                if (distinct) {

                        SqlRender::render(SqlRender::readSql(paste0(path, "/distinctBase.sql")),
                                          fields = fields,
                                          schema = schema,
                                          tableName = tableName)


                } else {

                        SqlRender::render(SqlRender::readSql(paste0(path, "/base.sql")),
                                          fields = fields,
                                          schema = schema,
                                          tableName = tableName)

                }

        }





#' Render y in "WHERE x IN (y)"
#' @description (Deprecated)  This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param vector vector of values that the SQL query is filtering for
#' @export


constructIn <-
        function(vector) {
                .Deprecated()
                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")

                SqlRender::render(SqlRender::readSql(paste0(path, "/in.sql")),
                                  vector = vector)
        }





#' Construct a join part of a SQL Statement
#' @description (Deprecated)  Construct {join type} JOIN {schema.table.column2} ON {schema.table.column2} = {schema.table.column1} part of the sql statement
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
                .Deprecated()

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





#' Construct LIMIT
#' @description (Deprecated)  This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param n rows to limit to
#' @export


constructLimit <-
        function(n) {
                .Deprecated()

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")

                SqlRender::render(SqlRender::readSql(paste0(path, "/limit.sql")),
                                  n = n)
        }





#' Construct ORDER BY RANDOM()
#' @param n Row number desired in the output
#' @import SqlRender
#' @export


constructRandom <-
        function(n) {
                .Deprecated()

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")

                SqlRender::render(SqlRender::readSql(paste0(path, "/orderByRandom.sql")),
                                  n = n)

        }





#' Construct schemaTableName
#' @description (Deprecated)  construct schemaTableName from the schema and tableName
#' @export

constructSchemaTableName <-
        function(schema,
                 tableName) {
                .Deprecated()

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")


                SqlRender::render(SqlRender::readSql(paste0(path, "/schemaTableName.sql")),
                                  schema = schema,
                                  tableName = tableName)

        }





#' Render x in "WHERE x IN y"
#'@description (Deprecated)  This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param vector vector of values that the SQL query is filtering for
#' @export


constructWhereIn <-
        function(field,
                 vector) {
                .Deprecated()

                if (is.character(vector)) {

                        vector <- paste0("'", vector, "'")

                }

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")

                SqlRender::render(SqlRender::readSql(paste0(path, "/whereIn.sql")),
                                  field = field,
                                  vector = vector)
        }





#' Render "WHERE x LIKE y"
#' @description (Deprecated)  This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param term Character string that the field parameter is searched for.
#' @export


constructWhereLike <-
        function(field,
                 term) {
                .Deprecated()

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")


                SqlRender::render(SqlRender::readSql(paste0(path, "/whereLike.sql")),
                                  field = field,
                                  term = term)
        }





#' Render "WHERE lowercase x IN y" Component
#' @description (Deprecated)  This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param vector vector of values that the SQL query is filtering for
#' @export


constructWhereLowerIn <-
        function(field,
                 vector) {
                .Deprecated()

                if (is.character(vector)) {

                        vector <- paste0("'", vector, "'")

                }

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")

                SqlRender::render(SqlRender::readSql(paste0(path, "/whereLowerIn.sql")),
                                  field = field,
                                  vector = vector)
        }





#' Render WHERE lowercase x LIKE y
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param term Character string that the field parameter is searched for.
#' @export


constructWhereLowerLike <-
        function(field,
                 term) {
                .Deprecated()

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")


                SqlRender::render(SqlRender::readSql(paste0(path, "/whereLowerLike.sql")),
                                  field = field,
                                  term = tolower(term))

        }





#' Render "WHERE lowercase x NOT IN y"
#' @description (Deprecated)  This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param vector vector of values that the SQL query is filtering for
#' @export


constructWhereLowerNotIn <-
        function(field,
                 vector) {
                .Deprecated()

                if (is.character(vector)) {

                        vector <- paste0("'", vector, "'")

                }

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")

                SqlRender::render(SqlRender::readSql(paste0(path, "/whereLowerNotIn.sql")),
                                  field = field,
                                  vector = vector)
        }





#' Render "WHERE x NOT IN y"
#' @description (Deprecated)  This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param vector vector of values that the SQL query is filtering for
#' @export


constructWhereNotIn <-
        function(field,
                 vector) {
                .Deprecated()

                if (is.character(vector)) {

                        vector <- paste0("'", vector, "'")

                }

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")

                SqlRender::render(SqlRender::readSql(paste0(path, "/whereNotIn.sql")),
                                  field = field,
                                  vector = vector)
        }





