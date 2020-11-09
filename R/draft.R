#' Construct a base SQL query
#' @description Construct the SELECT {fields} FROM {schema}.{table} base query. The SQL Statement is not terminated for the addition of other parameters generated using "construct" functions.
#' @param distinct if TRUE, the DISTINCT fields will be selected for.
#' @import SqlRender
#' @export

draftBase <-
    function(fields = "*",
             distinct = FALSE,
             schema,
             tableName) {

            if (distinct) {

                    SqlRender::render("SELECT DISTINCT @fields FROM @schema.@tableName",
                                      fields = fields,
                                      schema = schema,
                                      tableName = tableName)


            } else {

                    SqlRender::render("SELECT @fields FROM @schema.@tableName",
                                              fields = fields,
                                              schema = schema,
                                              tableName = tableName)

            }

    }




#' Render y in "WHERE x IN (y)"
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param vector vector of values that the SQL query is filtering for
#' @export


draftIn <-
    function(vector) {

        SqlRender::render("(@vector)",
                          vector = vector)

    }






#' Construct a join part of a SQL Statement
#' @description Construct {join type} JOIN {schema.table.column2} ON {schema.table.column2} = {schema.table.column1} part of the sql statement
#' @import SqlRender
#' @export

draftJoin <-
    function(schema,
             tableName,
             column,
             joinType = "LEFT",
             joinOnSchema,
             joinOnTableName,
             joinOnColumn) {

            SqlRender::render("@joinType JOIN @schema2.@tableName2 ON @schema2.@tableName2.@column2 = @schema1.@tableName1.@column1",
                              schema1 = schema,
                              tableName1 = tableName,
                              column1 = column,
                              joinType = joinType,
                              schema2 = joinOnSchema,
                              tableName2 = joinOnTableName,
                              column2 = joinOnColumn)

    }




#' Construct LIMIT
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param n rows to limit to
#' @export


draftLimit <-
    function(n) {


        SqlRender::render("LIMIT @n",
                          n = n)

    }


#' Construct ORDER BY RANDOM()
#' @param n Row number desired in the output
#' @import SqlRender
#' @export


draftRandom <-
    function(n) {


        SqlRender::render("ORDER BY RANDOM() LIMIT @n",
                          n = n)

    }

#' Construct schemaTableName
#' @description construct schemaTableName from the schema and tableName
#' @export

draftTablePath <-
        function(schema,
                 tableName) {

            .Deprecated("table.path")

            SqlRender::render("@schema.@tableName",
                              schema = schema,
                              tableName = tableName)

        }


#' Construct schemaTableName
#' @description construct schemaTableName from the schema and tableName
#' @export

table.path <-
    function(schema,
             tableName) {

        SqlRender::render("@schema.@tableName",
                          schema = schema,
                          tableName = tableName)

    }




#' Render x in "WHERE x IN y"
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param vector vector of values that the SQL query is filtering for
#' @export


draftWhereIn <-
    function(field,
             vector) {

        if (is.character(vector)) {

            vector <- sQuo(vector)

        }

        SqlRender::render("@field IN (@vector)",
                          field = field,
                          vector = vector)
    }


#' Render "WHERE x LIKE y"
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param term Character string that the field parameter is searched for.
#' @export


draftWhereLike <-
    function(field,
             term) {

        SqlRender::render("@field LIKE '%@term%'",
                          field = field,
                          term = term)
    }




#' Render "WHERE lowercase x IN y" Component
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param vector vector of values that the SQL query is filtering for
#' @export


draftWhereLowerIn <-
    function(field,
             vector) {

        if (is.character(vector)) {

                vector <- sQuo(vector)

        }

        SqlRender::render("LOWER(@field) IN (@vector)",
                          field = field,
                          vector = vector)
    }


#' Render WHERE lowercase x LIKE y
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param term Character string that the field parameter is searched for.
#' @export


draftWhereLowerLike <-
    function(field,
             term) {

        SqlRender::render("LOWER(@field) LIKE '%@term%'",
                          field = field,
                          term = tolower(term))

    }





#' Render "WHERE lowercase x NOT IN y"
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param vector vector of values that the SQL query is filtering for
#' @export


draftWhereLowerNotIn <-
    function(field,
             vector) {

        if (is.character(vector)) {

            vector <- sQuo(vector)

        }

        SqlRender::render("LOWER(@field) NOT IN (@vector)",
                          field = field,
                          vector = vector)
    }


#' Render "WHERE x NOT IN y"
#' @description This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param field Single field to be filtered for
#' @param vector vector of values that the SQL query is filtering for
#' @export


draftWhereNotIn <-
    function(field,
             vector) {

        if (is.character(vector)) {

            vector <- sQuo(vector)

        }

        SqlRender::render("@field NOT IN (@vector)",
                          field = field,
                          vector = vector)
    }


#' @title
#' Paste All Drafted Where Statements
#'
#' @rdname pasteWheres
#'
#' @importFrom rlang list2
#' @export

pasteWheres <-
    function(...) {

        Args <- rlang::list2(...)

        paste0("WHERE ",
                unlist(Args) %>%
                    paste(collapse = " AND ")
        )

    }



