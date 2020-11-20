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



#' Export a sql statement to a file
#' @param sql_statement sql statement R object
#' @param file File to write to.
#' @param ... Additional arguments passed to the readr::write_lines function
#' @importFrom readr write_lines
#' @export

saveSQL <-
        function(sql_statement,
                 file,
                 append = TRUE,
                 ...) {

                .Deprecated("write_sql_file")

                readr::write_lines(x = sql_statement,
                                   path = file,
                                   append = append,
                                   ...
                )
        }

#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropSchema <-
        function(conn,
                 schema,
                 cascade = FALSE,
                 if_exists = TRUE,
                 ...) {


                .Deprecated("dropCascade")


                sql_statement <- renderDropSchema(schema = schema,
                                                  cascade = cascade,
                                                  if_exists = if_exists)

                send(conn = conn,
                     sql_statement = sql_statement,
                     ...)

        }





#' Concatenate 2 WHERE constructs
#' @description When 2 WHERE constructs are included in the SQL Statement, they are concatenated and the 2nd "WHERE" is replaced with an "AND".
#' @param AND If TRUE, the WHERE constructs are concatenated with "AND". If FALSE, the concatenation is performed with an "OR".
#' @import stringr
#' @export

concatWhereConstructs <-
    function(sql_construct,
             where_sql_construct_2,
             ...,
             AND = TRUE) {


            .Deprecated()

            if (missing(...)) {

                            if (AND) {

                                    c(sql_construct,
                                      stringr::str_replace(where_sql_construct_2,
                                                           "WHERE",
                                                           "AND")) %>%
                                                paste(collapse = " ")
                            } else {

                                    c(sql_construct,
                                      stringr::str_replace(where_sql_construct_2,
                                                           "WHERE",
                                                           "OR")) %>%
                                            paste(collapse = " ")

                            }
            } else {

                    Args <- list(where_sql_construct_2,
                                 ...) %>%
                                unlist()

                    if (AND) {

                            c(sql_construct,
                            stringr::str_replace_all(Args,
                                                     "WHERE",
                                                     "AND")) %>%
                                    paste(collapse = " ")


                    } else {

                            c(sql_construct,
                              stringr::str_replace_all(Args,
                                                       "WHERE",
                                                       "OR")) %>%
                                    paste(collapse = " ")



                    }

            }

    }


#' Append to am Existing Table
#' @import DatabaseConnector
#' @description Like the writeTable function, this function is a wrapper around a DatabaseConnector function rather than one where a SQL statement is rendered using the SqlRender package. This function performs the additional step of converting all inputs to the data.frame class, especially in cases where the input is a tibble.
#' @param tableName Name of table to write to.
#' @param schema schema where `tableName` is located.
#' @param .data dataframe to append
#' @param ... Additional arguments passed to DatabaseConnector::dbAppendTable
#' @export


appendTable2 <-
    function(conn = conn,
             schema,
             tableName,
             .data,
             ...) {

        .Deprecated(new = "append")

        schemaTableName <- constructSchemaTableName(schema = schema,
                                                    tableName = tableName)


        if (nrow(.data)) {

            DatabaseConnector::dbAppendTable(conn = conn,
                                             name = schemaTableName,
                                             value = .data %>%
                                                 as.data.frame(),
                                             ...)
        }

    }

#' Write a Table
#' @import DatabaseConnector
#' @description Unlike the dropTable and renameTable functions, this function is a wrapper around the DatabaseConnector::dbWriteTable function rather than one where a SQL statement is rendered using the SqlRender package. This function that converts all inputs to the data.frame class, especially in cases where the input is a tibble, in which case an error would be thrown when writing.
#' @param ... Additional arguments passed to DatabaseConnector::dbWriteTable
#' @export


writeTable2 <-
    function(conn = conn,
             schema,
             tableName,
             .data,
             ...) {

        .Deprecated(new = "write")

        schemaTableName <- constructSchemaTableName(schema = schema,
                                                    tableName = tableName)

        DatabaseConnector::dbWriteTable(conn = conn,
                                        name = schemaTableName,
                                        value = .data %>%
                                            as.data.frame(),
                                        ...)

    }

#' Get Full Table
#' @export


getTable <-
    function(conn,
             schema,
             tableName) {

        .Deprecated(new = "readTable")

        query(conn = conn,
              buildQuery(schema = schema,
                         tableName = tableName))
    }


#' Refresh Table with New Data
#' @description A refresh is when a table needs to be overwritten and the table that is being overwritten is off-loaded to another table with today's date and the index of iterations for that day.
#' @import stringr
#' @import secretary
#' @import purrr
#' @export


refreshTable <-
    function(conn,
             schema,
             tableName,
             .data) {

        .Deprecated()

        tableNameHist <- appendDate(name = toupper(tableName))


        todayTables <-
            grep(tableNameHist,
                 lsTables(conn = conn,
                          schema = schema),
                 value = TRUE)

        n <- length(todayTables)


        # If this table has not been written yet today
        if (n == 0) {
            secretary::typewrite_bold("No Off-Loaded Tables Today")
            secretary::typewrite_bold("Next Table Name:", tableNameHist)
            # If more than 1 table has been written today, the new table name would be the length of the list of today's table + 1
        } else {
            secretary::typewrite_bold("Off-Loaded Tables Today:")
            todayTables %>%
                purrr::map(function(x) secretary::typewrite(x, tabs = 1))

            tableNameHist <- paste0(tableNameHist, "_", (1+n))
            secretary::typewrite_bold("Next Table Name:", tableNameHist)

        }

        secretary::press_enter()

        renameTable(conn = conn,
                    schema = schema,
                    tableName = tableName,
                    newTableName = tableNameHist)

        secretary::typewrite_bold(tableName, "renamed to", tableNameHist)

        writeTable(conn = conn,
                   tableName = tableName,
                   schema = schema,
                   .data = .data %>%
                       as.data.frame())

        secretary::typewrite_bold("New", tableName, "written.")

    }





#' Rename a table in a Postgres schema
#' @description This function will rename a table in a schema, but not move it out of a schema.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

renameTable <-
    function(conn,
             schema,
             tableName,
             newTableName,
             ...) {


        sql_statement <- renderRenameTable(schema = schema,
                                           tableName = tableName,
                                           newTableName = newTableName)

        send(conn = conn,
             sql_statement = sql_statement,
             ...)

    }



#' @title
#' Is the Connection Closed?
#'
#' @description
#' This function checks if a connection object is closed.
#'
#' @param conn Postgres connection object
#'
#' @return
#' TRUE if the connection is closed and FALSE invisibly if it is open.
#'
#' @rdname isClosed
#' @export

isClosed <-
    function(conn) {

        .Deprecated(new = "is_conn_open")

        results <- tryCatch(print(conn),
                            error = function(e) NULL)

        if (is.null(results)) {
            TRUE
        } else {
            invisible(FALSE)
        }
    }


#' @title
#' Remove a Closed Connection Object
#'
#' @description
#' (Deprecated) This function removes a connection object from the Global Environment if it is closed.
#'
#' @param conn Postgres connection object
#'
#' @rdname rmIfClosed
#' @export

rmIfClosed <-
    function(conn) {

        .Deprecated(new = "rm_if_closed")

        results <- isClosed(conn = conn)

        if (results == TRUE) {

            rm(list = deparse(substitute(conn)), envir = globalenv())

        }

    }


#' Get SourceFile Path
#' @description This function provides the path for files installed within a given package's library.
#' @param instSubdir Name of subdirectory in the inst/ folder
#' @param FileName Name of file in subdirectory
#' @param package Package name
#' @noRd


sourceFilePath <-
    function(instSubdir,
             FileName,
             package) {

        .Deprecated()
        paste0(system.file(package = package), "/", instSubdir, "/", FileName)
    }

#' Terminate a SQL Statement with a semicolon
#' @noRd

terminateBuild <-
    function(sql_statement) {

        .Deprecated()

        paste0(sql_statement, ";")

    }

