#' Append Current Date to a String
#' @description Date is appended in "YYYY_mm_dd" Format
#' @import stringr
#' @export

append_date <-
        function(name) {
                .Deprecated(new = "affix_date")
                paste0(name, "_", stringr::str_replace_all(as.character(Sys.Date()), "[-]{1}", "_"))
        }

#' Construct a base SQL query
#' @description Construct the SELECT {fields} FROM {schema}.{table} base query. The SQL Statement is not terminated for the addition of other parameters generated using "construct" functions.
#' @param distinct if TRUE, the DISTINCT fields will be selected for.
#' @import SqlRender
#' @export

construct_base <-
        function(fields = "*",
                 distinct = FALSE,
                 schema = "public",
                 tableName) {

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


construct_in <-
        function(vector) {
                .Deprecated()
                base <- system.file(package='pg13')
                path <- paste0(base, "/sql_constr")

                SqlRender::render(SqlRender::readSql(paste0(path, "/in.sql")),
                                  vector = vector)
        }





#' Construct LIMIT
#' @description (Deprecated)  This is a non-terminal render, meaning that the SQL component will not be terminated with a semicolon in order to construct complex SQL queries.
#' @import SqlRender
#' @param n rows to limit to
#' @export


construct_limit <-
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


construct_random <-
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

construct_schema_table_name <-
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


construct_where_in <-
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


construct_where_like <-
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


construct_where_lower_in <-
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


construct_where_lower_like <-
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


construct_where_lower_not_in <-
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


construct_where_not_in <-
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

save_sql <-
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

drop_schema <-
        function(conn,
                 schema,
                 cascade = FALSE,
                 if_exists = TRUE,
                 ...) {


                .Deprecated("dropCascade")


                sql_statement <- render_drop_schema(schema = schema,
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

concat_where_constructs <-
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

                    args <- list(where_sql_construct_2,
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


append_table2 <-
    function(conn = conn,
             schema,
             tableName,
             .data,
             ...) {

        .Deprecated(new = "append")

        schema_table_name <- construct_schema_table_name(schema = schema,
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


write_table2 <-
    function(conn = conn,
             schema,
             tableName,
             .data,
             ...) {

        .Deprecated(new = "write")

        schema_table_name <- construct_schema_table_name(schema = schema,
                                                    tableName = tableName)

        DatabaseConnector::dbWriteTable(conn = conn,
                                        name = schemaTableName,
                                        value = .data %>%
                                            as.data.frame(),
                                        ...)

    }

#' Get Full Table
#' @export


get_table <-
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


refresh_table <-
    function(conn,
             schema,
             tableName,
             .data) {

        .Deprecated()

        table_name_hist <- append_date(name = toupper(table_name))


        today_tables <-
            grep(tableNameHist,
                 lsTables(conn = conn,
                          schema = schema),
                 value = TRUE)

        n <- length(today_tables)


        # If this table has not been written yet today
        if (n == 0) {
            secretary::typewrite_bold("No Off-Loaded Tables Today")
            secretary::typewrite_bold("Next Table Name:", tableNameHist)
            # If more than 1 table has been written today, the new table name would be the length of the list of today's table + 1
        } else {
            secretary::typewrite_bold("Off-Loaded Tables Today:")
            todayTables %>%
                purrr::map(function(x) secretary::typewrite(x, tabs = 1))

            table_name_hist <- paste0(table_name_hist, "_", (1+n))
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

rename_table <-
    function(conn,
             schema,
             tableName,
             newTableName,
             ...) {


        sql_statement <- render_rename_table(schema = schema,
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
#' @rdname is_closed
#' @export

is_closed <-
    function(conn) {

        .Deprecated(new = "is_conn_open")

        results <- try_catch(print(conn),
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
#' @rdname rm_if_closed
#' @export

rm_if_closed <-
    function(conn) {

        .Deprecated(new = "rm_if_closed")

        results <- is_closed(conn = conn)

        if (results == TRUE) {

            rm(list = deparse(substitute(conn)), envir = globalenv())

        }

    }


#' Get SourceFile Path
#' @description This function provides the path for files installed within a given package's library.
#' @param instSubdir Name of subdirectory in the inst/ folder
#' @param FileName Name of file in subdirectory
#' @param package Package name
#' @export


source_file_path <-
    function(instSubdir,
             FileName,
             package) {

        .Deprecated()
        paste0(system.file(package = package), "/", instSubdir, "/", FileName)
    }

#' Terminate a SQL Statement with a semicolon
#' @export

terminate_build <-
    function(sql_statement) {

        paste0(sql_statement, ";")

    }






#' Execute SQL
#' @description This function differs from the send() and query() functions in that it provides additional features such as a progress bar and time estimations.
#' @export

execute <-
        function(conn,
                 sql_statement,
                 profile = FALSE,
                 progressBar = TRUE,
                 reportOverallTime = TRUE,
                 ...) {

                .Deprecated(new = "queries")
                DatabaseConnector::executeSql(connection = conn,
                                              sql = sql_statement,
                                              profile = profile,
                                              progressBar = progressBar,
                                              reportOverallTime = reportOverallTime,
                                              ...)
        }










#' Query local Postgess from a file
#' @import DatabaseConnector
#' @export

local_file_query <-
    function (file,
              dbname = "athena",
              port = 5432) {

        .Deprecated()
        conn <- local_connect(dbname = dbname,
                             port = port)

        data <- file_query(conn = conn,
                          file = file)

        dc(conn = conn,
           remove = FALSE)

        return(data)
    }





#' Send Statement to a local Postgess from a file
#' @import DatabaseConnector
#' @export

local_file_send <-
    function (file,
              dbname = "athena",
              port = 5432) {

        .Deprecated()

        conn <- local_connect(dbname = dbname,
                             port = port)

        data <- file_send(conn = conn,
                          file = file)

        dc(conn = conn,
           remove = FALSE)

        return(data)
    }






#' Query local Postgess
#' @import DatabaseConnector
#' @export

local_query <-
    function (sql_statement,
              dbname = "athena",
              port = 5432) {


        .Deprecated()

            conn <- local_connect(dbname = dbname,
                                 port = port)

            data <- query(conn = conn,
                          sql_statement = sql_statement)

            dc(conn = conn,
               remove = FALSE)

            return(data)
    }





#' Send a SQL Statement to a Local Postgres
#' @export


local_send <-
    function(sql_statement,
             dbname = "athena",
             port = 5432) {

                .Deprecated()

                conn <- local_connect(dbname = dbname,
                                    port = port)

                send(conn = conn,
                     sql_statement = sql_statement)

                dc(conn = conn,
                   remove = FALSE)
    }















#' Parse SQL into Single Statements
#' @description
#' This function is helpful in splitting a SQL file into digestible smaller statements based on the semicolon for troubleshooting or to skip of the statements that do not work within the full input SQL.
#' @import centipede
#' @return
#' List of sql statements
#' @export

parse_sql <-
        function(sql_statement) {

                .Deprecated(new = "execute_n")
                centipede::strsplit(sql_statement, split = "[\\;]{1}", type = "after") %>%
                        unlist() %>%
                        trimws() %>%
                        centipede::no_blank() %>%
                        as.list()
        }





#' Export a sql statement to a file
#' @param sql_statement sql statement R object
#' @param file File to write to.
#' @param ... Additional arguments passed to the readr::write_lines function
#' @importFrom readr write_lines
#' @export

save_sql <-
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















#' Build a JOIN SQL Query
#' @description A SQL query is built using the given arguments. Currently, only 1 whereIn and whereNot in parameters can be set.
#' @return SQL statement as a character string.
#' @import purrr
#' @import stringr
#' @export

buildJoinQuery <-
    function(fields = "*",
             distinct = FALSE,
             schema,
             tableName,
             column,
             joinOnSchema,
             joinOnTableName,
             joinOnColumn,
             joinType = "LEFT",
             whereInField = NULL,
             whereInVector = NULL,
             whereNotInField = NULL,
             whereNotInVector = NULL,
             caseInsensitive = TRUE,
             n = NULL,
             n_type = c("limit", "random")) {

                    ######
                    # QA to make sure all whereIn and n  arguments have been supplied in pairs
                    #####
                    whereIns <- list(whereInField, whereInVector) %>%
                                        purrr::set_names(c("field", "vector")) %>%
                                        purrr::keep(~!is.null(.))
                    whereNotIns <- list(whereNotInField, whereNotInVector) %>%
                                        purrr::set_names(c("field", "vector")) %>%
                                        purrr::keep(~!is.null(.))


                    list(whereIns, whereNotIns) %>%
                        purrr::map2(list("whereIn", "whereNotIn"),
                                   function(x,y) if (!(length(x) %in% c(0,2))) {stop('both "', y, '" arguments must be supplied')})

                    ######
                    # QA to make sure all n arugments have been supplied
                    #####

                    if (length(n) == 1 & length(n_type) != 1) {

                            n_type <- "limit"

                            warning('"n_type" set to "limit"')

                    }

                    #####
                    # Start
                    #####
                    sql_construct  <- constructBase(fields = fields,
                                                    distinct = distinct,
                                                    schema = schema,
                                                    tableName = tableName)

                    # Add join
                    sql_construct <-
                            c(sql_construct,
                              constructJoin(schema = schema,
                                            tableName = tableName,
                                            column = column,
                                            joinType = joinType,
                                            joinOnSchema = joinOnSchema,
                                            joinOnTableName = joinOnTableName,
                                            joinOnColumn = joinOnColumn)) %>%
                            paste(collapse = " ")



                    if (caseInsensitive) {


                        # If WhereIn arguments are not null include it in build
                        if (length(whereIns) == 2) {

                            sql_construct <-
                                paste(sql_construct,
                                      constructWhereLowerIn(field = whereIns$field,
                                                       vector = tolower(whereIns$vector)),
                                      collapse = " ")

                            # If WhereNotIn arguments are supplied on top of the WhereIn, add them to the query by modifying the constructWhereNotIn output by replacing the second "WHERE" with "AND"
                            if (length(whereNotIns) == 2) {


                                sql_construct <-
                                    paste(sql_construct,
                                          "AND",
                                          constructWhereLowerNotIn(field = whereNotIns$field,
                                                              vector = tolower(whereNotIns$vector)) %>%
                                              stringr::str_remove_all("WHERE") %>%
                                              trimws(),
                                          collapse = " ")


                            }

                        } else {

                            # Building a query if only whereNotIn arguments were supplied
                            if (length(whereNotIns) == 2) {


                                sql_construct <-
                                    paste(sql_construct,
                                          constructWhereLowerNotIn(field = whereNotIns$field,
                                                              vector = tolower(whereNotIns$vector)),
                                          collapse = " ")


                            }



                        }

                        # If n arguments are not null include it in build, as either a limit or random sample of size n
                        if (!is.null(n)) {

                            if (n_type == "limit") {

                                sql_construct <-
                                    paste(sql_construct,
                                          constructLimit(n = n),
                                          collapse = " ")

                            } else if (n_type == "random") {

                                sql_construct <-
                                    paste(sql_construct,
                                          constructRandom(n = n),
                                          collapse = " ")

                            } else {

                                warning('"n_type" not recognized and "n" removed from build')


                            }

                        }









                    } else {



                                    # If WhereIn arguments are not null include it in build
                                    if (length(whereIns) == 2) {

                                            sql_construct <-
                                                    paste(sql_construct,
                                                          constructWhereIn(field = whereIns$field,
                                                                            vector = whereIns$vector),
                                                          collapse = " ")

                                            # If WhereNotIn arguments are supplied on top of the WhereIn, add them to the query by modifying the constructWhereNotIn output by replacing the second "WHERE" with "AND"
                                            if (length(whereNotIns) == 2) {


                                                            sql_construct <-
                                                                paste(sql_construct,
                                                                      "AND",
                                                                      constructWhereNotIn(field = whereNotIns$field,
                                                                                       vector = whereNotIns$vector) %>%
                                                                          stringr::str_remove_all("WHERE") %>%
                                                                          trimws(),
                                                                      collapse = " ")


                                            }

                                    } else {

                                                # Building a query if only whereNotIn arguments were supplied
                                                if (length(whereNotIns) == 2) {


                                                    sql_construct <-
                                                        paste(sql_construct,
                                                              constructWhereNotIn(field = whereNotIns$field,
                                                                                  vector = whereNotIns$vector),
                                                              collapse = " ")


                                                }



                                    }

                                    # If n arguments are not null include it in build, as either a limit or random sample of size n
                                    if (!is.null(n)) {

                                                if (n_type == "limit") {

                                                    sql_construct <-
                                                                paste(sql_construct,
                                                                      constructLimit(n = n),
                                                                      collapse = " ")

                                                } else if (n_type == "random") {

                                                    sql_construct <-
                                                        paste(sql_construct,
                                                              constructRandom(n = n),
                                                              collapse = " ")

                                                } else {

                                                    warning('"n_type" not recognized and "n" removed from build')


                                                }

                                    }

                    }

                    #Add a semicolon to finish the query
                    sql_construct %>%
                            stringr::str_replace_all(pattern = "[\n]{2,}",
                                                     replacement = "\n") %>%
                            terminateBuild()


    }






#' Build a SQL Query
#' @description A SQL query is built using the given arguments. Currently, only 1 whereIn and whereNot in parameters can be set.
#' @return SQL statement as a character string.
#' @import purrr
#' @import stringr
#' @export

buildQuery <-
    function(fields = "*",
             distinct = FALSE,
             schema,
             tableName,
             whereInField = NULL,
             whereInVector = NULL,
             whereNotInField = NULL,
             whereNotInVector = NULL,
             caseInsensitive = TRUE,
             n = NULL,
             n_type = c("limit", "random")) {

                    ######
                    # QA to make sure all whereIn and n  arguments have been supplied in pairs
                    #####
                    whereIns <- list(whereInField, whereInVector) %>%
                                        purrr::set_names(c("field", "vector")) %>%
                                        purrr::keep(~!is.null(.))
                    whereNotIns <- list(whereNotInField, whereNotInVector) %>%
                                        purrr::set_names(c("field", "vector")) %>%
                                        purrr::keep(~!is.null(.))


                    list(whereIns, whereNotIns) %>%
                        purrr::map2(list("whereIn", "whereNotIn"),
                                   function(x,y) if (!(length(x) %in% c(0,2))) {stop('both "', y, '" arguments must be supplied')})

                    ######
                    # QA to make sure all n arugments have been supplied
                    #####

                    if (length(n) == 1 & length(n_type) != 1) {

                            n_type <- "limit"

                            warning('"n_type" set to "limit"')

                    }

                    #####
                    # Start
                    #####
                    sql_construct  <- constructBase(fields = fields,
                                                    distinct = distinct,
                                                    schema = schema,
                                                    tableName = tableName)


                    if (caseInsensitive) {


                        # If WhereIn arguments are not null include it in build
                        if (length(whereIns) == 2) {

                            sql_construct <-
                                paste(sql_construct,
                                      constructWhereLowerIn(field = whereIns$field,
                                                       vector = tolower(whereIns$vector)),
                                      collapse = " ")

                            # If WhereNotIn arguments are supplied on top of the WhereIn, add them to the query by modifying the constructWhereNotIn output by replacing the second "WHERE" with "AND"
                            if (length(whereNotIns) == 2) {


                                sql_construct <-
                                    paste(sql_construct,
                                          "AND",
                                          constructWhereLowerNotIn(field = whereNotIns$field,
                                                              vector = tolower(whereNotIns$vector)) %>%
                                              stringr::str_remove_all("WHERE") %>%
                                              trimws(),
                                          collapse = " ")


                            }

                        } else {

                            # Building a query if only whereNotIn arguments were supplied
                            if (length(whereNotIns) == 2) {


                                sql_construct <-
                                    paste(sql_construct,
                                          constructWhereLowerNotIn(field = whereNotIns$field,
                                                              vector = tolower(whereNotIns$vector)),
                                          collapse = " ")


                            }



                        }

                        # If n arguments are not null include it in build, as either a limit or random sample of size n
                        if (!is.null(n)) {

                            if (n_type == "limit") {

                                sql_construct <-
                                    paste(sql_construct,
                                          constructLimit(n = n),
                                          collapse = " ")

                            } else if (n_type == "random") {

                                sql_construct <-
                                    paste(sql_construct,
                                          constructRandom(n = n),
                                          collapse = " ")

                            } else {

                                warning('"n_type" not recognized and "n" removed from build')


                            }

                        }









                    } else {



                                    # If WhereIn arguments are not null include it in build
                                    if (length(whereIns) == 2) {

                                            sql_construct <-
                                                    paste(sql_construct,
                                                          constructWhereIn(field = whereIns$field,
                                                                            vector = whereIns$vector),
                                                          collapse = " ")

                                            # If WhereNotIn arguments are supplied on top of the WhereIn, add them to the query by modifying the constructWhereNotIn output by replacing the second "WHERE" with "AND"
                                            if (length(whereNotIns) == 2) {


                                                            sql_construct <-
                                                                paste(sql_construct,
                                                                      "AND",
                                                                      constructWhereNotIn(field = whereNotIns$field,
                                                                                       vector = whereNotIns$vector) %>%
                                                                          stringr::str_remove_all("WHERE") %>%
                                                                          trimws(),
                                                                      collapse = " ")


                                            }

                                    } else {

                                                # Building a query if only whereNotIn arguments were supplied
                                                if (length(whereNotIns) == 2) {


                                                    sql_construct <-
                                                        paste(sql_construct,
                                                              constructWhereNotIn(field = whereNotIns$field,
                                                                                  vector = whereNotIns$vector),
                                                              collapse = " ")


                                                }



                                    }

                                    # If n arguments are not null include it in build, as either a limit or random sample of size n
                                    if (!is.null(n)) {

                                                if (n_type == "limit") {

                                                    sql_construct <-
                                                                paste(sql_construct,
                                                                      constructLimit(n = n),
                                                                      collapse = " ")

                                                } else if (n_type == "random") {

                                                    sql_construct <-
                                                        paste(sql_construct,
                                                              constructRandom(n = n),
                                                              collapse = " ")

                                                } else {

                                                    warning('"n_type" not recognized and "n" removed from build')


                                                }

                                    }

                    }

                    #Add a semicolon to finish the query
                    sql_construct %>%
                            stringr::str_replace_all(pattern = "[\n]{2,}",
                                                     replacement = "\n") %>%
                            terminateBuild()


    }






#' Writes a Like SQL Query
#' @return a SQL Query as a character string.
#' @export

buildQueryLike <-
        function(fields = "*",
                 distinct = FALSE,
                 schema,
                 tableName,
                 whereLikeField,
                 whereLikeValue,
                 caseInsensitive = TRUE,
                 limit_n = NULL) {


                sql_construct <-
                constructBase(fields = fields,
                              distinct = distinct,
                              schema = schema,
                              tableName = tableName)

                if (caseInsensitive) {
                        sql_construct <-
                                c(sql_construct,
                                  constructWhereLowerLike(field = whereLikeField,
                                                          term = tolower(whereLikeValue))
                                ) %>%
                                paste(collapse = " ")
                } else {
                        sql_construct <-
                                c(sql_construct,
                                  constructWhereLike(field = whereLikeField,
                                                          term = whereLikeValue)
                                ) %>%
                                paste(collapse = " ")

                }
                sql_construct %>%
                        stringr::str_replace_all(pattern = "[\n]{2,}",
                                                 replacement = "\n") %>%
                    terminateBuild()


        }






#' Writes a SQL Query Loop
#' @description This function writes a SQL Query that loops over the words in a string.
#' @return a SQL Query as a character string.
#' @export

buildQueryString <-
        function(fields = "*",
                 distinct = FALSE,
                 schema,
                 tableName,
                 whereLikeField,
                 string,
                 split,
                 caseInsensitive = TRUE,
                 limit_n = NULL) {


                sql_construct <-
                constructBase(fields = fields,
                              distinct = distinct,
                              schema = schema,
                              tableName = tableName)

                Args <- strsplit(string, split = split) %>%
                                        unlist()


                if (caseInsensitive) {

                            Args <- tolower(Args)

                            for (i in 1:length(Args)) {

                                        if (i == 1) {
                                                    sql_construct <-
                                                        c(sql_construct,
                                                          constructWhereLowerLike(field = whereLikeField,
                                                                                  term = Args[1])
                                                        ) %>%
                                                        paste(collapse = " ")
                                        } else {


                                                sql_construct <-
                                                    concatWhereConstructs(sql_construct,
                                                                               where_sql_construct_2 = constructWhereLowerLike(field = whereLikeField, term = Args[i]))


                                        }
                            }

                } else {


                    for (i in 1:length(Args)) {

                                if (i == 1) {
                                            sql_construct <-
                                                c(sql_construct,
                                                  constructWhereLike(field = whereLikeField,
                                                                          term = Args[1])
                                                ) %>%
                                                paste(collapse = " ")
                                } else {


                                            sql_construct <-
                                                concatWhereConstructs(sql_construct,
                                                                      where_sql_construct_2 = constructWhereLike(field = whereLikeField, term = Args[i]))


                                }
                    }
                }

                if (!is.null(limit_n)) {

                            sql_construct <-
                                    c(sql_construct,
                                      constructLimit(n = limit_n)) %>%
                                        paste(collapse = " ")


                }

                sql_construct %>%
                        stringr::str_replace_all(pattern = "[\n]{2,}",
                                                 replacement = "\n") %>%
                        terminateBuild()


        }











#' Terminate a SQL Statement with a semicolon
#' @export

terminateBuild <-
    function(sql_statement) {

                paste0(sql_statement, ";")

    }










#' Cache a Resultset from a Join
#' @description This function will cache the resultset from a SQL query created by buildJoinQuery to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param ... Arguments passed to the buildJoinQuery function
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

cacheJoin <-
        function(.data,
                 ...,
                 db,
                 schema) {

                R.cache::saveCache(object = .data,
                                   key = list(...),
                                   dirs = paste0(db, "/", schema))
        }





#' Cache a Query Resultset
#' @description This function will cache the resultset from a SQL query to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

cacheQuery <-
        function(.data,
                 sqlQuery,
                 db) {

                R.cache::saveCache(object = .data,
                                   key = list(sqlQuery),
                                   dirs = db)
        }





#' Clear a Cache
#' @description This function will clear a cache subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

clearCache <-
        function(db) {

                R.cache::clearCache(path = getCachePath(db))


        }





#' Load a Cached Join Resultset
#' @description This function will load the cached resultset from a SQL query created by the buildJoinQuery function to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param ... Arguments passed to the buildJoinQuery function
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

loadCachedJoin <-
        function(...,
                 db,
                 schema) {

                R.cache::loadCache(key = list(...),
                                   dirs = paste0(db, "/", schema))
        }





#' Load a Cached Query Resultset
#' @description This function will load the cached resultset from a SQL query to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

loadCachedQuery <-
        function(sqlQuery,
                 db) {

                R.cache::loadCache(key = list(sqlQuery),
                                   dirs = db)
        }










#' @title
#' Cache a Resultset
#'
#' @description
#' Cache a resultset with the SQL Statement as the key to a "fantasia" cache directory using a wrapper around the \code{\link[R.cache]{saveCache}} function.
#'
#' @param object        R object to cache.
#' @param sql_statement SQL Statement from which a hexadecimal hash code will be generated and appended to the cache filename.
#' @param dirs          Argument passed to \code{\link[R.cache]{saveCache}}.
#'
#' @return
#' Invisibly returns the path to the cache file.
#'
#' @importFrom R.cache saveCache
#'
#' @rdname cache
#' @family cache functions
#' @export

cache <-
        function(object,
                 sql_statement,
                 dirs) {

                key <- list(sql_statement)
                x <- R.cache::saveCache(object = object,
                                        key = key,
                                        dirs = dirs)
                invisible(x)
        }


#' @title
#' Load a Cached Resultset from a SQL Query
#'
#' @description
#' Load a resultset from a "fantasia" cache directory with the SQL Statement as the key if it was previously cached or is within expiration period as indicated by the `hrs_expired` argument. This function is a wrapper around the \code{\link[R.cache]{loadCache}} function.
#'
#' @details
#' An expiration period is incorporated by first determining the cache path and its mtime from the file information. If the cache file exists, the resultset will be returned if the the difference between the current time and the mtime of the cache file is less than or equal to the `hrs_expired` value.
#'
#' @param sql_statement SQL Statement from which a hexadecimal hash code was be generated and appended to the cache filename. See \code{\link{cache}} for more information.
#' @param dirs Argument passed to \code{\link[R.cache]{findCache}} and \code{\link[R.cache]{loadCache}}
#'
#' @return
#' Resultset if the cached file exists and is not expired and NULL otherwise.
#'
#' @rdname loadCache
#' @family cache functions
#' @importFrom R.cache findCache loadCache
#' @export

loadCache <-
        function(sql_statement,
                 hrs_expired = 8,
                 dirs) {

                key <- list(sql_statement)
                # Get path to cache file if it exists

                cache_file_path <-
                        R.cache::findCache(key = key,
                                           dirs = dirs)


                if (!is.null(cache_file_path)) {

                        is_expired <-
                                difftime(time1 = Sys.time(),
                                         time2 = file.info(cache_file_path)$mtime,
                                         units = "hours") > hrs_expired

                        if (is_expired) {

                                NULL

                        } else {

                                R.cache::loadCache(key = key,
                                                   dirs = dirs)

                        }

                } else {

                        R.cache::loadCache(key = key,
                                           dirs = dirs)
                }

        }





#' @title
#' Postgres Connection
#'
#' @description
#' Creating a connection to a Postgres Database involves first making a connectionDetails object, which is a list of credentials and other parameters for the target database, followed by making the database connection itself that is declared in a connection class object.
#'
#' @seealso
#'  \code{\link[DatabaseConnector]{createConnectionDetails}},
#'  \code{\link[DatabaseConnector]{connect}}
#'
#' @name connection
NULL

#' @title
#' Connect to a Postgres Database
#'
#' @export
#'
#' @importFrom DatabaseConnector createConnectionDetails connect
#' @export


connDB <-
        function(user,
                 password,
                 port,
                 server) {

                conn_details <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                           user = user,
                                                                           password = password,
                                                                           port = port,
                                                                           server = server)

                DatabaseConnector::connect(conn_details)
        }


#' @title
#' Connect to a Postgres Database
#'
#' @inheritParams DatabaseConnector::createConnectionDetails
#' @param verbose If TRUE, returns console messages when a connection has been secured.
#' @inherit DatabaseConnector::connect return
#'
#' @rdname connect
#'
#' @export

connect <-
        function() {
                "dummy"
        }


#' @title
#' Connection Function Factory
#'
#' @description
#' Customize a connection function.
#'
#' @inheritParams connect
#'
#' @rdname connect_ff
#' @export

connect_ff <-
        function() {
                "dummy"
        }


#' Connect without Console Messages
#' @export
#' @export

quietly.connDB <-
        function() {
                "dummy"
        }

#' @title
#' Disconnect a Connection
#'
#' @description
#' Disconnect a Postgres Connection object with the option of removing the object from the parent environment.
#'
#' @inheritParams base_args
#' @param verbose If TRUE, returns a console message when the connection has been closed.
#' @param ...           Additional arguments passed to \code{\link[DatabaseConnector]{dbDisconnect}}.
#' @param remove        If TRUE, the Connection object argument is removed from the parent environment.
#'
#' @rdname dc
#'
#' @importFrom DatabaseConnector dbDisconnect
#'
#' @export

dc <-
        function(conn,
                 ...,
                 verbose = TRUE,
                 remove = FALSE) {

                DatabaseConnector::dbDisconnect(conn = conn,
                                                ...)

                if (verbose) {
                        secretary::typewrite("Postgres connection closed")
                }

                if (remove) {

                        rm(list = deparse(substitute(conn)), envir = parent.frame())

                }
        }


#' @title
#' Remove a Closed Connection Object
#'
#' @description
#' If a connection is not open, it is removed from the parent environment from which this function is called.
#'
#' @inheritParams base_args
#'
#' @rdname rm_if_closed
#'
#' @export


rm_if_closed <-
        function(conn) {

                if (!is_conn_open(conn = conn)) {

                        rm(list = deparse(substitute(conn)), envir = parent.frame())

                }


        }





#' @title
#' Draft SQL to Create a Table
#'
#' @description
#' Draft a SQL that creates a table using DDL derived from a dataframe. Drafting is stopped if the given tablename or fields in the dataframe are a SQL reserve word.
#' @param ... Named vector of field names and their corresponding data definition.
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[SqlRender]{render}}
#' @rdname createTable
#' @export
#' @importFrom rlang list2
#' @importFrom SqlRender render

draftCreateTable <-
        function(schema,
                 tableName,
                 if_not_exists = TRUE,
                 ...) {


                ddl <- rlang::list2(...)
                fields <- names(ddl)

                if (any(isReserved(tableName, fields))) {

                        stop("Cannot use reserved sql words.")

                }

                ddl <- mapply(paste, fields, ddl, collapse = " ")
                ddl <- paste(ddl, collapse = ",\n")

                if (if_not_exists) {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                        CREATE TABLE IF NOT EXISTS @schema.@tableName (
                                                @ddl
                                        );
                                        ",
                                        schema = schema,
                                        tableName = tableName,
                                        ddl = ddl
                                )

                } else {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                        CREATE TABLE @schema.@tableName (
                                                @ddl
                                        );
                                        ",
                                        schema = schema,
                                        tableName = tableName,
                                        ddl = ddl
                                )


                }

                sql_statement
        }

#' @title
#' Create a Table
#' @param ... Named vector of field names and their corresponding data definition.
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[pg13]{send}}
#' @rdname createTable
#' @export
#' @importFrom rlang list2
#' @importFrom SqlRender render

createTable <-
        function(conn,
                 schema,
                 tableName,
                 if_not_exists = TRUE,
                 ...,
                 verbose = TRUE,
                 render_sql = TRUE) {


                sql_statement <-
                draftCreateTable(
                        schema = schema,
                        tableName = tableName,
                        if_not_exists = if_not_exists,
                        ...
                )


                send(
                        conn = conn,
                        sql_statement = sql_statement,
                        verbose = verbose,
                        render_sql = render_sql
                )

        }

#' @title
#' Draft the SQL to Create Table
#'
#' @description
#' Draft a SQL that creates a table using DDL derived from a dataframe. Drafting is stopped if the given tablename or fields in the dataframe are a SQL reserve word.
#'
#' @seealso
#'  \code{\link[forcats]{fct_collapse}}
#' @rdname draftCreateTableFromDF
#' @export
#' @importFrom forcats fct_collapse

draftCreateTableFromDF <-
        function(schema,
                 tableName,
                 data,
                 if_not_exists = TRUE) {

                makeDDL <-
                        function(data) {

                                r_types <- lapply(data, class)
                                r_types <- sapply(r_types, paste, collapse = ", ") # Necessary because there are 2 classes for some data types, such as Sys.time()'s POSIXct, POSIXt

                                field_names <- names(r_types)

                                ddl <- factor(r_types)
                                ddl <-
                                        forcats::fct_collapse(
                                                .f = ddl,
                                                bigint = "integer",
                                                `timestamp without time zone` = "POSIXct, POSIXt",
                                                date = "Date",
                                                float  = c("double", "numeric"),
                                                other_level = "text"
                                        ) %>%
                                        as.character()

                                names(ddl) <- field_names
                                ddl
                        }

                ddl <- makeDDL(data = data)
                fields <- names(ddl)

                if (any(isReserved(tableName, fields))) {

                        stop("Cannot use reserved sql words.")

                }

                ddl <- mapply(paste, fields, ddl, collapse = " ")
                ddl <- paste(ddl, collapse = ",\n")

                if (if_not_exists) {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                        CREATE TABLE IF NOT EXISTS @schema.@tableName (
                                                @ddl
                                        );
                                        ",
                                        schema = schema,
                                        tableName = tableName,
                                        ddl = ddl
                                )

                } else {

                        sql_statement <-
                                SqlRender::render(
                                        "
                                        CREATE TABLE @schema.@tableName (
                                                @ddl
                                        );
                                        ",
                                        schema = schema,
                                        tableName = tableName,
                                        ddl = ddl
                                )


                }

                sql_statement

        }

#' @title
#' Create a Table with a Dataframe
#'
#' @description
#' Derive DDL using the data classes of each field in a dataframe. The map between the R data classes and the Postgresql data types can be found at \code{\link{renderCreateTableFromDF}}. The dataframe can then be appended to the table using \code{\link{appendTable}}. This method is favorable to a direct call to \code{\link{writeTable}} because in some cases, future appends to the table may not adhere to the data definitions created at the time of writing. For example, \code{\link{writeTable}} defaults to `VARCHAR(255)` for all character classes whereas future appends may contain text greater than 255 characters, resulting in error. This function rolls all character classes to `TEXT` data types instead.
#'
#' @rdname createTableFromDF
#' @export

createTableFromDF <-
        function(conn,
                 conn_fun,
                 schema,
                 tableName,
                 if_not_exists = TRUE,
                 data,
                 verbose = TRUE,
                 render_sql = TRUE) {

                sql_statement <- draftCreateTableFromDF(schema = schema,
                                                    tableName = tableName,
                                                    data = data,
                                                    if_not_exists = if_not_exists)


                send(
                        conn = conn,
                        sql_statement = sql_statement,
                        verbose = verbose,
                        render_sql = render_sql
                )

        }





#' @title
#' Create a New Database
#'
#' @inheritParams base_args
#' @param dbname Database name.
#' @param ... Additional arguments passed to `DatabaseConnector::dbSendStatement()`.
#' @export

createDB <-
    function(conn,
             dbname,
             ...) {


            sql_statement <- sprintf("CREATE DATABASE %s;", dbname)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

    }






#' Rename a table in a Postgres schema
#' @description This function will rename a table in a schema, but not move it out of a schema.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

renameDB <-
    function(conn,
             db,
             newDB,
             ...) {


            sql_statement <- renderRenameDB(schema = schema,
                                               db = db,
                                               newDB = newDB)

            send(conn = conn,
                   sql_statement = sql_statement,
                   ...)

    }











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
#' @description Construct the SELECT {fields} FROM {schema}.{table} base query. The SQL Statement is not terminated for the addition of other parameters generated using "construct" functions.
#' @param distinct if TRUE, the DISTINCT fields will be selected for.
#' @import SqlRender
#' @export

constructBase <-
        function(fields = "*",
                 distinct = FALSE,
                 schema = "public",
                 tableName) {

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
#' @export


sourceFilePath <-
    function(instSubdir,
             FileName,
             package) {

        .Deprecated()
        paste0(system.file(package = package), "/", instSubdir, "/", FileName)
    }

#' Terminate a SQL Statement with a semicolon
#' @export

terminateBuild <-
    function(sql_statement) {

        paste0(sql_statement, ";")

    }






#' Execute SQL
#' @description This function differs from the send() and query() functions in that it provides additional features such as a progress bar and time estimations.
#' @export

execute <-
        function(conn,
                 sql_statement,
                 profile = FALSE,
                 progressBar = TRUE,
                 reportOverallTime = TRUE,
                 ...) {

                .Deprecated(new = "queries")
                DatabaseConnector::executeSql(connection = conn,
                                              sql = sql_statement,
                                              profile = profile,
                                              progressBar = progressBar,
                                              reportOverallTime = reportOverallTime,
                                              ...)
        }










#' Query local Postgess from a file
#' @import DatabaseConnector
#' @export

localFileQuery <-
    function (file,
              dbname = "athena",
              port = 5432) {

        .Deprecated()
        conn <- localConnect(dbname = dbname,
                             port = port)

        data <- fileQuery(conn = conn,
                          file = file)

        dc(conn = conn,
           remove = FALSE)

        return(data)
    }





#' Send Statement to a local Postgess from a file
#' @import DatabaseConnector
#' @export

localFileSend <-
    function (file,
              dbname = "athena",
              port = 5432) {

        .Deprecated()

        conn <- localConnect(dbname = dbname,
                             port = port)

        data <- fileSend(conn = conn,
                          file = file)

        dc(conn = conn,
           remove = FALSE)

        return(data)
    }






#' Query local Postgess
#' @import DatabaseConnector
#' @export

localQuery <-
    function (sql_statement,
              dbname = "athena",
              port = 5432) {


        .Deprecated()

            conn <- localConnect(dbname = dbname,
                                 port = port)

            data <- query(conn = conn,
                          sql_statement = sql_statement)

            dc(conn = conn,
               remove = FALSE)

            return(data)
    }





#' Send a SQL Statement to a Local Postgres
#' @export


localSend <-
    function(sql_statement,
             dbname = "athena",
             port = 5432) {

                .Deprecated()

                conn <- localConnect(dbname = dbname,
                                    port = port)

                send(conn = conn,
                     sql_statement = sql_statement)

                dc(conn = conn,
                   remove = FALSE)
    }















#' Parse SQL into Single Statements
#' @description
#' This function is helpful in splitting a SQL file into digestible smaller statements based on the semicolon for troubleshooting or to skip of the statements that do not work within the full input SQL.
#' @import centipede
#' @return
#' List of sql statements
#' @export

parseSQL <-
        function(sql_statement) {

                .Deprecated(new = "execute_n")
                centipede::strsplit(sql_statement, split = "[\\;]{1}", type = "after") %>%
                        unlist() %>%
                        trimws() %>%
                        centipede::no_blank() %>%
                        as.list()
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








#' @title
#' Get Connection Database
#'
#' @description
#' Get the name of the database that the connection object is connected to.
#'
#' @export
#' @rdname getConnDB

getConnDB <-
        function(conn) {
                conn@jConnection$getCatalog()

        }





#' @title
#' List Fields
#'
#' @description
#' Fields for the given table are returned in lowercase.
#'
#' @importFrom DatabaseConnector dbListFields
#' @export
#'
#' @rdname lsFields
#' @family list functions


lsFields <-
    function(conn,
             schema,
             tableName,
             verbose = TRUE,
             render_sql = TRUE) {

            if (render_sql) {

                typewrite_sql("N/A")

            }


            if (verbose) {

                typewrite_activity("Listing Fields...")

            }

            resultset <- DatabaseConnector::dbListFields(conn = conn,
                                                        name = tableName,
                                                        schema = schema)

            if (verbose) {

                typewrite_activity("Listing Fields...complete")

            }

            tolower(resultset)

    }


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

        Fields <- lsFields(conn = conn,
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
#' List Schemas
#'
#' @description
#' List all the schemas in a database in lowercase.
#'
#' @export
#' @family list functions

lsSchema <-
        function(conn,
                 verbose = TRUE,
                 render_sql = TRUE) {

                query(conn = conn,
                      sql_statement = "SELECT nspname FROM pg_catalog.pg_namespace;",
                      verbose = verbose,
                      render_sql = render_sql) %>%
                unlist() %>%
                unname() %>%
                tolower()

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
                        lsSchema(conn = conn,
                                 verbose = FALSE,
                                 render_sql = FALSE)



                if (tolower(schema) %in% schemas) {

                    TRUE

                } else {

                    FALSE
                }


    }





#' @title
#' List Tables
#'
#' @inheritParams base_args
#'
#' @importFrom DatabaseConnector dbListTables
#'
#' @rdname lsTables
#'
#' @export
#' @family list functions

lsTables <-
    function(conn,
             schema,
             verbose = TRUE,
             render_sql = TRUE) {


            if (render_sql) {

                typewrite_sql("N/A")

            }


            if (verbose) {

                typewrite_activity("Listing Tables...")

            }


            resultset <- DatabaseConnector::dbListTables(conn = conn,
                                                        schema = schema)

            if (verbose) {

                typewrite_activity("Listing Tables...completed")

            }

            toupper(resultset)


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


        Tables <- lsTables(conn = conn,
                           schema = schema,
                           verbose = FALSE,
                           render_sql = FALSE)

        if (toupper(tableName) %in% Tables) {

            TRUE

        } else {

            FALSE
        }
    }





#' @title
#' Local Postgres Database Function Family
#'
#' @description
#' Connect and disconnect to a local Postgres database that does not require credentialing. These functions automatically set the dbms and server arguments to "postgresql" and "localhost/{dbname}", leaving the database name (`dbname`) and port (`port`) as the only connection details that are modifiable by the user. Note that these functions cannot be used to connect to a local instance that requires a username and password.
#'
#' @section Local ConnectionDetails:
#' This function returns a connectionDetails class object for functions that make the database connection within its caller environment, such as those belonging to the OHDSI Feature Extraction package. A connection can be made by passing the connectionDetails class object to the \code{\link[DatabaseConnector]{connect}}. This process can be simplified into a single function call with \code{\link{localConnect}}, which performs both operations but will not return a connectionDetails object if it is desired as a return value.
#'
#' @section Local Connection:
#' This function combines the process of creating a connectionDetails object and making the connection in a single function call. If a connectionDetails object is required, see \code{\link{makeLocalConnDetails}}.
#'
#' @param dbname        Name of a local Postgres database, Default: 'athena'
#' @param port          The port on the server to connect to, Default: 5432
#'
#' @name local
#' @family local functions
NULL


#' @title
#' Make a local Postgres Connection Details object
#'
#' @inherit local description
#' @inheritSection local Local ConnectionDetails
#' @inheritParams local
#' @return
#' A connectionDetails class object that lists all the details needed to connect to a database.
#' @seealso
#'  \code{\link[DatabaseConnector]{createConnectionDetails}}
#' @rdname makeLocalConnDetails
#' @family local functions
#' @export
#' @importFrom DatabaseConnector createConnectionDetails

makeLocalConnDetails <-
    function(dbname = "athena",
             port = 5432) {

                dbms     <- "postgresql"
                server   <- paste0("localhost/", dbname)

                DatabaseConnector::createConnectionDetails(dbms  = dbms,
                                                           server   = server,
                                                           port     = port)
    }


#' @title
#' Connect to a Local Postgres Database
#'
#' @inherit local description
#' @inheritSection local Local Connection
#' @inheritParams local
#' @return
#' A connection class object to the database.
#' @seealso
#'  \code{\link[DatabaseConnector]{connect}}
#' @rdname localConnect
#' @family local functions
#' @export
#' @importFrom DatabaseConnector connect

localConnect <-
        function(dbname = "athena",
                 port = 5432) {

                conn_details <- makeLocalConnDetails(dbname = dbname,
                                                     port = port)
                DatabaseConnector::connect(conn_details)
        }





#' Send Query to any Postgres connection
#' @param conn Connection object
#' @param file path to sql file
#' @param sql_statement SQL Query
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbGetQuery function
#' @import DatabaseConnector
#' @import SqlRender
#' @export

fileQuery <-
    function(conn,
             file,
             ...) {

        sql_statement <- SqlRender::readSql(sourceFile = file)

        query(conn = conn,
              sql_statement = sql_statement,
              ...)

    }





#' Send Query to any Postgres connection
#' @param conn Connection object
#' @param file path to sql file
#' @param sql_statement SQL Query
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbGetQuery function
#' @import DatabaseConnector
#' @import SqlRender
#' @export

fileSend <-
    function(conn,
             file,
             ...) {

        sql_statement <- SqlRender::readSql(sourceFile = file)

        send(conn = conn,
              sql_statement = sql_statement,
              ...)

    }










#' Query using a link to SQL file
#' @import DatabaseConnector
#' @import SqlRender
#' @export

linkQuery <-
    function(conn,
             link) {

                tmp_sql <- tempfile(fileext = ".txt")

                download.file(link,
                              destfile = tmp_sql)



                query(conn = conn,
                      statement = SqlRender::readSql(tmp_sql))


                unlink(tmp_sql)

    }





#' Send a statument using a link to SQL file
#' @import DatabaseConnector
#' @import SqlRender
#' @export

linkSend <-
    function(conn,
             link) {

                tmp_sql <- tempfile(fileext = ".txt")

                download.file(link,
                              destfile = tmp_sql)



                send(conn = conn,
                      statement = SqlRender::readSql(tmp_sql))


                unlink(tmp_sql)

    }















#' Render SQL to copy a file to a table
#' @import SqlRender
#' @export

renderCopy <-
    function(schema,
             tableName,
             csvFilePath) {


        SqlRender::render("COPY @schema.@tableName FROM '@csvFilePath' WITH DELIMITER E'\\t' CSV HEADER QUOTE E'\\b';",
                          schema = schema,
                          tableName = tableName,
                          csvFilePath = csvFilePath)

    }





#' Render SQL to Create Database
#' @import SqlRender
#' @export

renderCreateDB <-
    function(schema,
             db,
             newDB) {


        SqlRender::render("
                          CREATE DATABASE @newDB;
                          ",
                          newDB = newDB)

    }





#' Render SQL to Create a Schema
#' @description
#' Renders a SQL statement as a string that creates a schema.
#' @import SqlRender
#' @export

renderCreateSchema <-
    function(schema) {


        SqlRender::render("
                          CREATE SCHEMA @schema;
                          ",
                          schema = schema)

    }





#' Render SQL to Drop a Schema
#' @description Drop a schema if it exists
#' @param cascade If TRUE, a DROP SCHEMA CASCADE is performed.
#' @import SqlRender
#' @export

renderDropSchema <-
    function(schema,
             cascade = FALSE,
             if_exists = TRUE) {


        if (cascade) {

            SqlRender::render("
                              DROP SCHEMA @schema CASCADE
                              ;",
                              schema = schema)

        }

        if (if_exists) {

                SqlRender::render("
                                  DROP SCHEMA IF EXISTS @schema
                                  ;",
                                  schema = schema)

        } else {

                SqlRender::render("
                                  DROP SCHEMA @schema
                                  ;",
                                  schema = schema)
        }

    }






#' Render SQL to Drop a Table
#' @description Drop a table if it exists
#' @import SqlRender
#' @export

renderDropTable <-
    function(schema,
             tableName,
             if_exists = TRUE) {



        if (if_exists) {

            SqlRender::render("
                              DROP TABLE IF EXISTS @schema.@tableName;
                              ",
                              schema = schema,
                              tableName = tableName)

        } else {

            SqlRender::render("
                          DROP TABLE @schema.@tableName;
                          ",
                              schema = schema,
                              tableName = tableName)
        }

    }





#' @title
#' Render GRANT ALL PRIVILEGES
#' @description
#' Render SQL to grant all privileges to a schema a user or a group
#' @param schema schema to grant privileges to
#' @param group group name, Default: NULL
#' @param user user name, Default: NULL
#' @seealso
#'  \code{\link[SqlRender]{render}},\code{\link[SqlRender]{readSql}}
#' @rdname renderGrantSchema
#' @export
#' @importFrom SqlRender render readSql

renderGrantSchema <-
        function(schema,
                 group = NULL,
                 user = NULL) {

                if (is.null(group) && is.null(user)) {
                        stop("group or user is required")
                }


                if (!is.null(group)) {

                        SqlRender::render("
                                          GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA @schema to group @gp
                                          ",
                                          schema = schema,
                                          gp = group)

                } else {

                        SqlRender::render("GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA @schema to @user;",
                                          schema = schema,
                                          user = user)

                }

        }





#' @title
#' Render SQL that returns Column Information
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname renderInfoSchemaCols
#' @export
#' @importFrom SqlRender render



renderInfoSchemaCols <-
        function(schema) {

                SqlRender::render(
                                  "
                                  SELECT *
                                  FROM information_schema.columns
                                  WHERE table_schema = '@schema'
                                  ;
                                  ",
                                  schema = schema)
        }





#' Render SQL to List All Schema
#' @description
#' Renders a SQL statement that will list all schema in a database.
#' @import SqlRender
#' @export

renderLsSchema <-
    function() {


        SqlRender::render("
                          SELECT nspname
                          FROM pg_catalog.pg_namespace
                          ;
                          ")

    }





#' Render SQL to Rename a Table
#' @description This will rename a table within a schema, but not move the table out of a schema.
#' @import SqlRender
#' @export

renderRenameDB <-
    function(schema,
             db,
             newDB) {


        SqlRender::render("ALTER DATABASE @db RENAME TO @newDB;",
                          schema = schema,
                          db = db,
                          newDB = newDB)

    }





#' Render SQL to Rename a Table
#' @description This will rename a table within a schema, but not move the table out of a schema.
#' @import SqlRender
#' @export

renderRenameTable <-
    function(schema,
             tableName,
             newTableName) {


        SqlRender::render("ALTER TABLE @schema.@tableName RENAME TO @newTableName;",
                          schema = schema,
                          tableName = tableName,
                          newTableName = newTableName)

    }





#' Render SQL for a Table Row Count
#' @import SqlRender
#' @param fields Fields selected for. Defaults to "*".
#' @param distinct If TRUE, the distinct row count will be returned.
#' @param schema If NULL, defaults to "public"
#' @export

renderRowCount <-
    function(fields = "*",
             distinct = FALSE,
             schema,
             tableName) {


        if (distinct) {

                SqlRender::render(
                                    "
                                    SELECT DISTINCT COUNT(@fields)
                                    FROM @schema.@tableName
                                    ;
                                    ",
                    schema = schema,
                    fields = fields,
                    tableName = tableName)

        } else {

                SqlRender::render(
                                "
                                SELECT COUNT(@fields)
                                FROM @schema.@tableName
                                ;
                                ",
                                  schema = schema,
                                  fields = fields,
                                  tableName = tableName)

        }

    }











.writeErrorReport <-
        function(message,
                 sql,
                 errorFile,
                 halt = FALSE) {

                .systemInfo <- function() {
                        si <- sessionInfo()
                        lines <- c()
                        lines <- c(lines, "R version:")
                        lines <- c(lines, si$R.version$version.string)
                        lines <- c(lines, "")
                        lines <- c(lines, "Platform:")
                        lines <- c(lines, si$R.version$platform)
                        lines <- c(lines, "")
                        lines <- c(lines, "Attached base packages:")
                        lines <- c(lines, paste("-", si$basePkgs))
                        lines <- c(lines, "")
                        lines <- c(lines, "Other attached packages:")
                        for (pkg in si$otherPkgs) lines <- c(lines,
                                                             paste("- ", pkg$Package, " (", pkg$Version, ")", sep = ""))
                        return(paste(lines, collapse = "\n"))
                }

                report <- c("Timestamp:\n", as.character(Sys.time()), "\n\nError:\n", message, "\n\nSQL:\n", sql, "\n\n", .systemInfo())

                readr::write_lines(report,
                                   path = errorFile,
                                   append = TRUE)

                if (halt) {
                        stop(paste("Error executing SQL:",
                                   message,
                                   paste("\nError is documented in ", errorFile),
                                   sep = "\n"), call. = FALSE)
                } else {
                        warning(paste("\nError executing SQL:",
                                   message,
                                   paste("Error is documented in ", errorFile),
                                   sep = "\n"), call. = FALSE)
                }
        }





#' @export

reservedWords <-
        function() {
                c("ADD", "ALL", "ALTER", "AND", "ANY", "AS", "ASC", "AUTHORIZATION", "BACKUP", "BEGIN", "BETWEEN", "BREAK", "BROWSE", "BULK", "BY", "CASCADE", "CASE", "CHECK", "CHECKPOINT", "CLOSE", "CLUSTERED", "COALESCE", "COLLATE", "COLUMN", "COMMIT", "COMPUTE", "CONSTRAINT", "CONTAINS", "CONTAINSTABLE", "CONTINUE", "CONVERT", "CREATE", "CROSS", "CURRENT", "CURRENT_DATE", "CURRENT_TIME", "CURRENT_TIMESTAMP", "CURRENT_USER", "CURSOR", "DATABASE", "DBCC", "DEALLOCATE", "DECLARE", "DEFAULT", "DELETE", "DENY", "DESC", "DISK", "DISTINCT", "DISTRIBUTED", "DOUBLE", "DROP", "DUMP", "ELSE", "END", "ERRLVL", "ESCAPE", "EXCEPT", "EXEC", "EXECUTE", "EXISTS", "EXIT", "ABSOLUTE", "ACTION", "ADA", "ALLOCATE", "ARE", "ASSERTION", "AT", "AVG", "BIT", "BIT_LENGTH", "BOTH", "CASCADED", "CAST", "CATALOG", "CHAR", "CHAR_LENGTH", "CHARACTER", "CHARACTER_LENGTH", "COLLATION", "CONNECT", "CONNECTION", "CONSTRAINTS", "CORRESPONDING", "COUNT", "DATE", "DAY", "DEC", "DECIMAL", "DEFERRABLE", "DEFERRED", "DESCRIBE", "DESCRIPTOR", "DIAGNOSTICS", "DISCONNECT", "DOMAIN", "END-EXEC", "EXCEPTION", "ADMIN", "AFTER", "AGGREGATE", "ALIAS", "ARRAY", "ASENSITIVE", "ASYMMETRIC", "ATOMIC", "BEFORE", "BINARY", "BLOB", "BOOLEAN", "BREADTH", "CALL", "CALLED", "CARDINALITY", "CLASS", "CLOB", "COLLECT", "COMPLETION", "CONDITION", "CONSTRUCTOR", "CORR", "COVAR_POP", "COVAR_SAMP", "CUBE", "CUME_DIST", "CURRENT_CATALOG", "CURRENT_DEFAULT_TRANSFORM_GROUP", "CURRENT_PATH", "CURRENT_ROLE", "CURRENT_SCHEMA", "CURRENT_TRANSFORM_GROUP_FOR_TYPE", "CYCLE", "DATA", "DEPTH", "DEREF", "DESTROY", "DESTRUCTOR", "DETERMINISTIC", "DICTIONARY", "DYNAMIC", "EACH", "ELEMENT", "EQUALS", "EVERY", "FALSE", "FILTER", "FIRST", "FLOAT", "FOUND", "FREE", "FULLTEXTTABLE", "FUSION", "GENERAL", "GET", "GLOBAL", "GO", "GROUPING", "HOLD", "EXTERNAL", "FETCH", "FILE", "FILLFACTOR", "FOR", "FOREIGN", "FREETEXT", "FREETEXTTABLE", "FROM", "FULL", "FUNCTION", "GOTO", "GRANT", "GROUP", "HAVING", "HOLDLOCK", "IDENTITY", "IDENTITY_INSERT", "IDENTITYCOL", "IF", "IN", "INDEX", "INNER", "INSERT", "INTERSECT", "INTO", "IS", "JOIN", "KEY", "KILL", "LEFT", "LIKE", "LINENO", "LOAD", "MERGE", "NATIONAL", "NOCHECK", "NONCLUSTERED", "NOT", "NULL", "NULLIF", "OF", "OFF", "OFFSETS", "ON", "OPEN", "OPENDATASOURCE", "OPENQUERY", "OPENROWSET", "OPENXML", "OPTION", "OR", "ORDER", "OUTER", "OVER", "PERCENT", "PIVOT", "PLAN", "PRECISION", "PRIMARY", "PRINT", "PROC", "EXTRACT", "FORTRAN", "HOUR", "IMMEDIATE", "INCLUDE", "INDICATOR", "INITIALLY", "INPUT", "INSENSITIVE", "INT", "INTEGER", "INTERVAL", "ISOLATION", "LANGUAGE", "LAST", "LEADING", "LEVEL", "LOCAL", "LOWER", "MATCH", "MAX", "MIN", "MINUTE", "MODULE", "MONTH", "NAMES", "NATURAL", "NCHAR", "NEXT", "NO", "NONE", "NUMERIC", "OCTET_LENGTH", "ONLY", "OUTPUT", "HOST", "IGNORE", "INITIALIZE", "INOUT", "INTERSECTION", "ITERATE", "LARGE", "LATERAL", "LESS", "LIKE_REGEX", "LIMIT", "LN", "LOCALTIME", "LOCALTIMESTAMP", "LOCATOR", "MAP", "MEMBER", "METHOD", "MOD", "MODIFIES", "MODIFY", "MULTISET", "NCLOB", "NEW", "NORMALIZE", "OBJECT", "OCCURRENCES_REGEX", "OLD", "OPERATION", "ORDINALITY", "OUT", "OVERLAY", "PAD", "PARAMETER", "PARAMETERS", "PARTIAL", "PARTITION", "PATH", "POSTFIX", "PREFIX", "PREORDER", "PREPARE", "PERCENT_RANK", "PERCENTILE_CONT", "PERCENTILE_DISC", "POSITION_REGEX", "PRESERVE", "PRIOR", "PRIVILEGES", "RANGE", "READS", "REAL", "RECURSIVE", "REF", "REFERENCING", "REGR_AVGX", "REGR_AVGY", "REGR_COUNT", "REGR_INTERCEPT", "REGR_R2", "REGR_SLOPE", "REGR_SXX", "REGR_SXY", "REGR_SYY", "PROCEDURE", "PUBLIC", "RAISERROR", "READ", "READTEXT", "RECONFIGURE", "REFERENCES", "REPLICATION", "RESTORE", "RESTRICT", "RETURN", "REVERT", "REVOKE", "RIGHT", "ROLLBACK", "ROWCOUNT", "ROWGUIDCOL", "RULE", "SAVE", "SCHEMA", "SECURITYAUDIT", "SELECT", "SEMANTICKEYPHRASETABLE", "SEMANTICSIMILARITYDETAILSTABLE", "SEMANTICSIMILARITYTABLE", "SESSION_USER", "SET", "SETUSER", "SHUTDOWN", "SOME", "STATISTICS", "SYSTEM_USER", "TABLE", "TABLESAMPLE", "TEXTSIZE", "THEN", "TO", "TOP", "TRAN", "TRANSACTION", "TRIGGER", "TRUNCATE", "TRY_CONVERT", "TSEQUAL", "UNION", "UNIQUE", "UNPIVOT", "UPDATE", "UPDATETEXT", "USE", "USER", "VALUES", "VARYING", "VIEW", "WAITFOR", "WHEN", "WHERE", "WHILE", "WITH", "WITHIN GROUP", "WRITETEXT", "OVERLAPS", "PASCAL", "POSITION", "RELATIVE", "ROWS", "SCROLL", "SECOND", "SECTION", "SESSION", "SIZE", "SMALLINT", "SPACE", "SQL", "SQLCA", "SQLCODE", "SQLERROR", "SQLSTATE", "SQLWARNING", "SUBSTRING", "SUM", "TEMPORARY", "TIME", "TIMESTAMP", "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TRAILING", "TRANSLATE", "TRANSLATION", "TRIM", "TRUE", "UNKNOWN", "UPPER", "USAGE", "USING", "VALUE", "VARCHAR", "WHENEVER", "WORK", "WRITE", "YEAR", "ZONE", "RELEASE", "RESULT", "RETURNS", "ROLE", "ROLLUP", "ROUTINE", "ROW", "SAVEPOINT", "SCOPE", "SEARCH", "SENSITIVE", "SEQUENCE", "SETS", "SIMILAR", "SPECIFIC", "SPECIFICTYPE", "SQLEXCEPTION", "START", "STATE", "STATEMENT", "STATIC", "STDDEV_POP", "STDDEV_SAMP", "STRUCTURE", "SUBMULTISET", "SUBSTRING_REGEX", "SYMMETRIC", "SYSTEM", "TERMINATE", "THAN", "TRANSLATE_REGEX", "TREAT", "UESCAPE", "UNDER", "UNNEST", "VAR_POP", "VAR_SAMP", "VARIABLE", "WIDTH_BUCKET", "WITHOUT", "WINDOW", "WITHIN", "XMLAGG", "XMLATTRIBUTES", "XMLBINARY", "XMLCAST", "XMLCOMMENT", "XMLCONCAT", "XMLDOCUMENT", "XMLELEMENT", "XMLEXISTS", "XMLFOREST", "XMLITERATE", "XMLNAMESPACES", "XMLPARSE", "XMLPI", "XMLQUERY", "XMLSERIALIZE", "XMLTABLE", "XMLTEXT", "XMLVALIDATE")
        }

#' @title
#' Is a string a reserve word?
#'
#' @export

isReserved <-
        function(...) {

                Args <- list(...)
                Args <- unlist(Args)
                Args <- toupper(Args)

                sapply(Args, function(x) x %in% reservedWords())

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


#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropCascade <-
        function(conn,
                 schema,
                 ...) {


                sql_statement <- SqlRender::render("DROP SCHEMA @schema CASCADE;", schema = schema)

                send(conn = conn,
                     sql_statement = sql_statement,
                     ...)

        }






#' @title
#' Grant All Privileges to a Schema
#' @description
#' Grant all privileges to a schema to either a group or a user.
#' @param conn PARAM_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @param user PARAM_DESCRIPTION, Default: NULL
#' @param group PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @rdname grantSchema
#' @export

grantSchema <-
        function(conn,
                 schema,
                 user = NULL,
                 group = NULL) {

                sql_statement <-
                        renderGrantSchema(schema = schema,
                                          group = group,
                                          user = user)


                send(conn = conn,
                     sql_statement = sql_statement)

        }











dataTypeInfo <-
        function(conn,
                 schema,
                 tableName,
                 render_sql = TRUE) {


                sql_statement <-
                        SqlRender::render(
                                "
                                SELECT
                                        column_name as field,
                                        udt_name as data_type
                                FROM information_schema.columns
                                WHERE table_name = '@tableName'
                                ",
                                tableName = tableName
                        )

                query(
                        conn = conn,
                        sql_statement = sql_statement,
                        render_sql = render_sql)

        }





#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropCascade <-
        function(conn,
                 schema,
                 ...) {


                sql_statement <- SqlRender::render("DROP SCHEMA @schema CASCADE;", schema = schema)

                send(conn = conn,
                     sql_statement = sql_statement,
                     ...)

        }


#' Drop a Postgres schema
#' @description Drop a schema if it exists.
#' @param ... Additional arguments passed to the DatabaseConnector::dbSendStatement function
#' @export

dropIfExists <-
        function(conn,
                 schema,
                 ...) {


                sql_statement <- SqlRender::render("DROP SCHEMA IF EXISTS @schema;", schema = schema)

                send(conn = conn,
                     sql_statement = sql_statement,
                     ...)

        }


#' Create a Schema
#' @export

createSchema <-
        function(conn,
                 schema) {

                send(conn = conn,
                     SqlRender::render(
                             "CREATE SCHEMA @schema;",
                             schema = schema
                     ))
        }







#' @title
#' Summarize a Schema
#'
#' @param conn                  Postgres connection
#' @param schema                Schema to summarize
#' @param resultTableName       Table that the final output will be written to.
#'
#' @return
#' New Table in the same schema
#'
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[tibble]{as_tibble}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}}
#'
#' @rdname summarizeSchema
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom tibble as_tibble_col
#' @importFrom dplyr bind_rows mutate

summarizeSchema <-
        function(conn,
                 schema,
                 resultTableName) {

                        Tables <- lsTables(conn = conn,
                                                      schema = schema)

        output <- list()
        for (i in (length(output)+1):length(Tables)) {

                Table <- Tables[i]

                tableCols <-
                        query(conn = conn,
                                    sql_statement = buildQuery(schema = schema,
                                                                     tableName = Table,
                                                                     n = 1,
                                                                     n_type = "limit")) %>%
                        colnames()

                total_rows <-
                        query(conn = conn,
                                    sql_statement = renderRowCount(
                                            schema = schema,
                                            tableName = Table)) %>%
                        unlist()

                output_j <- list()
                for (j in 1:length(tableCols)) {
                        tableCol <- tableCols[j]

                        output_j[[j]] <-
                                query(conn = conn,
                                            sql_statement = buildQuery(fields = tableCol,
                                                                             distinct = TRUE,
                                                                             schema = schema,
                                                                             tableName = Table)) %>%
                                nrow()

                        names(output_j)[j] <- tableCol

                        Sys.sleep(1)

                }

                output[[i]] <-
                        output_j  %>%
                        purrr::map(tibble::as_tibble_col, "DISTINCT_COUNT") %>%
                        dplyr::bind_rows(.id = "FIELD") %>%
                        dplyr::mutate(total_rows = total_rows)

                names(output)[i] <- Table

        }

        final_output <- dplyr::bind_rows(output,
                                         .id = "TABLE")


        dropTable(conn = conn,
                        schema = schema,
                        tableName = resultTableName)

        writeTable(conn = conn,
                         schema = schema,
                         tableName = resultTableName,
                         final_output)
}

#' Send Function Factor
#' @export

send_ff <-
    function(
        user,
        password,
        port,
        server) {

        function(sql_statement,
                 verbose = TRUE,
                 render_sql = TRUE) {

            conn <- connect(user = user,
                            password = password,
                            port = port,
                            server = server,
                            verbose = verbose)

            on.exit(expr = dc(conn = conn,
                              verbose = verbose))


            send(conn = conn,
                 sql_statement = sql_statement,
                 verbose = verbose,
                 render_sql = render_sql)


        }


    }



#' Send a List of SQL Statements
#' @description
#' Iteratively send a list of SQL Statements such as the object returned by \code{\link{parseSQL}}.
#' @import progress
#' @param conn Connection object
#' @param sqlList list object of queries
#' @param verbose If TRUE, the SQL is printed back before executing
#' @param progressBar If TRUE, a progress bar is returned in the console.
#' @param skipErrors If TRUE, if a SQL execution returns an error, the statement is printed back in red in the console and the iteration will proceed to the next sql statement in line.
#' @param errorFile (optional) path to the error file where any errors are written to if not null.
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbGetQuery function
#' @export

sendList <-
    function(conn,
             sqlList,
             verbose = TRUE,
             progressBar = TRUE,
             skipErrors = TRUE,
             errorFile = NULL,
             ...) {

            if (!is.list(sqlList)) {

                    stop("'sqlList' must be a list")

            }

            if (progressBar) {

                    pb <- progress::progress_bar$new(total = length(sqlList),
                                                     format = "[:bar] :elapsedfull :current/:total (:percent)",
                                                     clear = FALSE)
                    pb$tick(0)
                    Sys.sleep(.2)

            }

            for (i in 1:length(sqlList)) {

                    sql <- sqlList[[i]]

                    if (verbose) {
                            secretary::typewrite("\n", sql)
                    }

                    if (progressBar) {
                            pb$tick()
                            Sys.sleep(.2)
                    }


                    if (skipErrors) {

                            tryCatch(send(conn = conn,
                                        sql_statement = sql,
                                        ...),
                                     error = function(err) {
                                             secretary::typewrite_error("\n", sql)
                                             if (!is.null(errorFile)) {
                                                     .writeErrorReport(message = err$message,
                                                                       sql = sql,
                                                                       errorFile = errorFile)
                                             }
                                     })


                    } else {

                            send(conn = conn,
                                 sql_statement = sql,
                                 ...)
                    }

            }
    }










#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param output.var PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param progressBar PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{reexports}}
#'  \code{\link[progress]{progress_bar}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[secretary]{typewrite_bold}}
#' @rdname summarizeDB
#' @export
#' @importFrom purrr map
#' @importFrom dplyr bind_rows select distinct mutate everything
#' @importFrom progress progress_bar
#' @importFrom tidyr pivot_wider
#' @importFrom secretary typewrite_bold

summarizeDB <-
        function(conn,
                 output.var,
                 verbose = TRUE,
                 progressBar = TRUE) {

                schemas <- lsSchema(conn = conn)
                schemas <-
                        grep("^pg|information_schema", schemas, value = TRUE, invert = TRUE)

                sqlList <-
                        schemas %>%
                                purrr::map(renderInfoSchemaCols)


                schemaInfo <-
                        queryList(conn = conn,
                                  sqlList = sqlList,
                                  verbose = verbose,
                                  progressBar = FALSE)

                schemaInfo <- dplyr::bind_rows(schemaInfo)



                schemaInfo <-
                        schemaInfo %>%
                        dplyr::select(table_schema,
                                      table_name,
                                      column_name) %>%
                        dplyr::distinct()


                if (progressBar) {


                        pb <- progress::progress_bar$new(format = "[:bar] :elapsed :current/:total :percent",
                                                         total = nrow(schemaInfo))


                        pb$tick(0)
                        Sys.sleep(0.2)

                }


                output <- list()
                for (i in 1:nrow(schemaInfo)) {

                        schema <- schemaInfo$table_schema[i]
                        tableName <- schemaInfo$table_name[i]
                        fieldName <- schemaInfo$column_name[i]


                        if (progressBar) {

                                pb$tick()
                                Sys.sleep(0.2)

                        }


                        output[[i]] <-
                                list(Total_Rows =
                                        renderRowCount(schema = schema,
                                                       distinct = TRUE,
                                                       tableName = tableName),
                                     Distinct_Field_Value =
                                         renderRowCount(fields = fieldName,
                                                        distinct = TRUE,
                                                        schema = schema,
                                                        tableName = tableName)) %>%
                                        purrr::map(~query(conn = conn,
                                                          sql_statement = .)) %>%
                                        dplyr::bind_rows(.id = "Variable") %>%
                                        dplyr::mutate(Schema = schema,
                                                         Table = tableName,
                                                         Field = fieldName) %>%
                                        dplyr::select(Schema,
                                                      Table,
                                                      Field,
                                                      Variable,
                                                         dplyr::everything())


                        assign(output.var, output, envir = parent.frame())

                }

                output <-
                        output %>%
                        dplyr::bind_rows() %>%
                        tidyr::pivot_wider(names_from = Variable,
                                           values_from = count)

                assign(output.var, output, envir = parent.frame())

                if (verbose) {
                        secretary::typewrite_bold("Complete.")
                }

        }












#' @title
#' Append a Table
#'
#' @description
#' Like the writeTable function, this function is a wrapper around a DatabaseConnector function rather than one where a SQL statement is rendered using the SqlRender package. This function performs the additional step of converting all inputs to the data.frame class, especially in cases where the input is a tibble.
#'
#' @inheritParams base_args
#' @param           ...     Additional arguments passed to `DatabaseConnector::dbAppendTable()`
#'
#' @rdname appendTable
#' @family table functions
#'
#' @importFrom DatabaseConnector dbAppendTable
#'
#' @export


appendTable <-
        function(conn,
                 conn_fun,
                 schema,
                 tableName,
                 data,
                 verbose = TRUE,
                 render_sql = TRUE,
                 ...) {

                if (!missing(conn_fun)) {
                    conn <- eval(rlang::parse_expr(conn_fun))
                    on.exit(dc(conn = conn,
                               verbose = verbose),
                            add = TRUE,
                            after = TRUE)
                }

                brake_closed_conn(conn = conn)
                flag_no_rows(data = data)

                schemaTableName <- sprintf("%s.%s", schema, tableName)


                if (render_sql) {

                    typewrite_sql(sql_statement = "N/A")

                }

                if (verbose) {

                    typewrite_activity("Appending...")

                }

                DatabaseConnector::dbAppendTable(conn = conn,
                                                 name = schemaTableName,
                                                 value = as.data.frame(data),
                                                 ...)

                if (verbose) {

                    typewrite_activity("Appending...complete")

                }

        }


#' @title
#' Write a Table
#'
#' @description
#' Unlike the dropTable and renameTable functions, this function is a wrapper around the `DatabaseConnector::dbWriteTable()` function rather than one where a SQL statement is rendered using the SqlRender package. This function that converts all inputs to a dataframe, especially in cases where the input is a tibble, in which case an error would be thrown when writing.
#'
#' @inheritParams base_args
#' @param       ...     Additional arguments passed to `DatabaseConnector::dbWriteTable()`
#'
#' @importFrom DatabaseConnector dbWriteTable
#'
#' @rdname writeTable
#'
#' @export


writeTable <-
        function(conn,
                 conn_fun,
                 schema,
                 tableName,
                 data,
                 drop_existing = FALSE,
                 verbose = TRUE,
                 render_sql = TRUE,
                 ...) {

                if (!missing(conn_fun)) {
                    conn <- eval(rlang::parse_expr(conn_fun))
                    on.exit(dc(conn = conn,
                               verbose = verbose),
                            add = TRUE,
                            after = TRUE)
                }

                brake_closed_conn(conn = conn)
                flag_no_rows(data = data)

                if (drop_existing) {

                    dropTable(conn = conn,
                              schema = schema,
                              tableName = tableName,
                              if_exists = TRUE)
                }

                schemaTableName <- sprintf("%s.%s", schema, tableName)


                if (render_sql) {

                    typewrite_sql(sql_statement = "N/A")

                }

                if (verbose) {

                    typewrite_activity("Writing...")

                }

                DatabaseConnector::dbWriteTable(conn = conn,
                                                name = schemaTableName,
                                                value = as.data.frame(data),
                                                ...)

                if (verbose) {

                    typewrite_activity("Writing...complete")

                }

        }

#' @title
#' Drop a Table
#'
#' @inheritParams base_args
#' @param           if_exists   If TRUE, the table will be dropped only if it exists.
#' @param           ...         Additional arguments passed to `DatabaseConnector::dbSendStatement()`
#'
#' @rdname dropTable
#' @export

dropTable <-
    function(conn,
             conn_fun,
             schema,
             tableName,
             if_exists = TRUE,
             verbose = TRUE,
             render_sql = TRUE,
             ...) {

            if (!missing(conn_fun)) {
                    conn <- eval(rlang::parse_expr(conn_fun))
                    on.exit(dc(conn = conn,
                               verbose = verbose),
                            add = TRUE,
                            after = TRUE)
            }

            brake_closed_conn(conn = conn)

            if (if_exists) {

                sql_statement <- sprintf("DROP TABLE IF EXISTS %s.%s;", schema, tableName)

            } else {

                sql_statement <- sprintf("DROP TABLE %s.%s;", schema, tableName)

            }

            if (render_sql) {

                typewrite_sql(sql_statement = sql_statement)

            }

            if (verbose) {

                typewrite_activity("Dropping...")

            }


            send(conn = conn,
                 sql_statement = sql_statement,
                 verbose = FALSE,
                 render_sql = FALSE,
                 ...)


            if (verbose) {

                typewrite_activity("Dropping...complete")

            }

    }

#' @title
#' Write a Table that is Dropped On Exit
#'
#' @description
#' A Staging Table is one that is dropped on exit in the parent frame from which the function is being called.
#'
#' @export
#' @rdname writeStagingTable

writeStagingTable <-
    function(conn,
             conn_fun,
             schema,
             tableName,
             data,
             drop_existing = FALSE,
             verbose = TRUE,
             render_sql = TRUE,
             ...) {

            writeTable(conn = conn,
                       conn_fun = conn_fun,
                       schema = schema,
                       tableName = tableName,
                       data = data,
                       drop_existing = drop_existing,
                       verbose = verbose,
                       render_sql = render_sql,
                       ... = ...)


            do.call(what = on.exit,
                    args = list(substitute(dropTable(conn = conn,
                                         conn_fun = conn_fun,
                                         schema = schema,
                                         tableName = tableName,
                                         if_exists = TRUE,
                                         verbose = verbose,
                                         render_sql = render_sql)),
                                add = TRUE,
                                after = FALSE),
                    envir = parent.frame())



    }

#' @title
#' Read an Entire Table
#'
#' @description
#' Shortcut for a `SELECT *` SQL statement.
#'
#' @inheritParams base_args
#'
#' @export


readTable <-
        function(conn,
                 schema,
                 tableName,
                 verbose = TRUE,
                 render_sql = TRUE) {


            sql_statement <- sprintf("SELECT * FROM %s.%s;", schema, tableName)

            if (render_sql) {

                typewrite_sql(sql_statement = sql_statement)
            }



            if (verbose) {

                typewrite_activity("Reading...")

            }

            resultset <-
            query(conn = conn,
                  sql_statement = sql_statement,
                  verbose = FALSE,
                  render_sql = FALSE)

            if (verbose) {

                typewrite_activity("Reading...complete")

            }

            resultset

        }




#' @title
#' Search a Table for a Value
#'
#' @description
#' Loop a query for a set of one or more values in a table across all the existing fields or optionally, a subset of the fields. Both the values and the table fields are ensured compatibility by 1. Converting each value in the `values` argument to the character class and 2. Casting each table field as varchar in the query.
#'
#' @inheritParams base_args
#' @param case_insensitive  If TRUE, both sides of the query are converted to lowercase.
#' @param values            Vector of length 1 or greater to search for.
#' @param ...               (Optional) Character strings of 1 or more fields in the table to search in.
#'
#' @importFrom rlang list2
#'
#' @rdname searchTable
#' @family table functions
#' @example inst/example/table.R
#'
#' @export


searchTable <-
        function(conn,
                 schema,
                 tableName,
                 ...,
                 values,
                 case_insensitive = TRUE,
                 verbose = TRUE,
                 render_sql = TRUE) {

                brake_closed_conn(conn = conn)

                # Format Values for SQL
                values <- as.character(values)

                if (case_insensitive) {
                        values <- tolower(values)
                }
                values <- sQuo(values)



                # Get Fields vector to loop over for each SQL query
                if (missing(...)) {

                        Fields <- lsFields(conn = conn,
                                           schema = schema,
                                           tableName = tableName,
                                           verbose = verbose,
                                           render_sql = FALSE)

                } else {

                        Fields <- unlist(rlang::list2(...))

                }


                sql_statements <- list()
                for (Field in Fields) {

                    i <- 1+length(sql_statements)

                    if (case_insensitive) {


                                sql_statements[[i]] <-
                                SqlRender::render(
                                                    "
                                                    SELECT *
                                                    FROM @schema.@tableName t
                                                    WHERE LOWER(t.@Field::varchar) IN (@values)
                                                    ;
                                                    ",
                                    schema = schema,
                                    tableName = tableName,
                                    Field = Field,
                                    values = values
                                )

                    } else {

                        sql_statements[[i]] <-
                            SqlRender::render(
                                                "
                                                SELECT *
                                                FROM @schema.@tableName t
                                                WHERE t.@Field::varchar IN (@values)
                                                ;
                                                ",
                                schema = schema,
                                tableName = tableName,
                                Field = Field,
                                values = values
                            )



                    }

                }


                resultsets <- list()
                for (i in seq_along(sql_statements)) {

                        resultsets[[i]] <-
                            suppressWarnings(
                            query(conn = conn,
                                  sql_statement = sql_statements[[i]],
                                  verbose = verbose,
                                  render_sql = render_sql))

                }

                names(resultsets) <- Fields

                metrics <-
                    resultsets %>%
                    purrr::map(~ tibble::as_tibble_col(x = nrow(.), column_name = "Rows")) %>%
                    dplyr::bind_rows(.id = "Field")


                list(ROWS = metrics,
                     RESULTSETS = resultsets %>%
                                        purrr::keep(~ nrow(.) > 0))

        }







#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @export
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



#' Wrap with Single Quotes
#'
#' @name sQuo
#' @rdname sQuo
#' @export
#' @export

sQuo <-
        function(vector) {
                vector <- as.character(vector)
                paste0("'", vector, "'")
        }


#' @title
#' Affix the System Date to a String
#'
#' @description
#' Date is affixed at the end of a string in "YYYY_mm_dd" Format
#'
#' @importFrom stringr str_replace_all
#'
#' @export

affix_date <-
        function(string) {
                paste0(name, "_", stringr::str_replace_all(as.character(Sys.Date()), "[-]{1}", "_"))
        }


#' Typewrite SQL
#' @importFrom secretary typewrite greenTxt
#' @importFrom stringr str_replace_all
#' @export

typewrite_sql <-
        function (sql_statement)
        {
                sql_statement <- stringr::str_replace_all(sql_statement,
                                                          "[\r\n\t]{1,}|\\s{2,}", " ")
                sql_statement <- trimws(sql_statement)
                secretary::typewrite(secretary::greenTxt("SQL:"), sql_statement)
        }

#' Typewrite Activity
#' @importFrom secretary typewrite greenTxt
#' @importFrom stringr str_replace_all
#' @export

typewrite_activity <-
        function (activity)
        {
                secretary::typewrite(secretary::yellowTxt(activity))
        }





#' Get SourceFile Path
#' @description This function provides the path for files installed within a given package's library.
#' @param instSubdir Name of subdirectory in the inst/ folder
#' @param FileName Name of file in subdirectory
#' @param package Package name
#' @export


sourceFilePath <-
        function(instSubdir,
                 FileName,
                 package) {
                paste0(system.file(package = package), "/", instSubdir, "/", FileName)
        }










#' @export

readView <-
        function(conn,
                 schema,
                 viewName,
                 verbose = TRUE,
                 render_sql = TRUE) {

                query(conn = conn,
                      sql_statement = SqlRender::render('SELECT * FROM @schema."@viewName";', schema = schema, viewName = viewName),
                      verbose = verbose,
                      render_sql = render_sql)

        }

#' @export

refreshMatView <-
        function(conn,
                 schema,
                 matViewName,
                 verbose = TRUE,
                 render_sql = TRUE) {


                send(conn = conn,
                     sql_statement = SqlRender::render('REFRESH MATERIALIZED VIEW @schema."@matViewName"', schema = schema, matViewName = matViewName),
                     verbose = verbose,
                     render_sql = render_sql)

        }




#' Build a SQL Query
#' @description A SQL query is built using the given arguments. Currently, only 1 whereIn and whereNot in parameters can be set.
#' @return SQL statement as a character string.
#' @import purrr
#' @import stringr
#' @export

buildQuery <-
    function(fields = "*",
             distinct = FALSE,
             schema,
             tableName,
             whereInField = NULL,
             whereInVector = NULL,
             whereNotInField = NULL,
             whereNotInVector = NULL,
             caseInsensitive = TRUE,
             n = NULL,
             n_type = c("limit", "random")) {

        ######
        # QA to make sure all whereIn and n  arguments have been supplied in pairs
        #####
        whereIns <- list(whereInField, whereInVector) %>%
            purrr::set_names(c("field", "vector")) %>%
            purrr::keep(~!is.null(.))
        whereNotIns <- list(whereNotInField, whereNotInVector) %>%
            purrr::set_names(c("field", "vector")) %>%
            purrr::keep(~!is.null(.))


        list(whereIns, whereNotIns) %>%
            purrr::map2(list("whereIn", "whereNotIn"),
                        function(x,y) if (!(length(x) %in% c(0,2))) {stop('both "', y, '" arguments must be supplied')})

        ######
        # QA to make sure all n arugments have been supplied
        #####

        if (length(n) == 1 & length(n_type) != 1) {

            n_type <- "limit"

            warning('"n_type" set to "limit"')

        }

        #####
        # Start
        #####
        sql_construct  <- constructBase(fields = fields,
                                        distinct = distinct,
                                        schema = schema,
                                        tableName = tableName)


        if (caseInsensitive) {


            # If WhereIn arguments are not null include it in build
            if (length(whereIns) == 2) {

                sql_construct <-
                    paste(sql_construct,
                          constructWhereLowerIn(field = whereIns$field,
                                                vector = tolower(whereIns$vector)),
                          collapse = " ")

                # If WhereNotIn arguments are supplied on top of the WhereIn, add them to the query by modifying the constructWhereNotIn output by replacing the second "WHERE" with "AND"
                if (length(whereNotIns) == 2) {


                    sql_construct <-
                        paste(sql_construct,
                              "AND",
                              constructWhereLowerNotIn(field = whereNotIns$field,
                                                       vector = tolower(whereNotIns$vector)) %>%
                                  stringr::str_remove_all("WHERE") %>%
                                  trimws(),
                              collapse = " ")


                }

            } else {

                # Building a query if only whereNotIn arguments were supplied
                if (length(whereNotIns) == 2) {


                    sql_construct <-
                        paste(sql_construct,
                              constructWhereLowerNotIn(field = whereNotIns$field,
                                                       vector = tolower(whereNotIns$vector)),
                              collapse = " ")


                }



            }

            # If n arguments are not null include it in build, as either a limit or random sample of size n
            if (!is.null(n)) {

                if (n_type == "limit") {

                    sql_construct <-
                        paste(sql_construct,
                              constructLimit(n = n),
                              collapse = " ")

                } else if (n_type == "random") {

                    sql_construct <-
                        paste(sql_construct,
                              constructRandom(n = n),
                              collapse = " ")

                } else {

                    warning('"n_type" not recognized and "n" removed from build')


                }

            }









        } else {



            # If WhereIn arguments are not null include it in build
            if (length(whereIns) == 2) {

                sql_construct <-
                    paste(sql_construct,
                          constructWhereIn(field = whereIns$field,
                                           vector = whereIns$vector),
                          collapse = " ")

                # If WhereNotIn arguments are supplied on top of the WhereIn, add them to the query by modifying the constructWhereNotIn output by replacing the second "WHERE" with "AND"
                if (length(whereNotIns) == 2) {


                    sql_construct <-
                        paste(sql_construct,
                              "AND",
                              constructWhereNotIn(field = whereNotIns$field,
                                                  vector = whereNotIns$vector) %>%
                                  stringr::str_remove_all("WHERE") %>%
                                  trimws(),
                              collapse = " ")


                }

            } else {

                # Building a query if only whereNotIn arguments were supplied
                if (length(whereNotIns) == 2) {


                    sql_construct <-
                        paste(sql_construct,
                              constructWhereNotIn(field = whereNotIns$field,
                                                  vector = whereNotIns$vector),
                              collapse = " ")


                }



            }

            # If n arguments are not null include it in build, as either a limit or random sample of size n
            if (!is.null(n)) {

                if (n_type == "limit") {

                    sql_construct <-
                        paste(sql_construct,
                              constructLimit(n = n),
                              collapse = " ")

                } else if (n_type == "random") {

                    sql_construct <-
                        paste(sql_construct,
                              constructRandom(n = n),
                              collapse = " ")

                } else {

                    warning('"n_type" not recognized and "n" removed from build')


                }

            }

        }

        #Add a semicolon to finish the query
        sql_construct %>%
            stringr::str_replace_all(pattern = "[\n]{2,}",
                                     replacement = "\n") %>%
            terminateBuild()


    }


#' Send Function Factor
#' @export

send_ff <-
    function(
        user,
        password,
        port,
        server) {

        function(sql_statement,
                 verbose = TRUE,
                 render_sql = TRUE) {

            conn <- connect(user = user,
                            password = password,
                            port = port,
                            server = server,
                            verbose = verbose)

            on.exit(expr = dc(conn = conn,
                              verbose = verbose))


            send(conn = conn,
                 sql_statement = sql_statement,
                 verbose = verbose,
                 render_sql = render_sql)


        }


    }



#' Send a List of SQL Statements
#' @description
#' Iteratively send a list of SQL Statements such as the object returned by \code{\link{parseSQL}}.
#' @import progress
#' @param conn Connection object
#' @param sqlList list object of queries
#' @param verbose If TRUE, the SQL is printed back before executing
#' @param progressBar If TRUE, a progress bar is returned in the console.
#' @param skipErrors If TRUE, if a SQL execution returns an error, the statement is printed back in red in the console and the iteration will proceed to the next sql statement in line.
#' @param errorFile (optional) path to the error file where any errors are written to if not null.
#' @param ... Additional arguments to pass onto the DatabaseConnector::dbGetQuery function
#' @export

send_list <-
    function(conn,
             sqlList,
             verbose = TRUE,
             progressBar = TRUE,
             skipErrors = TRUE,
             errorFile = NULL,
             ...) {

        if (!is.list(sqlList)) {

            stop("'sqlList' must be a list")

        }

        if (progressBar) {

            pb <- progress::progress_bar$new(total = length(sql_list),
                                             format = "[:bar] :elapsedfull :current/:total (:percent)",
                                             clear = FALSE)
            pb$tick(0)
            Sys.sleep(.2)

        }

        for (i in 1:length(sqlList)) {

            sql <- sql_list[[i]]

            if (verbose) {
                secretary::typewrite("\n", sql)
            }

            if (progressBar) {
                pb$tick()
                Sys.sleep(.2)
            }


            if (skipErrors) {

                tryCatch(send(conn = conn,
                              sql_statement = sql,
                              ...),
                         error = function(err) {
                             secretary::typewrite_error("\n", sql)
                             if (!is.null(errorFile)) {
                                 .writeErrorReport(message = err$message,
                                                   sql = sql,
                                                   errorFile = errorFile)
                             }
                         })


            } else {

                send(conn = conn,
                     sql_statement = sql,
                     ...)
            }

        }
    }

#' Terminate a SQL Statement with a semicolon
#' @export

terminate_build <-
    function(sql_statement) {

        paste0(sql_statement, ";")

    }

#' Cache a Resultset from a Join
#' @description This function will cache the resultset from a SQL query created by buildJoinQuery to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param ... Arguments passed to the buildJoinQuery function
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

cache_join <-
    function(.data,
             ...,
             db,
             schema) {

        R.cache::saveCache(object = .data,
                           key = list(...),
                           dirs = paste0(db, "/", schema))
    }





#' Cache a Query Resultset
#' @description This function will cache the resultset from a SQL query to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

cache_query <-
    function(.data,
             sqlQuery,
             db) {

        R.cache::saveCache(object = .data,
                           key = list(sqlQuery),
                           dirs = db)
    }





#' Clear a Cache
#' @description This function will clear a cache subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param .data dataframe to cache
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

clear_cache <-
    function(db) {

        R.cache::clearCache(path = getCachePath(db))


    }





#' Load a Cached Join Resultset
#' @description This function will load the cached resultset from a SQL query created by the buildJoinQuery function to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param ... Arguments passed to the buildJoinQuery function
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

load_cached_join <-
    function(...,
             db,
             schema) {

        R.cache::loadCache(key = list(...),
                           dirs = paste0(db, "/", schema))
    }





#' Load a Cached Query Resultset
#' @description This function will load the cached resultset from a SQL query to a subdirectory of the cache root directory, the path of which will be {db/schema}.
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param db name of database
#' @param schema name of schema
#' @import R.cache
#' @export

load_cached_query <-
    function(sqlQuery,
             db) {

        R.cache::loadCache(key = list(sqlQuery),
                           dirs = db)
    }





