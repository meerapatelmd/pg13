#' Render SQL to copy a file to a table
#' @import SqlRender
#' @export

renderCopy <-
    function(schema,
             tableName,
             csvFilePath) {


        SqlRender::render("
                          COPY @schema.@tableName FROM '@csvFilePath' WITH DELIMITER E'\\t' CSV HEADER QUOTE E'\\b';
                          ",
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





