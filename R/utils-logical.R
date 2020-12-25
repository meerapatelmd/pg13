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
                 table,
                 field) {

                fields <- ls_fields(conn = conn,
                                    schema = schema,
                                    table = table,
                                    verbose = FALSE,
                                    render_sql = FALSE)

                if (tolower(field) %in% fields) {

                        TRUE

                } else {

                        FALSE
                }
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
                        ls_schema(conn = conn,
                                  verbose = FALSE,
                                  render_sql = FALSE)



                if (tolower(schema) %in% schemas) {

                        TRUE

                } else {

                        FALSE
                }


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
                 table_name) {


                tables <- ls_tables(conn = conn,
                                    schema = schema,
                                    verbose = FALSE,
                                    render_sql = FALSE)

                if (toupper(table_name) %in% tables) {

                        TRUE

                } else {

                        FALSE
                }
        }


#' @title
#' Does a database exist?
#'
#' @inheritParams base_args
#'
#' @rdname db_exists
#'
#' @export
#' @family logical functions

db_exists <-
        function(conn,
                 db_name) {


                dbs <- ls_db(conn = conn,
                                    verbose = FALSE,
                                    render_sql = FALSE)

                if (tolower(db_name) %in% dbs) {

                        TRUE

                } else {

                        FALSE
                }
        }





#' @export

reserved_words <-
        function() {
                c("ADD", "ALL", "ALTER", "AND", "ANY", "AS", "ASC", "AUTHORIZATION", "BACKUP", "BEGIN", "BETWEEN", "BREAK", "BROWSE", "BULK", "BY", "CASCADE", "CASE", "CHECK", "CHECKPOINT", "CLOSE", "CLUSTERED", "COALESCE", "COLLATE", "COLUMN", "COMMIT", "COMPUTE", "CONSTRAINT", "CONTAINS", "CONTAINSTABLE", "CONTINUE", "CONVERT", "CREATE", "CROSS", "CURRENT", "CURRENT_DATE", "CURRENT_TIME", "CURRENT_TIMESTAMP", "CURRENT_USER", "CURSOR", "DATABASE", "DBCC", "DEALLOCATE", "DECLARE", "DEFAULT", "DELETE", "DENY", "DESC", "DISK", "DISTINCT", "DISTRIBUTED", "DOUBLE", "DROP", "DUMP", "ELSE", "END", "ERRLVL", "ESCAPE", "EXCEPT", "EXEC", "EXECUTE", "EXISTS", "EXIT", "ABSOLUTE", "ACTION", "ADA", "ALLOCATE", "ARE", "ASSERTION", "AT", "AVG", "BIT", "BIT_LENGTH", "BOTH", "CASCADED", "CAST", "CATALOG", "CHAR", "CHAR_LENGTH", "CHARACTER", "CHARACTER_LENGTH", "COLLATION", "CONNECT", "CONNECTION", "CONSTRAINTS", "CORRESPONDING", "COUNT", "DATE", "DAY", "DEC", "DECIMAL", "DEFERRABLE", "DEFERRED", "DESCRIBE", "DESCRIPTOR", "DIAGNOSTICS", "DISCONNECT", "DOMAIN", "END-EXEC", "EXCEPTION", "ADMIN", "AFTER", "AGGREGATE", "ALIAS", "ARRAY", "ASENSITIVE", "ASYMMETRIC", "ATOMIC", "BEFORE", "BINARY", "BLOB", "BOOLEAN", "BREADTH", "CALL", "CALLED", "CARDINALITY", "CLASS", "CLOB", "COLLECT", "COMPLETION", "CONDITION", "CONSTRUCTOR", "CORR", "COVAR_POP", "COVAR_SAMP", "CUBE", "CUME_DIST", "CURRENT_CATALOG", "CURRENT_DEFAULT_TRANSFORM_GROUP", "CURRENT_PATH", "CURRENT_ROLE", "CURRENT_SCHEMA", "CURRENT_TRANSFORM_GROUP_FOR_TYPE", "CYCLE", "DATA", "DEPTH", "DEREF", "DESTROY", "DESTRUCTOR", "DETERMINISTIC", "DICTIONARY", "DYNAMIC", "EACH", "ELEMENT", "EQUALS", "EVERY", "FALSE", "FILTER", "FIRST", "FLOAT", "FOUND", "FREE", "FULLTEXTTABLE", "FUSION", "GENERAL", "GET", "GLOBAL", "GO", "GROUPING", "HOLD", "EXTERNAL", "FETCH", "FILE", "FILLFACTOR", "FOR", "FOREIGN", "FREETEXT", "FREETEXTTABLE", "FROM", "FULL", "FUNCTION", "GOTO", "GRANT", "GROUP", "HAVING", "HOLDLOCK", "IDENTITY", "IDENTITY_INSERT", "IDENTITYCOL", "IF", "IN", "INDEX", "INNER", "INSERT", "INTERSECT", "INTO", "IS", "JOIN", "KEY", "KILL", "LEFT", "LIKE", "LINENO", "LOAD", "MERGE", "NATIONAL", "NOCHECK", "NONCLUSTERED", "NOT", "NULL", "NULLIF", "OF", "OFF", "OFFSETS", "ON", "OPEN", "OPENDATASOURCE", "OPENQUERY", "OPENROWSET", "OPENXML", "OPTION", "OR", "ORDER", "OUTER", "OVER", "PERCENT", "PIVOT", "PLAN", "PRECISION", "PRIMARY", "PRINT", "PROC", "EXTRACT", "FORTRAN", "HOUR", "IMMEDIATE", "INCLUDE", "INDICATOR", "INITIALLY", "INPUT", "INSENSITIVE", "INT", "INTEGER", "INTERVAL", "ISOLATION", "LANGUAGE", "LAST", "LEADING", "LEVEL", "LOCAL", "LOWER", "MATCH", "MAX", "MIN", "MINUTE", "MODULE", "MONTH", "NAMES", "NATURAL", "NCHAR", "NEXT", "NO", "NONE", "NUMERIC", "OCTET_LENGTH", "ONLY", "OUTPUT", "HOST", "IGNORE", "INITIALIZE", "INOUT", "INTERSECTION", "ITERATE", "LARGE", "LATERAL", "LESS", "LIKE_REGEX", "LIMIT", "LN", "LOCALTIME", "LOCALTIMESTAMP", "LOCATOR", "MAP", "MEMBER", "METHOD", "MOD", "MODIFIES", "MODIFY", "MULTISET", "NCLOB", "NEW", "NORMALIZE", "OBJECT", "OCCURRENCES_REGEX", "OLD", "OPERATION", "ORDINALITY", "OUT", "OVERLAY", "PAD", "PARAMETER", "PARAMETERS", "PARTIAL", "PARTITION", "PATH", "POSTFIX", "PREFIX", "PREORDER", "PREPARE", "PERCENT_RANK", "PERCENTILE_CONT", "PERCENTILE_DISC", "POSITION_REGEX", "PRESERVE", "PRIOR", "PRIVILEGES", "RANGE", "READS", "REAL", "RECURSIVE", "REF", "REFERENCING", "REGR_AVGX", "REGR_AVGY", "REGR_COUNT", "REGR_INTERCEPT", "REGR_R2", "REGR_SLOPE", "REGR_SXX", "REGR_SXY", "REGR_SYY", "PROCEDURE", "PUBLIC", "RAISERROR", "READ", "READTEXT", "RECONFIGURE", "REFERENCES", "REPLICATION", "RESTORE", "RESTRICT", "RETURN", "REVERT", "REVOKE", "RIGHT", "ROLLBACK", "ROWCOUNT", "ROWGUIDCOL", "RULE", "SAVE", "SCHEMA", "SECURITYAUDIT", "SELECT", "SEMANTICKEYPHRASETABLE", "SEMANTICSIMILARITYDETAILSTABLE", "SEMANTICSIMILARITYTABLE", "SESSION_USER", "SET", "SETUSER", "SHUTDOWN", "SOME", "STATISTICS", "SYSTEM_USER", "TABLE", "TABLESAMPLE", "TEXTSIZE", "THEN", "TO", "TOP", "TRAN", "TRANSACTION", "TRIGGER", "TRUNCATE", "TRY_CONVERT", "TSEQUAL", "UNION", "UNIQUE", "UNPIVOT", "UPDATE", "UPDATETEXT", "USE", "USER", "VALUES", "VARYING", "VIEW", "WAITFOR", "WHEN", "WHERE", "WHILE", "WITH", "WITHIN GROUP", "WRITETEXT", "OVERLAPS", "PASCAL", "POSITION", "RELATIVE", "ROWS", "SCROLL", "SECOND", "SECTION", "SESSION", "SIZE", "SMALLINT", "SPACE", "SQL", "SQLCA", "SQLCODE", "SQLERROR", "SQLSTATE", "SQLWARNING", "SUBSTRING", "SUM", "TEMPORARY", "TIME", "TIMESTAMP", "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TRAILING", "TRANSLATE", "TRANSLATION", "TRIM", "TRUE", "UNKNOWN", "UPPER", "USAGE", "USING", "VALUE", "VARCHAR", "WHENEVER", "WORK", "WRITE", "YEAR", "ZONE", "RELEASE", "RESULT", "RETURNS", "ROLE", "ROLLUP", "ROUTINE", "ROW", "SAVEPOINT", "SCOPE", "SEARCH", "SENSITIVE", "SEQUENCE", "SETS", "SIMILAR", "SPECIFIC", "SPECIFICTYPE", "SQLEXCEPTION", "START", "STATE", "STATEMENT", "STATIC", "STDDEV_POP", "STDDEV_SAMP", "STRUCTURE", "SUBMULTISET", "SUBSTRING_REGEX", "SYMMETRIC", "SYSTEM", "TERMINATE", "THAN", "TRANSLATE_REGEX", "TREAT", "UESCAPE", "UNDER", "UNNEST", "VAR_POP", "VAR_SAMP", "VARIABLE", "WIDTH_BUCKET", "WITHOUT", "WINDOW", "WITHIN", "XMLAGG", "XMLATTRIBUTES", "XMLBINARY", "XMLCAST", "XMLCOMMENT", "XMLCONCAT", "XMLDOCUMENT", "XMLELEMENT", "XMLEXISTS", "XMLFOREST", "XMLITERATE", "XMLNAMESPACES", "XMLPARSE", "XMLPI", "XMLQUERY", "XMLSERIALIZE", "XMLTABLE", "XMLTEXT", "XMLVALIDATE")
        }

#' @title
#' Is a string a reserve word?
#'
#' @export

is_reserved <-
        function(...) {

                args <- list(...)
                args <- unlist(args)
                args <- toupper(args)

                sapply(Args, function(x) x %in% reservedWords())

        }





