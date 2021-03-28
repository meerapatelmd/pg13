#' @title
#' Arguments
#'
#' @param data  A dataframe or tibble.
#' @param sql_statement A SQL Statement.
#' @param sql_statements A list or vector of SQL Statements of length 1 or greater.
#' @param schema The target schema for the operation.
#' @param tableName The target table in the `schema` for an operation or for writing functions, the name of the table that will be created.
#' @param verbose If TRUE, details on the activity are returned in the console, such as when the querying starting and was completed.
#' @param render_sql If TRUE, the SQL statement for the operation is returned in the console.
#' @param predicate             Evaluating function.
#' @param stop_message          Message to receive with the `predicate` evaluates to true.
#' @param arguments             (option) character vector of arguments for the new function. If missing, the new function will not have any arguments. Arguments in the `predicate` should also be considered. For example, if the predicate is "is.logical(x)", the `arguments` value should be "x".
#' @param  conn    Connection object
#' @param .data A dataframe or tibble.
#' @param db name of database
#' @param sqlQuery sql statement of length 1 that will be the sole key for cache retrieval.
#' @param object  R object to cache.
#' @param dirs Argument passed to \code{\link[R.cache]{saveCache}}, \code{\link[R.cache]{findCache}}, and/or \code{\link[R.cache]{loadCache}}
#' @param remove        If TRUE, the Connection object argument is removed from the parent environment.
#' @param distinct if TRUE, the DISTINCT fields will be selected for.
#' @param vector vector of values that the SQL query is filtering for
#' @param field Single field to be filtered for
#' @param term Character string that the field parameter is searched for.
#' @param warn_message          Message to receive with the `predicate` evaluates to true.
#' @param dbname        Name of a local Postgres database, Default: 'athena'
#' @param port          The port on the server to connect to, Default: 5432
#' @param datetime_field PARAM_DESCRIPTION
#' @param log_table PARAM_DESCRIPTION
#' @param conn Connection object
#' @param file path to sql file
#' @param warn_no_rows If TRUE, a warning is given that query has returned 0 rows.
#' @param cascade If TRUE, a DROP SCHEMA CASCADE is performed.
#' @param group group name, Default: NULL
#' @param user user name, Default: NULL
#' @param fields Fields selected for. Defaults to "*".
#' @param distinct If TRUE, the distinct row count will be returned.
#' @param resultTableName       Table that the final output will be written to.
#' @param sqlList list object of queries
#' @param progressBar If TRUE, a progress bar is returned in the console.
#' @param skipErrors If TRUE, if a SQL execution returns an error, the statement is printed back in red in the console and the iteration will proceed to the next sql statement in line.
#' @param errorFile (optional) path to the error file where any errors are written to if not null.
#' @param if_exists   If TRUE, the table will be dropped only if it exists.
#' @param case_insensitive  If TRUE, both sides of the query are converted to lowercase.
#' @param values            Vector of length 1 or greater to search for.
#' @param instSubdir Name of subdirectory in the inst/ folder
#' @param FileName Name of file in subdirectory
#' @param package Package name
#' @param table Target table for the operation.
#' @param select_table_fields The fields to select for in the first table
#' @param select_join_on_fields The fields to select for in the table being join to the first table
#' @param column Column to join on.
#' @param join_on_schema Schema of the table that is being joined to the first table.
#' @param join_on_table  Table that is being joined to the first table.
#' @param join_on_column Column in the `join_on_table` that is joined onto the `column` of the first table.
#' @param kind Type of join. Defaults to left, and options include "LEFT", "RIGHT", "INNER", or "FULL"
#'
#' @param where_in_field Paired with `where_in_vector`, adds a "WHERE {field} IN ({vector})" clause to the query corresponding to the field-value pair.
#' @param where_not_in_field Paired with `where_not_in_vector`, adds a "WHERE {field} NOT IN ({vector})" clause to the query corresponding to the field-value pair.
#' @param limit (Optional) Integer of the row limit. Takes precedence over `random` if both arguments are provided.
#' @param random (Optional) Integer of the random number of rows to return. Is preceded by `limit` if both `limit` and `random` are provided.
#'
#' @name base_args
#' @keywords internal
#' @export
NULL

