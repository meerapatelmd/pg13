#' @title
#' Base Arguments
#'
#' @param conn  A connection object returned by `connect()`.
#' @param data  A dataframe or tibble.
#' @param sql_statement A SQL Statement.
#' @param sql_statements A list or vector of SQL Statements of length 1 or greater.
#' @param schema The target schema for the operation.
#' @param tableName The target table in the `schema` for an operation or for writing functions, the name of the table that will be created.
#' @param verbose If TRUE, details on the activity are returned in the console, such as when the querying starting and was completed.
#' @param render_sql If TRUE, the SQL statement for the operation is returned in the console.
#'
#' @name base_args
NULL
