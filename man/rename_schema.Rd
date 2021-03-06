% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_schema.R
\name{rename_schema}
\alias{rename_schema}
\title{Rename a Schema}
\usage{
rename_schema(
  conn,
  conn_fun,
  schema,
  new_schema_name,
  verbose = TRUE,
  render_sql = TRUE,
  render_only = FALSE
)
}
\arguments{
\item{conn}{Connection object}

\item{schema}{Schema to rename.}

\item{new_schema_name}{New schema name.}

\item{verbose}{If TRUE, details on the activity are returned in the console, such as when the querying starting and was completed.}

\item{render_sql}{If TRUE, the SQL statement for the operation is returned in the console.}
}
\description{
Rename a Schema
}
\seealso{
\code{\link[SqlRender]{render}}
}
