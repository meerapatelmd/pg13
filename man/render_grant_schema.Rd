% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render.R
\name{render_grant_schema}
\alias{render_grant_schema}
\title{Render GRANT ALL PRIVILEGES}
\usage{
render_grant_schema(schema, group = NULL, user = NULL)
}
\arguments{
\item{schema}{schema to grant privileges to}

\item{group}{group name, Default: NULL}

\item{user}{user name, Default: NULL}
}
\description{
Render SQL to grant all privileges to a schema a user or a group
}
\seealso{
\code{\link[SqlRender]{render}},\code{\link[SqlRender]{readSql}}
}
