% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constraints.R
\name{get_kcu}
\alias{get_kcu}
\title{FUNCTION_TITLE}
\usage{
get_kcu(
  schema,
  conn,
  conn_fun = "pg13::local_connect()",
  checks = c("conn_status", "conn_type", "rows"),
  verbose = TRUE,
  render_sql = TRUE,
  render_only = FALSE,
  log_file = "",
  append_log = TRUE,
  sep_log = "\\n",
  sql_style = c("inline", "chunk"),
  rmd_file = "",
  sql_file = "",
  warn_no_rows = "deprecated",
  warnMissingParameters = TRUE,
  ...
)
}
\arguments{
\item{schema}{PARAM_DESCRIPTION}

\item{conn}{PARAM_DESCRIPTION}

\item{conn_fun}{PARAM_DESCRIPTION, Default: 'pg13::local_connect()'}

\item{checks}{PARAM_DESCRIPTION, Default: c("conn_status", "conn_type", "rows")}

\item{verbose}{PARAM_DESCRIPTION, Default: TRUE}

\item{render_sql}{PARAM_DESCRIPTION, Default: TRUE}

\item{render_only}{PARAM_DESCRIPTION, Default: FALSE}

\item{log_file}{PARAM_DESCRIPTION, Default: ''}

\item{append_log}{PARAM_DESCRIPTION, Default: TRUE}

\item{sep_log}{PARAM_DESCRIPTION, Default: ''}

\item{sql_style}{PARAM_DESCRIPTION, Default: c("inline", "chunk")}

\item{rmd_file}{PARAM_DESCRIPTION, Default: ''}

\item{sql_file}{PARAM_DESCRIPTION, Default: ''}

\item{warn_no_rows}{PARAM_DESCRIPTION, Default: 'deprecated'}

\item{warnMissingParameters}{PARAM_DESCRIPTION, Default: TRUE}

\item{...}{PARAM_DESCRIPTION}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
FUNCTION_DESCRIPTION
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[glue]{glue}}
}
