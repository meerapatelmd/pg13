% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_table}
\alias{read_table}
\title{Read an Entire Table}
\usage{
read_table(
  conn,
  conn_fun,
  schema,
  table,
  verbose = TRUE,
  render_sql = TRUE,
  warn_no_rows = TRUE,
  render_only = FALSE,
  log_file = "",
  append_log = TRUE,
  sep_log = "\\n"
)
}
\arguments{
\item{conn}{Connection object}

\item{schema}{The target schema for the operation.}

\item{table}{Target table for the operation.}

\item{verbose}{If TRUE, details on the activity are returned in the console, such as when the querying starting and was completed.}

\item{render_sql}{If TRUE, the SQL statement for the operation is returned in the console.}

\item{warn_no_rows}{If TRUE, a warning is given that query has returned 0 rows.}
}
\description{
Shortcut for a \verb{SELECT *} SQL statement.
}
\examples{
library(pg13)
create_test_schema <-
        function(conn) {

                if (!schema_exists(conn = conn,
                                   schema = "test_schema")) {

                        create_schema(conn = conn,
                                      schema = "test_schema")

                }
        }

conn <- local_connect(dbname = "pg13_test")
create_test_schema(conn = conn)

# Write a table without dropping
write_table(conn = conn,
            schema = "test_schema",
            table_name = "test_table2",
            drop_existing = FALSE,
            data = data.frame(A = 1:3, B = letters[1:3]))

# Write a table with dropping
write_table(conn = conn,
            schema = "test_schema",
            table_name = "test_table",
            drop_existing = TRUE,
            data = data.frame(A = 1:3, B = letters[1:3]))


# Include the name of the dataframe object in the messages
test_data <-
        data.frame(A = 1:3, B = letters[1:3])
write_table(conn = conn,
            schema = "test_schema",
            table_name = "test_table3",
            drop_existing = TRUE,
            data = test_data)

# Append a table
append_table(conn = conn,
             schema = "test_schema",
             table = "test_table",
             data = data.frame(A = 1:3, B = letters[1:3]))

# Warning is returned if NAs are in the input data
append_table(conn = conn,
             schema = "test_schema",
             table = "test_table",
             data = data.frame(A = 1:3, B = rep(NA_character_, 3)))

# Alert is returned if the input data contains 0 rows
test_data2 <-
        data.frame(A = 1,
                   B = 2)
test_data2 <- test_data2[-1,]
append_table(conn = conn,
             schema = "test_schema",
             table = "test_table",
             data = test_data2)

# Message is returned if incoming data contains more than 0 rows
test_data <- read_table(conn = conn,
                           schema = "test_schema",
                           table = "test_table")

test_data

# Alert is returned if incoming data contains 0 rows
write_table(conn = conn,
            schema = "test_schema",
            table_name = "test_table4",
            drop_existing = TRUE,
            data = test_data2)
test_data <- read_table(conn = conn,
                        schema = "test_schema",
                        table = "test_table4")
test_data

search_table(conn = conn,
             schema = "test_schema",
             table = "test_table",
             values = 1:3)

search_table(conn = conn,
             schema = "test_schema",
             table = "test_table",
             values = "a")

search_table(conn = conn,
             schema = "test_schema",
             table = "test_table",
             values = c("A", "b", "C"),
             case_insensitive = FALSE)

drop_table(conn = conn,
           schema = "test_schema",
           table = "test_table",
           if_exists = FALSE)

drop_table(conn = conn,
           schema = "test_schema",
           table = "test_table2",
           if_exists = FALSE)

drop_schema(conn = conn,
            schema = "test_schema",
            cascade = TRUE)

dc(conn = conn)
}
\seealso{
Other table functions: 
\code{\link{appendTable}()},
\code{\link{append_table}()},
\code{\link{create_table_from_df}()},
\code{\link{create_table}()},
\code{\link{drop_all_staging_tables}()},
\code{\link{drop_table_batch}()},
\code{\link{drop_table}()},
\code{\link{searchTable}()},
\code{\link{search_table}()},
\code{\link{write_staging_table}()},
\code{\link{write_table}()}

Other read functions: 
\code{\link{read_view}()}
}
\concept{read functions}
\concept{table functions}
