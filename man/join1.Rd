% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join1.R
\name{join1}
\alias{join1}
\title{1st Degree Join}
\usage{
join1(
  conn,
  conn_fun,
  write_schema,
  data,
  column,
  select_table_fields = "*",
  select_join_on_fields = "*",
  join_on_schema,
  join_on_table,
  join_on_column,
  kind = c("LEFT", "RIGHT", "INNER", "FULL"),
  where_in_field,
  where_in_vector,
  where_in_join_on_field,
  where_in_join_on_vector,
  where_not_in_field,
  where_not_in_vector,
  where_not_in_join_on_field,
  where_not_in_join_on_vector,
  where_is_null_field,
  where_is_not_null_field,
  where_is_null_join_on_field,
  where_is_not_null_join_on_field,
  case_insensitive,
  distinct = FALSE,
  verbose = TRUE,
  render_sql = TRUE,
  render_only = FALSE
)
}
\description{
1st Degree Join
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

# Write a table to join to without dropping
write_table(conn = conn,
            schema = "test_schema",
            table_name = "test_table2",
            drop_existing = TRUE,
            data = data.frame(A = 1:25, B = letters[1:25]))

# Joining Test Data
test_data <-
        data.frame(A = 1:100, B = letters[1:100])
test_data

join1(conn = conn,
      write_schema = "public",
      data = test_data,
      column = "A",
      kind = "INNER",
      join_on_schema = "test_schema",
      join_on_table = "test_table2",
      join_on_column = "A")

join1(conn = conn,
      write_schema = "public",
      data = test_data,
      column = "A",
      kind = "RIGHT",
      join_on_schema = "test_schema",
      join_on_table = "test_table2",
      join_on_column = "A")

join1(conn = conn,
      write_schema = "public",
      data = test_data,
      column = "A",
      kind = "LEFT",
      join_on_schema = "test_schema",
      join_on_table = "test_table2",
      join_on_column = "A")

join1(conn = conn,
      write_schema = "public",
      data = test_data,
      column = "A",
      kind = "FULL",
      join_on_schema = "test_schema",
      join_on_table = "test_table2",
      join_on_column = "A")


drop_schema(conn = conn,
            schema = "test_schema",
            cascade = TRUE)

dc(conn = conn)
}
