library(pg13)
library(tidyverse)
test_data <-
        tibble::tibble(A = 1:3,
                       B = letters[1:3],
                       C = c(TRUE, FALSE, TRUE),
                       D = c(1.23421, 234.23421, 342.0134014134),
                       E = c(Sys.Date(), Sys.Date()-1013, Sys.Date() + 134))
conn <- local_connect("pg13_test")


# Writing a staging table. Note that since the function is not called
# inside a enclosed parent frame, the table is not dropped even when
# drop_on_exit is set to TRUE
write_staging_table(conn = conn,
                    schema = "public",
                    data = test_data,
                    drop_on_exit = TRUE)

ls_tables(conn = conn,
          schema = "public")


# All tables in a schema can be dropped with pattern matching to the "V{timestamp}" format
drop_all_staging_tables(conn = conn,
                        schema = "public")

# The `time_diff_hours` option allows for the user to drop all tables other than the most recently written tables based on the number of hours from the current timestamp.
write_staging_table(conn = conn,
                    schema = "public",
                    data = test_data)

drop_all_staging_tables(conn = conn,
                        schema = "public",
                        time_diff_hours = 8)

ls_tables(conn = conn,
          schema = "public")

# If this function is called within an enclosed parent frame with `drop_on_exit` set to TRUE, the table will be dropped at the conclusion of the function.
test_fun <-
        function() {

                new_table <-
                write_staging_table(conn = conn,
                                    schema = "public",
                                    data = test_data,
                                    drop_on_exit = TRUE)

                print("The test is finished.")
        }

test_fun()
ls_tables(conn = conn,
          schema = "public")

# Dropping all staging tables before exiting the example
drop_all_staging_tables(conn = conn,
                        schema = "public",
                        time_diff_hours = 8)

dc(conn = conn)
