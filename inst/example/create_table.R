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

# Write a table
write_table(conn = conn,
            schema = "test_schema",
            table_name = "test_table",
            drop_existing = TRUE,
            data = data.frame(A = 1:3, B = letters[1:3]))

# Write the same table using create_table() instead
create_table(conn = conn,
             schema = "test_schema",
             table_name = "test_table_b",
             if_not_exists = TRUE,
             A = "integer",
             B = "varchar(1)")

append_table(conn = conn,
             schema = "test_schema",
             table = "test_table_b",
             data = data.frame(A = 1:3, B = letters[1:3]))

# Under the hood is the draft_create_table()
draft_create_table(schema = "test_schema",
                   table_name = "test_table_b",
                   A = "integer",
                   B = "varchar(1)")

# The DDL can be automatically discerned using create_table_from_df()
create_table_from_df(conn = conn,
                     schema = "test_schema",
                     table_name = "test_table_c",
                     data = data.frame(A = 1:3, B = letters[1:3]))

# Under the hood is the draft_create_table_from_df()
draft_create_table_from_df(schema = "test_schema",
                           table_name = "test_table_c",
                           data = data.frame(A = 1:3, B = letters[1:3]))

drop_schema(conn = conn,
            schema = "test_schema",
            cascade = TRUE)

dc(conn = conn)
