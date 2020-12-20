conn <- local_connect(dbname = "pg13_test")
create_test_schema(conn = conn)
write_table(conn = conn,
            schema = "test_schema",
            table_name = "test_table",
            drop_existing = TRUE,
            data = data.frame(A = 1:3, B = letters[1:3]))

data_type_info(conn = conn,
               table = "test_table")



drop_table(conn = conn,
           schema = "test_schema",
           table = "test_table")

drop_schema(conn = conn,
            schema = "test_schema",
            cascade = TRUE)

dc(conn = conn)
