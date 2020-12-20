library(pg13)
conn <- local_connect(dbname = "postgres")
create_test_db <-
        function(conn) {

                if (!db_exists(conn = conn,
                          db_name = "pg13_test")) {

                        cli::cli_rule("Create 'pg13_test' Database")

                        create_db(conn = conn,
                                  db_name = "pg13_test")

                }

        }

create_test_schema <-
        function(conn) {

                if (!schema_exists(conn = conn,
                                   schema = "test_schema")) {

                        cli::cli_rule("Create 'test_schema' Schema")

                        create_schema(conn = conn,
                                      schema = "test_schema")

                }
        }

create_test_db(conn = conn)
dc(conn = conn)


conn <- local_connect(dbname = "pg13_test")
create_test_schema(conn = conn)
write_table(conn = conn,
            schema = "test_schema",
            table_name = "test_table",
            drop_existing = TRUE,
            data = data.frame(A = 1:3, B = letters[1:3]))


append_table(conn = conn,
             schema = "test_schema",
             table = "test_table",
             data = data.frame(A = 1:3, B = letters[1:3]))

append_table(conn = conn,
             schema = "test_schema",
             table = "test_table",
             data = data.frame(A = 1:3, B = rep(NA_character_, 3)))

test_data <- read_table(conn = conn,
                           schema = "test_schema",
                           table = "test_table")

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
           if_exists = FALSE,
           data = data.frame(A = 1:3, B = rep(NA_character_, 3)))

drop_schema(conn = conn,
            schema = "test_schema",
            cascade = TRUE)

dc(conn = conn)
