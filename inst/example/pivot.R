library(tidyverse)
conn <- pg13::local_connect()
 pg13::write_table(conn = conn,
                  schema = "public",
                  table_name = "test_table",
                  drop_existing = TRUE,
                  data =
                          tribble(~Person,~Attribute, ~Attribute_Value,
                                  "Meera", "Height", "Tall",
                                  "Syed", "Height", "Tall",
                                  "Syed", "Character", "Funny",
                                  "Meera", "Character", NA_character_))

pivot_table(conn = conn,
            schema = "public",
            table = "test_table",
            id_column = "person",
            names_from_column = "attribute",
            values_from_column = "attribute_value")


pg13::write_table(conn = conn,
                  schema = "public",
                  table_name = "test_table",
                  drop_existing = TRUE,
                  data =
                          tribble(~Person,~Attribute, ~Attribute_Value, ~Attribute_Date,
                                  "Meera", "Age", "Tall", as.Date("2020-12-27"),
                                  "Syed", "Height", "Tall", as.Date("2020-12-27"),
                                  "Syed", "Age", "Funny", as.Date("2020-12-27")))
pivot_table(conn = conn,
            schema = "public",
            table = "test_table",
            id_column = "person",
            names_from_column = "attribute",
            values_from_column = "attribute_date")


pg13::write_table(conn = conn,
                  schema = "public",
                  table_name = "test_table",
                  drop_existing = TRUE,
                  data =
                          tribble(~Person,~Measurement, ~Results, ~Results_Date,
                                  "Meera", "Age", 36, as.Date("2020-12-27"),
                                  "Syed", "Height", 74, as.Date("2020-12-27"),
                                  "Syed", "Age", 32, as.Date("2020-12-27"),
                                  "Meera", "Height", 68, as.Date("2020-12-28")
                                  )
                                )

pivot_table(conn = conn,
            schema = "public",
            table = "test_table",
            id_column = "person",
            names_from_column = "measurement",
            values_from_column = "results")


pg13::drop_table(conn = conn,
                 schema = "public",
                 table = "test_table")

pg13::dc(conn = conn)

