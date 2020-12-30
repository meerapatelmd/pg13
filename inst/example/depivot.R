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

depivot_table(conn = conn,
              schema = "public",
              table = "test_table",
              names_to_columns = "attribute",
              names_to = "field",
              values_to_column = "attribute_type")


depivot_table(conn = conn,
              schema = "public",
              table = "test_table",
              names_to_columns = c("person", "attribute", "attribute_value"),
              names_to = "field",
              values_to_column = "attribute_type")

pg13::drop_table(conn = conn,
                 schema = "public",
                 table = "test_table")


pg13::dc(conn = conn)

