# Building SQL Statements using R
library(pg13)

# Build a Simple Query
build_query(schema = "public",
            table = "concept_synonym")

build_query(distinct = TRUE,
            schema = "public",
            table = "concept_synonym")


build_query(fields = c("concept_id", "concept_name"),
            distinct = TRUE,
            schema = "public",
            table = "concept_synonym")

# WHERE filters may be applied, but will be ignored if the field-value pair is not provided
build_query(fields = c("concept_id", "concept_name"),
            schema = "public",
            table = "concept_synonym",
            where_in_field = "concept_synonym")

# The query defaults to transforming elements to lowercase for case insensitivity that can be turned off
build_query(fields = c("concept_id", "concept_name"),
            schema = "public",
            table = "concept_synonym",
            where_in_field = "concept_synonym",
            where_in_vector = c("Myocardial Infarction", "Heart Attack"))

build_query(fields = c("concept_id", "concept_name"),
            schema = "public",
            table = "concept_synonym",
            where_in_field = "concept_synonym",
            where_in_vector = c("Myocardial Infarction", "Heart Attack"),
            case_insensitive = FALSE)

