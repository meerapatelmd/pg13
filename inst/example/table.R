conn <- localConnect(dbname = "athena",
                     port = 5432)

searchTable(conn = conn,
            schema = "public",
            tableName = "concept",
            values = "Aspirin",
            case_insensitive = TRUE)


searchTable(conn = conn,
            schema = "public",
            tableName = "concept",
            values = "Aspirin",
            case_insensitive = FALSE)


searchTable(conn = conn,
            schema = "public",
            tableName = "concept",
            values = "Aspirin",
            case_insensitive = FALSE,
            verbose = FALSE)

searchTable(conn = conn,
            schema = "public",
            tableName = "concept",
            values = "Aspirin",
            case_insensitive = FALSE,
            verbose = FALSE,
            render_sql = TRUE,
            "concept_id",
            "concept_name")


searchTable(conn = conn,
            schema = "public",
            tableName = "concept",
            c("concept_id", "concept_name"),
            values = "Aspirin",
            case_insensitive = FALSE,
            verbose = FALSE,
            render_sql = TRUE)

dc(conn = conn,
   remove = TRUE)
