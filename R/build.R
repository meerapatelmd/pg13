#' @title
#' Build a Query for a Join
#'
#' @description
#' Build a query the joins 2 tables, respectively aliased as `a` and `b` in the query itself.
#'
#' @inheritParams args
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname build_join_query
#' @export
#' @importFrom SqlRender render

build_join_query <-
  function(select_table_fields = "*",
           select_join_on_fields = "*",
           distinct = FALSE,
           schema,
           table,
           column,
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
           limit,
           random) {
    .Deprecated(new = "draft_join1")



    if (!missing(random) && distinct) {
      typewrite_alert_danger("Cannot return DISTINCT and RANDOM. Overriding to DISTINCT == FALSE.")

      distinct <- FALSE
    }

    kind <-
      match.arg(
        arg = kind,
        choices = c("LEFT", "RIGHT", "INNER", "FULL"),
        several.ok = FALSE
      )

    # +++
    # Base Query
    # +++

    if (distinct) {
      sql_statement <-
        SqlRender::render(
          "SELECT DISTINCT @select_table_fields, @select_join_on_fields\nFROM @schema.@table a\n@kind JOIN @join_on_schema.@join_on_table b\nON a.@column = b.@join_on_column",
          select_table_fields = sprintf("a.%s", select_table_fields),
          select_join_on_fields = sprintf("b.%s", select_join_on_fields),
          schema = schema,
          table = table,
          kind = kind,
          join_on_schema = join_on_schema,
          join_on_table = join_on_table,
          column = column,
          join_on_column = join_on_column
        )
    } else {
      sql_statement <-
        SqlRender::render(
          "SELECT @select_table_fields, @select_join_on_fields\nFROM @schema.@table a\n@kind JOIN @join_on_schema.@join_on_table b\nON a.@column = b.@join_on_column",
          select_table_fields = sprintf("a.%s", select_table_fields),
          select_join_on_fields = sprintf("b.%s", select_join_on_fields),
          schema = schema,
          table = table,
          kind = kind,
          join_on_schema = join_on_schema,
          join_on_table = join_on_table,
          column = column,
          join_on_column = join_on_column
        )
    }

    # +++
    # Optional Where Filter
    # +++

    where <- list()
    if (!missing(where_in_field) && missing(where_in_vector) |
      missing(where_in_field) && !missing(where_in_vector)) {
      typewrite_alert_danger("both `where_in_field` & `where_in_vector` required. Ignoring filter...", wrap = TRUE)
    } else if (!missing(where_in_field) && !missing(where_in_vector)) {
      if (is.character(where_in_vector)) {
        where_in_vector <- s_quo(vector = where_in_vector)
      }

      where_ins <- list(
        field = where_in_field,
        vector = where_in_vector
      )

      where[[length(where) + 1]] <- where_ins
      names(where)[length(where)] <- "where_ins"
    }

    if (!missing(where_in_join_on_field) && missing(where_in_join_on_vector) |
      missing(where_in_join_on_field) && !missing(where_in_join_on_vector)) {
      typewrite_alert_danger("both `where_in_join_on_field` & `where_in_join_on_vector` required. Ignoring filter...", wrap = TRUE)
    } else if (!missing(where_in_join_on_field) && !missing(where_in_join_on_vector)) {
      if (is.character(where_in_join_on_vector)) {
        where_in_join_on_vector <- s_quo(vector = where_in_join_on_vector)
      }

      where_ins_join_on <- list(
        field = where_in_join_on_field,
        vector = where_in_join_on_vector
      )


      where[[length(where) + 1]] <- where_ins_join_on
      names(where)[length(where)] <- "where_ins_join_on"
    }

    if (!missing(where_not_in_field) && missing(where_not_in_vector) |
      missing(where_not_in_field) && !missing(where_not_in_vector)) {
      typewrite_alert_danger("both `where_not_in_field` & `where_not_in_vector` required. Ignoring filter...", wrap = TRUE)
    } else if (!missing(where_not_in_field) && !missing(where_not_in_vector)) {
      if (is.character(where_not_in_vector)) {
        where_not_in_vector <- s_quo(vector = where_not_in_vector)
      }

      where_not_ins <- list(
        field = where_not_in_field,
        vector = where_not_in_vector
      )

      where[[length(where) + 1]] <- where_not_ins
      names(where)[length(where)] <- "where_not_ins"
    }


    if (!missing(where_not_in_join_on_field) && missing(where_not_in_join_on_vector) |
      missing(where_not_in_join_on_field) && !missing(where_not_in_join_on_vector)) {
      typewrite_alert_danger("both `where_not_in_join_on_field` & `where_not_in_join_on_vector` required. Ignoring filter...", wrap = TRUE)
    } else if (!missing(where_not_in_join_on_field) && !missing(where_not_in_join_on_vector)) {
      if (is.character(where_not_in_join_on_vector)) {
        where_not_in_join_on_vector <- s_quo(vector = where_not_in_join_on_vector)
      }

      where_not_ins_join_on <- list(
        field = where_not_in_join_on_field,
        vector = where_not_in_join_on_vector
      )


      where[[length(where) + 1]] <- where_not_ins_join_on
      names(where)[length(where)] <- "where_not_ins_join_on"
    }


    if (!missing(where_is_null_field)) {
      where[[length(where) + 1]] <- SqlRender::render("a.@field IS NULL", field = where_is_null_field)
      names(where)[length(where)] <- "where_is_null_field"
    }

    if (!missing(where_is_not_null_field)) {
      where[[length(where) + 1]] <- SqlRender::render("a.@field IS NOT NULL", field = where_is_not_null_field)
      names(where)[length(where)] <- "where_is_not_null_field"
    }

    if (!missing(where_is_null_join_on_field)) {
      where[[length(where) + 1]] <- SqlRender::render("b.@field IS NULL", field = where_is_null_join_on_field)
      names(where)[length(where)] <- "where_is_null_join_on_field"
    }

    if (!missing(where_is_not_null_join_on_field)) {
      where[[length(where) + 1]] <- SqlRender::render("b.@field IS NOT NULL", field = where_is_not_null_join_on_field)
      names(where)[length(where)] <- "where_is_not_null_join_on_field"
    }



    if (length(where) > 0) {
      sql_statement <- sprintf("%s\nWHERE\n", sql_statement)

      where_clauses <- vector()
      if ("where_ins" %in% names(where)) {
        field <- where$where_ins$field
        vector <- where$where_ins$vector


        if (case_insensitive && is.character(vector)) {
          where_in_clause_a <-
            SqlRender::render(
              "LOWER(a.@field::varchar) IN (@vector)",
              field = field,
              vector = tolower(vector)
            )
        } else {
          where_in_clause_a <-
            SqlRender::render(
              "a.@field IN (@vector)",
              field = field,
              vector = vector
            )
        }

        where_clauses <-
          c(
            where_clauses,
            where_in_clause_a
          )
      }

      if ("where_ins_join_on" %in% names(where)) {
        field <- where$where_ins_join_on$field
        vector <- where$where_ins_join_on$vector

        if (case_insensitive && is.character(vector)) {
          where_in_clause_b <-
            SqlRender::render(
              "LOWER(b.@field::varchar) IN (@vector)",
              field = field,
              vector = tolower(vector)
            )
        } else {
          where_in_clause_b <-
            SqlRender::render(
              "b.@field IN (@vector)",
              field = field,
              vector = vector
            )
        }

        where_clauses <-
          c(
            where_clauses,
            where_in_clause_b
          )
      }

      if ("where_not_ins" %in% names(where)) {
        field <- where$where_not_ins$field
        vector <- where$where_not_ins$vector

        if (case_insensitive && is.character(vector)) {
          where_not_in_clause_a <-
            SqlRender::render(
              "LOWER(a.@field::varchar) NOT IN (@vector)",
              field = field,
              vector = tolower(vector)
            )
        } else {
          where_not_in_clause_a <-
            SqlRender::render(
              "a.@field NOT IN (@vector)",
              field = field,
              vector = vector
            )
        }

        where_clauses <-
          c(
            where_clauses,
            where_not_in_clause_a
          )
      }

      if ("where_not_ins_join_on" %in% names(where)) {
        field <- where$where_not_ins_join_on$field
        vector <- where$where_not_ins_join_on$vector


        if (case_insensitive && is.character(vector)) {
          where_not_in_clause_b <-
            SqlRender::render(
              "LOWER(b.@field::varchar) NOT IN (@vector)",
              field = field,
              vector = tolower(vector)
            )
        } else {
          where_not_in_clause_b <-
            SqlRender::render(
              "b.@field NOT IN (@vector)",
              field = field,
              vector = vector
            )
        }

        where_clauses <-
          c(
            where_clauses,
            where_not_in_clause_b
          )
      }



      null_field_args <- c(
        "where_is_null_field",
        "where_is_null_join_on_field",
        "where_is_not_null_field",
        "where_is_not_null_join_on_field"
      )

      if (any(null_field_args %in% names(where))) {
        null_where_clauses <- unlist(where[names(where) %in% c(
          "where_is_null_field",
          "where_is_null_join_on_field",
          "where_is_not_null_field",
          "where_is_not_null_join_on_field"
        )])

        final_where <- paste(c(null_where_clauses, where_clauses), collapse = " AND \n")
      } else {
        final_where <- paste(where_clauses, collapse = " AND \n")
      }

      sql_statement <- paste0(sql_statement, final_where)
    }

    # +++
    # Add Limit or Random
    # +++

    if (!missing(limit)) {
      sql_statement <-
        sprintf(
          "%s\n%s",
          sql_statement,
          SqlRender::render("LIMIT @limit", limit = limit)
        )
    } else if (!missing(random)) {
      sql_statement <-
        sprintf(
          "%s\n%s",
          sql_statement,
          SqlRender::render("ORDER BY RANDOM()\nLIMIT @limit", limit = random)
        )
    }

    sql_statement
  }


#' @title
#' Build a Simple SQL Query
#'
#' @inheritParams args
#' @example inst/example/build_query.R
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname build_query
#' @export
#' @importFrom SqlRender render

build_query <-
  function(fields = "*",
           distinct = FALSE,
           schema,
           table,
           where_in_field,
           where_in_vector,
           where_not_in_field,
           where_not_in_vector,
           where_is_null_field,
           where_is_not_null_field,
           case_insensitive = TRUE,
           limit,
           random) {
    if (!missing(random) && distinct) {
      typewrite_alert_danger("Cannot return DISTINCT and RANDOM. Overriding to DISTINCT == FALSE.")

      distinct <- FALSE
    }


    # +++
    # Base Query
    # +++

    if (distinct) {
      sql_statement <-
        SqlRender::render(
          "SELECT DISTINCT @fields\nFROM @schema.@table\n",
          fields = fields,
          schema = schema,
          table = table
        )
    } else {
      sql_statement <-
        SqlRender::render(
          "SELECT @fields\nFROM @schema.@table",
          fields = fields,
          schema = schema,
          table = table
        )
    }

    # +++
    # Optional Where Filter
    # +++

    where <- list()
    if (!missing(where_in_field) && missing(where_in_vector) |
      missing(where_in_field) && !missing(where_in_vector)) {
      typewrite_alert_danger("both `where_in_field` & `where_in_vector` required. Ignoring filter...", wrap = TRUE)
    } else if (!missing(where_in_field) && !missing(where_in_vector)) {
      if (is.character(where_in_vector)) {
        where_in_vector <- s_quo(vector = where_in_vector)
      }

      where_ins <- list(
        field = where_in_field,
        vector = where_in_vector
      )

      where[[length(where) + 1]] <- where_ins
      names(where)[length(where)] <- "where_ins"
    }


    if (!missing(where_not_in_field) && missing(where_not_in_vector) |
      missing(where_not_in_field) && !missing(where_not_in_vector)) {
      typewrite_alert_danger("both `where_not_in_field` & `where_not_in_vector` required. Ignoring filter...", wrap = TRUE)
    } else if (!missing(where_not_in_field) && !missing(where_not_in_vector)) {
      if (is.character(where_not_in_vector)) {
        where_not_in_vector <- s_quo(vector = where_not_in_vector)
      }

      where_not_ins <- list(
        field = where_not_in_field,
        vector = where_not_in_vector
      )

      where[[length(where) + 1]] <- where_not_ins
      names(where)[length(where)] <- "where_not_ins"
    }


    if (!missing(where_is_null_field)) {
      where[[length(where) + 1]] <- SqlRender::render("a.@field IS NULL", field = where_is_null_field)
      names(where)[length(where)] <- "where_is_null_field"
    }

    if (!missing(where_is_not_null_field)) {
      where[[length(where) + 1]] <- SqlRender::render("a.@field IS NOT NULL", field = where_is_not_null_field)
      names(where)[length(where)] <- "where_is_not_null_field"
    }



    if (length(where) > 0) {
      sql_statement <- sprintf("%s\nWHERE\n", sql_statement)

      where_clauses <- vector()
      if ("where_ins" %in% names(where)) {
        field <- where$where_ins$field
        vector <- where$where_ins$vector


        if (case_insensitive && is.character(vector)) {
          where_in_clause_a <-
            SqlRender::render(
              "LOWER(a.@field::varchar) IN (@vector)",
              field = field,
              vector = tolower(vector)
            )
        } else {
          where_in_clause_a <-
            SqlRender::render(
              "a.@field IN (@vector)",
              field = field,
              vector = vector
            )
        }

        where_clauses <-
          c(
            where_clauses,
            where_in_clause_a
          )
      }


      if ("where_not_ins" %in% names(where)) {
        field <- where$where_not_ins$field
        vector <- where$where_not_ins$vector

        if (case_insensitive && is.character(vector)) {
          where_not_in_clause_a <-
            SqlRender::render(
              "LOWER(a.@field::varchar) NOT IN (@vector)",
              field = field,
              vector = tolower(vector)
            )
        } else {
          where_not_in_clause_a <-
            SqlRender::render(
              "a.@field NOT IN (@vector)",
              field = field,
              vector = vector
            )
        }

        where_clauses <-
          c(
            where_clauses,
            where_not_in_clause_a
          )
      }


      null_field_args <- c(
        "where_is_null_field",
        "where_is_not_null_field"
      )

      if (any(null_field_args %in% names(where))) {
        null_where_clauses <- unlist(where[names(where) %in% c(
          "where_is_null_field",
          "where_is_not_null_field"
        )])

        final_where <- paste(c(null_where_clauses, where_clauses), collapse = " AND \n")
      } else {
        final_where <- paste(where_clauses, collapse = " AND \n")
      }

      sql_statement <- paste0(sql_statement, final_where)
    }

    # +++
    # Add Limit or Random
    # +++

    if (!missing(limit)) {
      sql_statement <-
        sprintf(
          "%s\n%s",
          sql_statement,
          SqlRender::render("LIMIT @limit", limit = limit)
        )
    } else if (!missing(random)) {
      sql_statement <-
        sprintf(
          "%s\n%s",
          sql_statement,
          SqlRender::render("ORDER BY RANDOM()\nLIMIT @limit", limit = random)
        )
    }

    sql_statement
  }

#' @title
#' Build a Regex Query
#'
#' @description
#' Build a query that searches for a regular expression.
#'
#' @inheritParams args
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname build_query_regex
#' @export
#' @importFrom SqlRender render



build_query_regex <-
  function(fields = "*",
           distinct = FALSE,
           schema,
           table,
           where_regex_field,
           regex,
           invert = FALSE,
           case_insensitive = TRUE,
           limit,
           random) {
    if (case_insensitive && !invert) {
      operator <- "~*"
    } else if (case_insensitive && invert) {
      operator <- "!~*"
    } else if (!case_insensitive && !invert) {
      operator <- "~"
    } else if (!case_insensitive && invert) {
      operator <- "!~"
    }

    if (!missing(random) && distinct) {
      typewrite_alert_warning("Cannot return DISTINCT and RANDOM. Overriding to DISTINCT == FALSE.")

      distinct <- FALSE
    }


    # +++
    # Base Query
    # +++

    if (distinct) {
      sql_statement <-
        SqlRender::render(
          "SELECT DISTINCT @fields\nFROM @schema.@table\nWHERE @where_regex_field @operator '@regex'",
          fields = fields,
          schema = schema,
          table = table,
          where_regex_field = where_regex_field,
          operator = operator,
          regex = regex
        )
    } else {
      sql_statement <-
        SqlRender::render(
          "SELECT @fields\nFROM @schema.@table\nWHERE @where_regex_field @operator '@regex'",
          fields = fields,
          schema = schema,
          table = table,
          where_regex_field = where_regex_field,
          operator = operator,
          regex = regex
        )
    }


    # +++
    # Add Limit or Random
    # +++

    if (!missing(limit)) {
      sql_statement <-
        sprintf(
          "%s\n%s",
          sql_statement,
          SqlRender::render("LIMIT @limit", limit = limit)
        )
    } else if (!missing(random)) {
      sql_statement <-
        sprintf(
          "%s\n%s",
          sql_statement,
          SqlRender::render("ORDER BY RANDOM()\nLIMIT @limit", limit = random)
        )
    }

    sql_statement
  }


#' @title
#' Build a Like Query
#'
#' @description
#' A `Like Query` is one where a search for a value is performed with flanking wildcards.
#'
#' @inheritParams args
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname build_query_like
#' @export
#' @importFrom SqlRender render


build_query_like <-
  function(fields = "*",
           distinct = FALSE,
           schema,
           table,
           where_like_field,
           where_like_value,
           case_insensitive = TRUE,
           limit_n = NULL) {
    if (distinct) {
      if (case_insensitive) {
        sql_statement <-
          SqlRender::render(
            "SELECT DISTINCT @fields\nFROM @schema.@table\nWHERE LOWER(@where_like_field) LIKE '%@where_like_value%'",
            fields = fields,
            schema = schema,
            table = table,
            where_like_field = where_like_field,
            where_like_value = tolower(where_like_value)
          )
      } else {
        sql_statement <-
          SqlRender::render(
            "SELECT DISTINCT @fields\nFROM @schema.@table\nWHERE @where_like_field LIKE '%@where_like_value%'",
            fields = fields,
            schema = schema,
            table = table,
            where_like_field = where_like_field,
            where_like_value = where_like_value
          )
      }
    } else {
      if (case_insensitive) {
        sql_statement <-
          SqlRender::render(
            "SELECT @fields\nFROM @schema.@table\nWHERE LOWER(@where_like_field) LIKE '%@where_like_value%'",
            fields = fields,
            schema = schema,
            table = table,
            where_like_field = where_like_field,
            where_like_value = tolower(where_like_value)
          )
      } else {
        sql_statement <-
          SqlRender::render(
            "SELECT @fields\nFROM @schema.@table\nWHERE @where_like_field LIKE '%@where_like_value%'",
            fields = fields,
            schema = schema,
            table = table,
            where_like_field = where_like_field,
            where_like_value = where_like_value
          )
      }
    }

    sql_statement
  }






#' @title
#' Build a Query the Loops Over String
#' @description
#' Write a SQL Query that loops over the words in a string.
#' @inheritParams args
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname build_query_string
#' @export
#' @importFrom SqlRender render

build_query_string <-
  function(fields = "*",
           distinct = FALSE,
           schema,
           table,
           where_like_field,
           string,
           split = " |[[:punct:]]",
           case_insensitive = TRUE,
           limit_n = NULL) {
    args <- unlist(strsplit(string, split = split))
    args <- trimws(args, which = "both")
    args <- args[!(args %in% c(""))]


    if (case_insensitive) {
      sql_statement <-
        SqlRender::render("SELECT @fields\nFROM @schema.@table\nWHERE @where_clauses",
          fields = fields,
          schema = schema,
          table = table,
          where_clauses = paste(sprintf(
            "LOWER(%s) LIKE '%s'",
            where_like_field,
            paste0("%", tolower(args), "%")
          ),
          collapse = " AND\n"
          )
        )
    } else {
      sql_statement <-
        SqlRender::render("SELECT @fields\nFROM @schema.@table\nWHERE @where_clauses",
          fields = fields,
          schema = schema,
          table = table,
          where_clauses = paste(sprintf(
            "%s LIKE '%s'",
            where_like_field,
            paste0("%", args, "%")
          ),
          collapse = " AND\n"
          )
        )
    }


    sql_statement
  }
