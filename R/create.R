#' Create a Schema
#' @export
#' @rdname create_schema
#' @example inst/example/schema.R
#' @family schema functions
#' @family create functions

create_schema <-
  function(conn,
           conn_fun = "pg13::local_connect()",
           schema,
           verbose = TRUE,
           render_sql = TRUE,
           render_only = FALSE,
           ...) {
    send(
      conn = conn,
      conn_fun = conn_fun,
      verbose = verbose,
      render_sql = render_sql,
      render_only = render_only,
      sql_statement =
        SqlRender::render(
          "CREATE SCHEMA @schema;",
          schema = schema
        ),
      ...
    )
  }



#' @title
#' Draft SQL to Create a Table
#'
#' @description
#' Draft a SQL that creates a table using DDL derived from a dataframe. Drafting is stopped if the given tablename or fields in the dataframe are a SQL reserve word.
#' @param ... Named vector of field names and their corresponding data definition.
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[SqlRender]{render}}
#' @rdname draft_create_table
#' @export
#' @importFrom rlang list2
#' @importFrom SqlRender render
#' @family create functions
#' @family draft functions
#' @example inst/example/create_table.R

draft_create_table <-
  function(schema,
           table_name,
           if_not_exists = TRUE,
           ...) {
    ddl <- rlang::list2(...)
    fields <- names(ddl)

    if (any(isReserved(table_name, fields))) {
      stop("Cannot use reserved sql words.")
    }

    ddl <- mapply(paste, fields, ddl, collapse = " ")
    ddl <- paste(ddl, collapse = ",\n")

    if (if_not_exists) {
      sql_statement <-
        SqlRender::render(
          "
                                        CREATE TABLE IF NOT EXISTS @schema.@table_name (
                                                @ddl
                                        );
                                        ",
          schema = schema,
          table_name = table_name,
          ddl = ddl
        )
    } else {
      sql_statement <-
        SqlRender::render(
          "
                                        CREATE TABLE @schema.@table_name (
                                                @ddl
                                        );
                                        ",
          schema = schema,
          table_name = table_name,
          ddl = ddl
        )
    }

    sql_statement
  }

#' @title
#' Create a Table
#' @param ... Named vector of field names and their corresponding data definition.
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[SqlRender]{render}}
#'  \code{\link[pg13]{send}}
#' @rdname create_table
#' @export
#' @importFrom rlang list2
#' @importFrom SqlRender render
#' @family create functions
#' @family table functions
#' @example inst/example/create_table.R

create_table <-
  function(conn,
           conn_fun = "pg13::local_connect()",
           schema,
           table_name,
           if_not_exists = TRUE,
           ...,
           verbose = TRUE,
           render_sql = TRUE) {
    sql_statement <-
      draft_create_table(
        schema = schema,
        table_name = table_name,
        if_not_exists = if_not_exists,
        ...
      )


    send(
      conn = conn,
      conn_fun = conn_fun,
      sql_statement = sql_statement,
      verbose = verbose,
      render_sql = render_sql
    )
  }

#' @title
#' Draft the SQL to Create Table
#'
#' @description
#' Draft a SQL that creates a table using DDL derived from a dataframe. Drafting is stopped if the given tablename or fields in the dataframe are a SQL reserve word.
#'
#' @seealso
#'  \code{\link[forcats]{fct_collapse}}
#' @rdname draft_create_table_from_df
#' @export
#' @importFrom forcats fct_collapse
#' @family create functions
#' @family draft functions
#' @example inst/example/create_table.R

draft_create_table_from_df <-
  function(schema,
           table_name,
           data,
           if_not_exists = TRUE) {
    make_ddl <-
      function(data) {
        r_types <- lapply(data, class)
        r_types <- sapply(r_types, paste, collapse = ", ") # necessary because there are 2 classes for some data types, such as sys.time()'s posixct, posixt

        field_names <- names(r_types)

        ddl <- factor(r_types)
        ddl <-
          forcats::fct_collapse(
            .f = ddl,
            bigint = "integer",
            `timestamp without time zone` = "POSIXct, POSIXt",
            date = "Date",
            float = c("double", "numeric"),
            other_level = "text"
          ) %>%
          as.character()

        names(ddl) <- field_names
        ddl
      }

    ddl <- make_ddl(data = data)
    fields <- names(ddl)

    if (any(isReserved(table_name, fields))) {
      stop("Cannot use reserved sql words.")
    }

    ddl <- mapply(paste, fields, ddl, collapse = " ")
    ddl <- paste(ddl, collapse = ",\n")

    if (if_not_exists) {
      sql_statement <-
        SqlRender::render(
          "
                                        CREATE TABLE IF NOT EXISTS @schema.@table_name (
                                                @ddl
                                        );
                                        ",
          schema = schema,
          table_name = table_name,
          ddl = ddl
        )
    } else {
      sql_statement <-
        SqlRender::render(
          "
                                        CREATE TABLE @schema.@table_name (
                                                @ddl
                                        );
                                        ",
          schema = schema,
          table_name = table_name,
          ddl = ddl
        )
    }

    sql_statement
  }

#' @title
#' Create a Table with a Dataframe
#'
#' @description
#' Derive DDL using the data classes of each field in a dataframe. The map between the R data classes and the Postgresql data types can be found at \code{\link{renderCreateTableFromDF}}. The dataframe can then be appended to the table using \code{\link{appendTable}}. This method is favorable to a direct call to \code{\link{writeTable}} because in some cases, future appends to the table may not adhere to the data definitions created at the time of writing. For example, \code{\link{writeTable}} defaults to `VARCHAR(255)` for all character classes whereas future appends may contain text greater than 255 characters, resulting in error. This function rolls all character classes to `TEXT` data types instead.
#'
#' @rdname create_table_from_df
#' @export
#' @family create functions
#' @family table functions
#' @example inst/example/create_table.R

create_table_from_df <-
  function(conn,
           conn_fun = "pg13::local_connect()",
           schema,
           table_name,
           if_not_exists = TRUE,
           data,
           verbose = TRUE,
           render_sql = TRUE) {
    sql_statement <- draft_create_table_from_df(
      schema = schema,
      table_name = table_name,
      data = data,
      if_not_exists = if_not_exists
    )


    send(
      conn = conn,
      conn_fun = conn_fun,
      sql_statement = sql_statement,
      verbose = verbose,
      render_sql = render_sql
    )
  }
