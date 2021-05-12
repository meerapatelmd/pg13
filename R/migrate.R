#' @title
#' Migrate Tables
#'
#' @description
#' Migrate Tables in one schema to another schema that does or does not belong to
#' another connection. The new schema can be of a different name, but the table names
#' remain the same.
#'
#' @param ... (optional) Character strings of names of tables to migrate.
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[secretary]{typewrite_warning}},\code{\link[secretary]{c("typewrite", "typewrite")}},\code{\link[secretary]{typewrite_progress}},\code{\link[secretary]{character(0)}}
#'  \code{\link[purrr]{map}}
#' @rdname migrate
#' @export
#' @importFrom rlang list2
#' @importFrom secretary typewrite_warning typewrite typewrite_progress enbold
#' @importFrom purrr map


migrate <-
  function(conn_1,
           schema_1,
           ...,
           conn_2,
           schema_2,
           drop_existing = TRUE,
           verbose = TRUE,
           render_sql = TRUE) {
    Tables <- ls_tables(
      conn = conn_1,
      schema = schema_1,
      verbose = verbose,
      render_sql = render_sql
    )

    if (missing(...)) {
      target_tables <- Tables
    } else {
      args <- rlang::list2(...)
      args <- unlist(args)

      target_tables <- args[args %in% Tables]

      if (length(target_tables) != length(args)) {
        secretary::typewrite_warning("Not all tables supplied as arguments were found:")
        target_tables %>%
          purrr::map(~ secretary::typewrite(., tabs = 4, timepunched = FALSE))
      }
    }

    for (i in seq_along(target_tables)) {
      secretary::typewrite_progress(
        iteration = i,
        total = length(target_tables)
      )

      secretary::typewrite(secretary::enbold("Table:"), target_tables[i])

      x <- read_table(
        conn = conn_1,
        schema = schema_1,
        table = target_tables[i],
        verbose = verbose,
        render_sql = render_sql
      )

      write_table(
        conn = conn_2,
        schema = schema_2,
        table_name = target_tables[i],
        data = x,
        drop_existing = drop_existing
      )
    }
  }
