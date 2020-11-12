#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param conn PARAM_DESCRIPTION
#' @param output.var PARAM_DESCRIPTION
#' @param verbose PARAM_DESCRIPTION, Default: TRUE
#' @param progressBar PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{reexports}}
#'  \code{\link[progress]{progress_bar}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[secretary]{typewrite_bold}}
#' @rdname summarizeDB
#' @export
#' @importFrom purrr map
#' @importFrom dplyr bind_rows select distinct mutate everything
#' @importFrom progress progress_bar
#' @importFrom tidyr pivot_wider
#' @importFrom secretary typewrite_bold

summarizeDB <-
        function(conn,
                 output.var,
                 verbose = TRUE,
                 progressBar = TRUE) {

                schemas <- lsSchema(conn = conn)
                schemas <-
                        grep("^pg|information_schema", schemas, value = TRUE, invert = TRUE)

                sqlList <-
                        schemas %>%
                                purrr::map(renderInfoSchemaCols)


                schemaInfo <-
                        queryList(conn = conn,
                                  sqlList = sqlList,
                                  verbose = verbose,
                                  progressBar = FALSE)

                schemaInfo <- dplyr::bind_rows(schemaInfo)



                schemaInfo <-
                        schemaInfo %>%
                        dplyr::select(table_schema,
                                      table_name,
                                      column_name) %>%
                        dplyr::distinct()


                if (progressBar) {


                        pb <- progress::progress_bar$new(format = "[:bar] :elapsed :current/:total :percent",
                                                         total = nrow(schemaInfo))


                        pb$tick(0)
                        Sys.sleep(0.2)

                }


                output <- list()
                for (i in 1:nrow(schemaInfo)) {

                        schema <- schemaInfo$table_schema[i]
                        tableName <- schemaInfo$table_name[i]
                        fieldName <- schemaInfo$column_name[i]


                        if (progressBar) {

                                pb$tick()
                                Sys.sleep(0.2)

                        }


                        output[[i]] <-
                                list(Total_Rows =
                                        renderRowCount(schema = schema,
                                                       distinct = TRUE,
                                                       tableName = tableName),
                                     Distinct_Field_Value =
                                         renderRowCount(fields = fieldName,
                                                        distinct = TRUE,
                                                        schema = schema,
                                                        tableName = tableName)) %>%
                                        purrr::map(~query(conn = conn,
                                                          sql_statement = .)) %>%
                                        dplyr::bind_rows(.id = "Variable") %>%
                                        dplyr::mutate(Schema = schema,
                                                         Table = tableName,
                                                         Field = fieldName) %>%
                                        dplyr::select(Schema,
                                                      Table,
                                                      Field,
                                                      Variable,
                                                         dplyr::everything())


                        assign(output.var, output, envir = parent.frame())

                }

                output <-
                        output %>%
                        dplyr::bind_rows() %>%
                        tidyr::pivot_wider(names_from = Variable,
                                           values_from = count)

                assign(output.var, output, envir = parent.frame())

                if (verbose) {
                        secretary::typewrite_bold("Complete.")
                }

        }







