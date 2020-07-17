#' Writes a DROP TABLE and CREATE TABLE sql statement for a given dataframe
#' @param dataframe dataframe to be loaded into table
#' @param to_table_name name of the table that the dataframe will be loaded into
#' @param add_to_varchar integer that is added to the character count for good measure.
#' @import purrr
#' @import dplyr
#' @export


write_sql_to_drop_create_table_from_df <-
        function(dataframe, to_table_name, add_to_varchar = 10) {
                field_types <-  paste(colnames(dataframe),
                                      colnames(dataframe) %>%
                                              purrr::map(function(x) add_to_varchar +
                                                             (dataframe %>%
                                                                 dplyr::select(all_of(x)) %>%
                                                                 unlist() %>%
                                                                 nchar() %>%
                                                                 max(na.rm = TRUE))) %>%
                                              purrr::map(function(x) paste0("varchar(", x, ")")) %>%
                                              purrr::set_names(colnames(dataframe)) %>%
                                              unlist(),
                                      collapse = ",\n\t")

                sql_statement <- paste0("DROP TABLE IF EXISTS ", to_table_name, ";\n",
                                        "CREATE TABLE ", to_table_name, " (\n\t",
                                        field_types, "\n);\n\n")
                return(sql_statement)
        }
