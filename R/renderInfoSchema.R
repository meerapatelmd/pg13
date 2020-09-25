#' @title
#' Render SQL that returns Column Information
#' @description FUNCTION_DESCRIPTION
#' @param schema PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname renderInfoSchemaCols
#' @export
#' @importFrom SqlRender render



renderInfoSchemaCols <-
        function(schema) {

                SqlRender::render(
                                  "
                                  SELECT *
                                  FROM information_schema.columns
                                  WHERE table_schema = '@schema'
                                  ;
                                  ",
                                  schema = schema)
        }
