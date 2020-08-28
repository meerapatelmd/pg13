#' @title
#' Render GRANT ALL PRIVILEGES
#' @description
#' Render SQL to grant all privileges to a schema a user or a group
#' @param schema schema to grant privileges to
#' @param group group name, Default: NULL
#' @param user user name, Default: NULL
#' @seealso
#'  \code{\link[SqlRender]{render}},\code{\link[SqlRender]{readSql}}
#' @rdname renderGrantSchema
#' @export
#' @importFrom SqlRender render readSql

renderGrantSchema <-
        function(schema,
                 group = NULL,
                 user = NULL) {

                if (is.null(group) && is.null(user)) {
                        stop("group or user is required")
                }

                base <- system.file(package='pg13')
                path <- paste0(base, "/sql")

                if (!is.null(group)) {
                        SqlRender::render(SqlRender::readSql(paste0(path, "/grantGroup.sql")),
                                          schema = schema,
                                          gp = group)
                } else {
                        SqlRender::render(SqlRender::readSql(paste0(path, "/grantUser.sql")),
                                          schema = schema,
                                          user = user)
                }

        }
