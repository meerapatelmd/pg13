#' @title
#' Is PLR Installed?
#'
#' @rdname is_plr_installed
#' @family PLR
#' @export

is_plr_installed <-
function(conn,
         conn_fun = "pg13::local_connect()",
         verbose = TRUE,
         render_sql = TRUE,
         render_only = FALSE,
         checks = "") {


        extensions <-
        ls_extensions(conn = conn,
                      conn_fun = conn_fun,
                      verbose = verbose,
                      render_sql = render_sql,
                      render_only = render_only,
                      checks = checks)


        "plr" %in% extensions


}
