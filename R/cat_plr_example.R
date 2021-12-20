#' @title
#' Print an Example of a Postgres Function using PLR
#' @family PLR
#' @rdname cat_plr_example
#' @export

cat_plr_example <-
        function() {

                cat(
                "
                CREATE OR REPLACE FUNCTION setup_rxmap(mth_version varchar, mth_release_dt varchar)
                RETURNS void
                AS '
                library(metathesaurus)
                setup_rxmap(mth_version = mth_version,mth_release_dt = mth_release_dt)
                ' LANGUAGE plr;


                SELECT setup_rxmap('{mth_version}', '{mth_release_dt}');
                "
                )

        }
