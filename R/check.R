






check_data_rows <-
        function(data) {

                if (nrow(data) == 0) {

                        cli::cli_alert_warning(text = "'data' has 0 rows")

                } else {

                        cli::cli_alert_success(text = "data")
                }
        }
