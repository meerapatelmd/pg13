.onLoad <-
  function(lib, pkg) {
    safely.print <<- purrr::safely(print,
      quiet = TRUE
    )
    quietly.safely.print <<- purrr::quietly(safely.print)


    is_conn_open <<-
      function(conn) {
        output <- quietly.safely.print(conn)

        if (is.null(output$result$error)) {
          TRUE
        } else {
          FALSE
        }
      }


    quietly.conn_db <<- purrr::quietly(conn_db)

    connect <<-
      function(user,
               password,
               port,
               server,
               extraSettings = NULL,
               oracleDriver = "thin",
               connectionString = NULL,
               pathToDriver = system.file(package = "pg13", "driver"),
               verbose = TRUE) {
        conn <- quietly.conn_db(
          user = user,
          password = password,
          port = port,
          server = server,
          extraSettings = extraSettings,
          oracleDriver = oracleDriver,
          connectionString = connectionString,
          pathToDriver = pathToDriver
        )

        if (verbose) {
          db_name <- conn@jConnection$getCatalog()

          secretary::typewrite(sprintf("%s to %s", conn$output, db_name))
        }

        conn$result
      }


    local_connect <<-
      function(dbname = "athena",
               port = 5432,
               user = NULL,
               password = NULL,
               extraSettings = NULL,
               oracleDriver = "thin",
               connectionString = NULL,
               pathToDriver = system.file(package = "pg13", "driver"),
               verbose = TRUE) {
        server <- sprintf("localhost/%s", dbname)

        conn <- quietly.conn_db(
          user = user,
          password = password,
          port = port,
          server = server,
          extraSettings = extraSettings,
          oracleDriver = oracleDriver,
          connectionString = connectionString,
          pathToDriver = pathToDriver
        )

        if (verbose) {
          dbname <- conn$result@jConnection$getCatalog()

          secretary::typewrite(sprintf("Connected to '%s'", dbname))
        }

        conn$result
      }




    connect_ff <<-
      function(user,
               password,
               port,
               server) {
        function(verbose = TRUE) {
          connect(
            user = user,
            password = password,
            port = port,
            server = server,
            verbose = verbose
          )
        }
      }
  }
