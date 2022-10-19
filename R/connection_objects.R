#' Get Database Connection Object
#'
#' @param dsn A string of text representing the name of a DSN entry
#'
#' @return either a DBMS connection object, or an ODBC connection object, from DBI::dbConnect
#' @export
#'
get_db_connection_object <- function(dsn) {
  # Server-side db connection with RStudio Connect ####
  # These conditionals should fail to trigger on a personal machine,
  # as the username and password should not be stored in a DNS entry on any personal machine.
  # But these conditionals should trigger on a server.
  # The motivation: It is secure to store this information in a server's DSN entry,
  # but NOT secure to store on a personal machine's DSN entry.
  if ( DBI::dbCanConnect(odbc::odbc(), DSN=dsn) ) {
    conn <- DBI::dbConnect(odbc::odbc(), DSN=dsn)
  }
  else if ( DBI::dbCanConnect(RPostgres::Postgres(), DSN=dsn) ) {
    conn <- DBI::dbConnect(RPostgres::Postgres(), DSN=dsn)
  }
  # Local db connection ####
  # These should trigger on a personal machine,
  # assuming all other dependent data io infrastructure is set up properly.
  else if ( dsn == "edify" ) {
    conn <- DBI::dbConnect( RPostgres::Postgres(),
                            dbname="analytics",
                            host="utahtech.db.edh.eab.com",
                            port=51071,
                            user=keyring::key_get("edify", "username"),
                            password=keyring::key_get("edify", "password") )
  }
  else {
    conn <- DBI::dbConnect( odbc::odbc(),
                            DSN=dsn,
                            UID=keyring::key_get("sis_db", "username"),
                            PWD=keyring::key_get("sis_db", "password") )
  }
  return(conn)
}


#' Get Pins Connection Object
#'
#' @return A connection object to read and write pins to on RStudio Connect
#' @export
#'
get_pins_connection_object <- function() {
  # Obtain the API key from environment variable.
  api_key <- Sys.getenv("RSCONNECT_SERVICE_USER_API_KEY")
  # If API key is not available as environment variable, use keyring entry.
  # NOTE: The API key should only be an environment variable on the server
  #       For local machines, set a keyring entry.
  if (api_key == "") {
    api_key <- keyring::key_get("pins", "api_key")
  }
  # Register the connection to the pinning board.
  pins_board <- pins::board_rsconnect(auth="manual",
                                      account="rsconnectapi!service",
                                      server="https://rs-connect.utahtech.edu",
                                      key=api_key)
  return(pins_board)
}
