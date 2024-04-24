#' Set up a database connection
#'
#' @param dsn A string of text representing the name of a DSN entry
#'
#' @return either a DBMS connection object, or an ODBC connection object, from DBI::dbConnect
#' @export
#'
#' @importFrom DBI dbCanConnect
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @importFrom RPostgres Postgres
#' @importFrom keyring key_get
#'

get_connection_object <- function(dsn) {
  # Server-side db connection with RStudio Connect ####
  # These conditionals should fail to trigger on a personal machine,
  # as the username and password should not be stored in a DNS entry on any personal machine.
  # But these conditionals should trigger on a server.
  # The motivation: It is secure to store this information in a server's DSN entry,
  # but NOT secure to store on a personal machine's DSN entry.
  if ( DBI::dbCanConnect(odbc::odbc(), DSN=dsn) ) {
    conn <- DBI::dbConnect(odbc::odbc(), DSN=dsn)
  }
  # Local db connection ####
  # These should trigger on a personal machine,
  # assuming all other dependent data io infrastructure is set up properly.
  else if ( dsn == "edify" ) {
    conn <- DBI::dbConnect( RPostgres::Postgres(),
                       dbname="analytics",
                       host="utahtech.db.edh.eab.com",
                       port=51071,
                       user=key_get("edify", "username"),
                       password=key_get("edify", "password") )
  }
  else {
    conn <- DBI::dbConnect( odbc::odbc(),
                       DSN=dsn,
                       UID=key_get("sis_db", "username"),
                       PWD=key_get("sis_db", "password") )
  }
  return(conn)
}
