#' Munge dataframe
#'
#' @param df a dataframe you want to clean.
#'
#' @return a clean dataframe
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
mung_dataframe <- function(df) {
  output_df <- df %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    janitor::clean_names() %>%
    tibble::as_tibble()
  return(output_df)
}

#' Get data from a sql file
#'
#' @param file_name The name of the SQL file you want to use.
#' @param dsn A DSN entry: edify, PROD, REPT, BRPT, etc...
#' @param context The R project context in which this SQL lives. Current options are "project", "shiny", and "sandbox".
#'
#' @return a dataframe containing the results of your sql query
#' @export
#'
get_data_from_sql_file <- function(file_name, dsn, context='project') {

  conn <- get_db_connection_object(dsn)

  # capture query
  if (context == 'project') {
    query <- readr::read_file( here::here('sql', file_name) )
  } else if (context == 'shiny') {
    query <- readr::read_file(here::here('inst', 'sql', file_name) )
  } else if (context == 'sandbox') {
    query <- readr::read_file(here::here('sandbox', 'sql', file_name) )
  } else {
    query <- readr::read_file( file_name )
  }

  df <- DBI::dbGetQuery(conn, query) %>%
    mung_dataframe()
  return(df)
}

#' Get data from a pin stored in RStudio Connect
#'
#' @param pin_name The name of the pin, as stored on RStudio Connect instance.
#'
#'
#' @return Data from the requested pin.
#' @export
#'
#'
get_data_from_pin <- function(pin_name) {
  pins_board <- get_pins_connection_object()
  # pull data from the pin
  pin_data <- pins::pin_read(pins_board, pin_name) %>%
    mung_dataframe()
  return(pin_data)
}

#' Get data from a rds source.
#'
#' @param file_name The rds file you want to load
#'
#' @return a dataframe containing the data from the .rds file
#' @export
#'
#'
load_data_from_rds <- function(file_name) {
  df <- readRDS( here::here('data', file_name) ) %>%
    mung_dataframe()
  return(df)
}

#' Load data from an Excel spreadsheet formatted file
#'
#' @param file_name the name of the file, assumed to be located in a directory called 'data'
#'
#'
#' @return A dataframe with all content of the Excel spreadsheet.
#' @export
#'
#'
load_data_from_xlsx <- function(file_name) {
  df <- readxl::read_excel( here::here('data', file_name) ) %>%
    mung_dataframe()
  return(df)
}

#' Load data from a CSV formatted file
#'
#' @param file_name the name of the file, assumed to be located in a directory called 'data'
#'
#'
#' @return A dataframe with all content of the CSV file.
#' @export
#'
#'
load_data_from_csv <- function(file_name) {
  df <- utils::read.csv( here::here('data', file_name) ) %>%
    mung_dataframe()
  return(df)
}
