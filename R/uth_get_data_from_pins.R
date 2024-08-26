#' get_data_from_pin_prod
#' A helper function for get_data_from_pins()
#'
#' @param pin_name a pin already hosted on one of the Posit Servers
#'
#' @return data stored as a pin
#' @importFrom pins pin_read
#'
get_data_from_pin_prod <- function(pin_name){

  server_choice <- "https://connect.ie.utahtech.edu"

  api_key <- Sys.getenv("RSCONNECT_SERVICE_USER_API_KEY")
  rsconnect <- pins::board_connect(key =api_key, server = server_choice)
  output_df <- pins::pin_read(pin_name, board = rsconnect)

  return(output_df)
}

#' get_data_from_pin_test
#' A helper function for get_data_from_pins()
#'
#' @param pin_name a pin already hosted on one of the Posit Servers
#'
#' @return data stored as a pin
#'
get_data_from_pin_test <- function(pin_name){

  server_choice <- "https://connect.test.ie.utahtech.edu"

  api_key <- Sys.getenv("RSCONNECT_SERVICE_USER_API_KEY")
  rsconnect <- pins::board_connect(key =api_key, server = server_choice)
  output_df <- pins::pin_read(pin_name, board = rsconnect)

  return(output_df)
}

#' get_data_from_pin_local
#' A helper function for get_data_from_pins()
#'
#' @param pin_name a pin already hosted on one of the Posit Servers
#' @param prod defaulted to true, is for the user to pick which server to pull pins from locally. If false it will pull pins from the test server
#'
#' @return data stored as a pin
#' @importFrom pins pin_read
#' @importFrom keyring key_get
#'
get_data_from_pin_local <- function(pin_name, prod=TRUE) {

  if(prod == TRUE){
    server_choice <- "https://connect.ie.utahtech.edu"
    api_key <- keyring::key_get("pins", "api_key")

  } else {
    server_choice <- "https://connect.test.ie.utahtech.edu"
    api_key <- keyring::key_get("pins_test", "api_key")
  }

  rsconnect <- pins::board_connect(key=api_key, server=server_choice)
  df <- pins::pin_read(pin_name, board=rsconnect) %>%
    mung_dataframe()
  return(df)
}


#' get_data_from_pins
#'
#' @param pin_name a pin already hosted on one of the Posit Servers
#' @param prod defaulted to true, is for the user to pick which server to pull pins from locally. If false it will pull pins from the test server
#'
#' @return data stored as a pin
#' @export
#'
get_data_from_pins <- function(pin_name, prod=TRUE){

  test_data <- tryCatch(get_data_from_pin_prod(pin_name),
                        error=function(e) get_data_from_pin_test(pin_name),
                        error=function(e) get_data_from_pin_local(pin_name, prod)
  )

  return(test_data)
}
