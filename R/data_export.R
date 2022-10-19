
#' Save Data as File
#'
#' @details
#' ** Saves data to local machine. **
#'
#' The default location for the file to be saved is in a folder called "sensitive".
#' If this folder does not exist, this function will not work.
#'
#' @param input_df A Data Frame. Note: can contain any arbitrary data fields.
#' @param file_name The name of the file that will be saved (e.g. "my_data.txt").
#' @param delim A character that will be placed between data fields, in the output file (e.g. "|").
#' @param with_header Boolean. Will determine if the column names of input_df will be included in the output file.
#' @param quote Boolean. If TRUE will wrap each data column in double quotes.
#'
#' @importFrom utils write.table
#'
#' @return Returns nothing.
#' @export
#'
save_data_as_file <- function(input_df, file_name, delim="|", with_header=FALSE, quote=FALSE) {
  file_location <- here::here("sensitive", file_name)
  write.table( input_df,
               file = file_location,
               sep = delim,
               na = "",
               row.names = FALSE,
               col.names = with_header,
               quote = quote )
  return( NULL )
}


#' Save Data as Pin
#'
#' @param input_df A Data Frame. Note: can contain any arbitrary data fields.
#' @param pin_name The name of the pin to be saved.
#'
#' @return Returns nothing.
#' @export
#'
save_data_as_pin <- function(input_df, pin_name) {
  pins_board <- get_pins_connection_object()
  pins::pin_write(pins_board, input_df, pin_name)
  return( NULL )
}
