#' uth_clean_student_id
#'
#' @param student_id A student id as a number, email, text with a leading d or D.
#'
#' @importFrom dplyr mutate if_else case_when everything
#' @importFrom stringr str_detect str_starts str_sub str_pad str_remove
#' @importFrom rlang sym
#'
#' @return A dataframe with student ids to manipulated into a character format without a leading "D", This can also check if a student id is a slate_id or a banner_id if it has leading alpha from nsc house tables
#' @export
#'
uth_clean_student_id <- function(student_id) {

  student_id <- {{student_id}}

  output_df <- tibble::tibble(
    student_id = student_id) %>%
    dplyr::mutate(student_id = ifelse(stringr::str_detect(student_id, "@"), stringr::str_sub(student_id,1,8), student_id)) %>%
    dplyr::mutate(student_id = dplyr::case_when(
      is.numeric(student_id) ~ as.character(student_id),
      is.character(student_id) & stringr::str_starts(student_id, "BO") ~ stringr::str_remove(student_id, "BO"),
      is.character(student_id) & stringr::str_starts(student_id, "D") ~ stringr::str_remove(student_id, "D"),
      is.character(student_id) & stringr::str_starts(student_id, "d") ~ stringr::str_remove(student_id, "d"),
      TRUE ~ as.character(student_id))) %>%
    dplyr::mutate(student_id = stringr::str_pad(student_id, 8, "left", "0")) %>%
    dplyr::select(student_id) %>%
    dplyr::pull(student_id)
}
