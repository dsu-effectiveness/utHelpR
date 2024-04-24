#' uth_update_utpackages
#'
#' @param utpackage A package on made by Utah Tech Univeristy housed on a Github repo
#'
#' @importFrom devtools install_github
#' @importFrom gitcreds gitcreds_get
#' @return A package made by Utah Tech Univeristy housed on a Github
#' @export
#'
#'
uth_update_utpackages <- function(utpackage){
  if (utpackage == "utMetrics"){
    devtools::install_github("ut-effectiveness/utMetrics", auth_token = gitcreds::gitcreds_get()$password, force = TRUE, build_vignettes = TRUE)
  } else if (utpackage == "utHelpR") {
    devtools::install_github("dsu-effectiveness/utHelpR")
  } else if (utpackage == "utDataStor") {
    devtools::install_github("dsu-effectiveness/utDataStoR", build_vignettes = TRUE)
  } else if (utpackage == "usheUtils") {
    devtools::install_github("dsu-effectiveness/usheUtils")
  } else {
    print("Here is a list of current packages: utHelpR, utMetrics, utDataStor, usheUtils")
  }
  print("Remember to restart your R session once the package has downloaded")
}
