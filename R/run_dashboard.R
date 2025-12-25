#' Launch GBG Dashboard
#'
#' Launch the GBG epidemiological dashboard Shiny application
#'
#' @export
#' @importFrom shiny runApp
#' @examples
#' \dontrun{
#' run_dashboard()
#' }
run_dashboard <- function() {
  appDir <- system.file("shiny", package = "gbg")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing `gbg`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
