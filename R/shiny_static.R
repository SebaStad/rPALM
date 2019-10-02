#' Title
#'
#' @return Opens the static driver creation shiny App
#' @export
#'
#' @examples
#' runExample()
shiny_static <- function() {
  require(shiny)
  require(shinyTree)
  require(DT)

  appDir <- system.file("shiny-exam/static_generator", package = "rPALM")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `rPALM`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
