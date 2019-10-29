#' Title
#'
#' @return Opens the static driver creation shiny App
#' @export
#'
#' @examples
#' runExample()
shiny_static <- function(provideDir = NULL) {
  require(shiny)
  require(shinyTree)
  require(DT)

  appDir <- system.file("shiny-exam/static_generator", package = "rPALM")
  if (appDir == "" && is.null(provideDir)) {
    stop("Could not find example directory. Try re-installing `rPALM`.", call. = FALSE)
  }
  if(!is.null(provideDir)){
    appDir <- paste(provideDir,"shiny-exam/static_generator",sep="/")
  }

  shiny::runApp(appDir, display.mode = "normal")
}
