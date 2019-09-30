palm_global <- R6::R6Class("palm_global", public= list(
  head = NULL,
  #' Create global parameters for static driver
  #'
  #' Create an R6 class that contains all global attributes needed
  #' in the static driver.
  #'
  #' @param title Title of the file
  #' @param author Author, i.e. you
  #' @param institute Your institute
  #' @param location Site of the static driver
  #' @param x0 x-coordinate of the lower left point in your coordinate system
  #' @param y0 y-coordinate of the lower left point in your coordinate system
  #' @param z0 lowest z-coordinate in your domain
  #' @param t0 good question
  #' @param lat Latitude of the lower left point of your domain
  #' @param lon Longitude of the lower left point of your domain
  #' @param dx Grid spacing
  #'
  #' @return Creates the PALM-Header Class
  #' @export
  #'
  #' @examples
  #' palm_global$new('Testglobal', 'Max Mustermann', 'Some Institute',
  #' 'Somewehere', 1, 1, 0, 0, 33,33, 10)
  initialize = function(title, author, institute, location, x0, y0, z0, t0, lat, lon, dx = 5){
    ncdflist = list(
      "Conventions" = "CF-1.7",
      "data_content" = "",
      "source" = "",
      "Version" = "1.0",
      "dependencies" = "",
      "history" = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "UTC"),
      "keywords" = "",
      "campaign" = "UseUClim",
      "creation_time" = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "UTC"),
      "title" = title,
      "acronym" = "",
      "institution" = institute,
      "author" = author,
      "contact_person" = author,
      "licence" = "Text",
      "origin_time" = t0,
      "location" = location,
      "site" = "",
      "origin_x" = x0,
      "origin_y" = y0,
      "origin_z" = z0,
      "resolution" = dx,
      "origin_lat" = lat,
      "origin_lon" = lon,
      "rotation_angle" = 0.0,
      "references" = "Maronga et al. (2015, GMD) - full description of PALM 4.0",
      "Comment" = "",
      "palm_version" = 6.0
    )
    self$head <- ncdflist
  },
  changeVar = function(variable, input){
    self$head[[variable]]  <- input
  },
  changeTimeVar = function(variable, input){
    # not yet implemented
    self$head   <- self$head
  }
)
)
