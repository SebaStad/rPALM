palm_ncdf_dimension_template <- R6::R6Class("palm_ncdf_dimension_template",
                                              public = list(
                                              dim_list = list(),
#' Title
#'
#' @param longname      Longname of the Dimension
#' @param standardname  Standardname of the Dimension, also used as VarName in ncdf!
#' @param units         Units of the Dimensions
#' @param vals          Values of the Dimensions
#'
#' @return A R6 Class, that resembles a Dimension in ncdf!
#' @export
#'
#' @examples
#' x <- 1
                                                initialize = function(longname,
                                                                      standardname,
                                                                      units,
                                                                      vals){
                                                  dim       <- list("long_name" = longname,
                                                                       "standard_name" = standardname,
                                                                       "units" = units,
                                                                       "vals" = vals)
                                                  self$dim_list[[standardname]] <- dim
                                                },
                                                add_another_dim = function(longname,
                                                                           standardname,
                                                                           units,
                                                                           vals){
                                                  dim       <- list("long_name" = longname,
                                                                    "standard_name" = standardname,
                                                                    "units" = units,
                                                                    "vals" = vals)
                                                  self$dim_list[[standardname]] <- dim
                                                },
                                                print = function(){
                                                  cat("yadayada")
                                                }
                                              )
)
