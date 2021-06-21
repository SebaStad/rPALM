require(ncdf4)
require(R6)
palm_ncdf_import   <- R6::R6Class("palm_ncdf_import",
                                  inherit = palm_ncdf_berlin,
                                  public = list(
                                    source = NULL,
#' R6-Class to import data from an already existing static driver
#'
#' @param newfilename Character. Filename for the future static driver
#' @param pathtofiles Character. Path where the old static driver lies
#' @param oldfilename Character. Name of the old static driver
#' @param oldversion Logical. Only use TRUE for PALM versions <2900
#'
#' @return The initialize/new function creates a barebones class with references to the static driver.
#' To import the data of the static file, read_static() has to be called afterwards
#' @export
#'
#' @examples
#' import_static <- palm_ncdf_import$new("new_static_driver.nc",
#'                                       "Path/to/files", "old_static_driver.nc", F)
#' import_static$read_static()
                                    initialize = function(newfilename, pathtofiles, oldfilename, oldversion = TRUE){
                                      if(substrRight(newfilename,3)!=".nc"){
                                        newfilename <- paste(newfilename,".nc",sep="")
                                      }
                                      self$exportname     <- newfilename
                                      self$path           <- pathtofiles
                                      self$source         <- oldfilename
                                      self$plotcntr       <- 0
                                      self$oldversion     <- oldversion
                                    },
                                    importfiles = function(...){
                                      print("This is not a supported function in this class.")
                                    },
                                    read_static = function(...){
                                      setwd(self$path)
                                      ncfile  <- ncdf4::nc_open(self$source)


                                      #####
                                      # Recreate Header Class for Global Attributes
                                      globattr <- ncdf4::ncatt_get(ncfile,0)
                                      dummy_obj <- palm_global$new(title     = globattr$title,
                                                                   author    = globattr$author,
                                                                   institute = globattr$institution,
                                                                   location  = globattr$location,
                                                                   x0        = globattr$origin_x,
                                                                   y0        = globattr$origin_y,
                                                                   z0        = globattr$origin_z,
                                                                   t0        = globattr$origin_time,
                                                                   lat       = globattr$origin_lat,
                                                                   lon       = globattr$origin_lon)
                                      for(tt in names(dummy_obj$head)){
                                        dummy_obj$head[[which(names(dummy_obj$head)==tt)]]  <- ncdf4::ncatt_get(ncfile,0,tt)$value
                                      }
                                      self$header  <- dummy_obj

                                      #####
                                      # Get Dimensions
                                      # x
                                      dimen        <- list()

                                      for(zz in seq(ncfile$ndims)){
                                        vec         <- ncdf4::ncvar_get(ncfile, names(ncfile$dim)[zz])
                                        attr        <- ncdf4::ncatt_get(ncfile, names(ncfile$dim)[zz])
                                        adata <- list()
                                        for(ii in seq(attr)){
                                          adata[[names(attr)[ii]]]   <-attr[[ii]]
                                        }
                                        adata[["vals"]]= vec

                                        dimen[[names(ncfile$dim)[zz]]]     <- adata

                                      }
                                      self$dims  <- dimen

                                      dat <- list()
                                      whichdimensions <- list()

                                      for(zz in seq(ncfile$nvars)){
                                        vec         <- ncdf4::ncvar_get(ncfile, names(ncfile$var)[zz])
                                        attr        <- ncdf4::ncatt_get(ncfile, names(ncfile$var)[zz])
                                        vec[is.na(vec)]  <- attr$`_FillValue`
                                        adata        <- list("_FillValue" = attr$`_FillValue`,
                                                             "units" = attr$units,
                                                             "long_name" = attr$long_name,
                                                             "name" = names(ncfile$var)[zz],
                                                             "source" = attr$source,
                                                             "vals" = vec,
                                                             "type" = ncfile$var[[names(ncfile$var)[zz]]]$prec,
                                                             "res_orig" = attr$res_orig,
                                                             "lod" = attr$lod)
                                        dat[[names(ncfile$var)[zz]]]     <- adata
                                        whichdimensions[[names(ncfile$var)[zz]]]  <- ncfile$var[[names(ncfile$var)[zz]]]$dimids + 1

                                      }
                                      self$data  <- dat
                                      self$vardimensions  <- whichdimensions
                                      self$header$head$rotation_angle <- as.numeric(self$header$head$rotation_angle)
                                      self$header$head$resolution <- abs(self$dims$x$vals[1] - self$dims$x$vals[2])
                                      ncdf4::nc_close(ncfile)

                                      for(i in names(self$data)){
                                        if(is.null(self$data[[i]]$long_name)){
                                          print(glue::glue("Fixing long_name for {i}"))
                                          self$data[[i]]$long_name <- i
                                        }

                                      }

                                    }

                                  )
)
