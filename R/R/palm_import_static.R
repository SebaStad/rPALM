palm_ncdf_import   <- R6::R6Class("palm_ncdf_import",
                                  inherit = palm_ncdf_berlin,
                                  public = list(
                                    source = NULL,
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
                                      ncdf4::nc_close(ncfile)
                                    }

                                  )
)
