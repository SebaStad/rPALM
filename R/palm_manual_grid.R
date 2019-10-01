palm_ncdf_manual   <- R6::R6Class("palm_ncdf_manual",
                                  inherit = palm_ncdf_berlin,
                                  public = list(
                                    source = NULL,
                                    vardimensions = NULL,
                                    oldversion = NULL,
                                    plotcntr = 0,
#' R6-Class to create a new static driver from scratch. Creates the most important data arrays depending on the
#' grid definitions, given by the initialize/new function
#'
#' @param nx Numerical. Grid points in x direction
#' @param ny Numerical. Grid points in y direction
#' @param dx Numerical. Grid spacing
#' @param headclass R6-Class.
#' @param oldversion Logical. Only use TRUE for PALM versions <2900
#' @param gui.arcgis Logical. Set to TRUE, when you want to import rastered files created by ARCGIS.
#'
#' @return The initialize/new functions creates a R6 class, where most important arrays (vegetation_type, buildings_2d, etc)
#' are initialized with fill values.
#' @export
#'
#' @examples
#' manual_static <- palm_ncdf_manual$new(200,200,10, palm_headclass, FALSE, FALSE)
                                    initialize = function(nx, ny, dx, headclass, oldversion = FALSE, gui.arcgis = FALSE){
                                      if(oldversion){
                                        self$oldversion <- TRUE
                                      } else {
                                        self$oldversion <- FALSE
                                      }
                                      # Needs an own header file
                                      self$header         <- headclass
                                      basedistance        <- headclass$head$resolution
                                      self$arcgis         <- gui.arcgis
                                      # Same structure as berlin!
                                      dimen <- list()
                                      dat   <- list()
                                      whichdimension <- list()
                                      ######################
                                      topo_data <- array(0, c(nx,ny))
                                      fill_127  <- array(-127, c(nx,ny))
                                      fill_9999 <- array(-9999.9, c(nx,ny))

                                      ########
                                      # x and y dimension
                                      basedistance <- dx
                                      xvec         <- seq(dx*0.5, nx*dx, by = dx)
                                      yvec         <- seq(dx*0.5, ny*dx, by = dx)

                                      adata        <- list("long_name" = "x",
                                                           "standard_name" = "x",
                                                           "units" = "m",
                                                           "vals" = xvec)
                                      #"type" = "float")
                                      dimen$x      <- adata

                                      # Y
                                      adata        <- list("long_name" = "y",
                                                           "standard_name" = "y",
                                                           "units" = "m",
                                                           "vals" = yvec)
                                      #"type" = "float")
                                      dimen$y      <- adata

                                      self$dims <- dimen

                                      ###############
                                      # Orography
                                      adata        <- list("_FillValue" = -9999.9,
                                                           "units" = "m",
                                                           "long_name" = "terrain_height",
                                                           # "long_name" = "orography",
                                                           "res_orig" = basedistance,
                                                           "source" = "TBD",
                                                           "vals" = topo_data,
                                                           "type" = "float")
                                      if(self$oldversion){
                                        v.whichdim <- "orography_2D"
                                        dat$orography_2D  <- adata
                                      } else {
                                        v.whichdim <- "zt"
                                        dat$zt  <- adata
                                      }
                                      whichdimension[[v.whichdim]]  <- c(1,2)
                                      ######
                                      # Buildings
                                      # 2D

                                      adata    <- list("_FillValue" = -9999.9,
                                                       "units" = "m",
                                                       "long_name" = "building",
                                                       "res_origin" = dx,
                                                       "source" = "CityGML",
                                                       "lod"= 1,
                                                       "vals" = fill_9999,
                                                       "type" = "float")
                                      if(self$oldversion){
                                        dat$buildings_2D  <- adata
                                        v.whichdim <- "buildings_2D"
                                      } else {
                                        dat$buildings_2d  <- adata
                                        v.whichdim <- "buildings_2d"
                                      }
                                      whichdimension[[v.whichdim]]  <- c(1,2)


                                      # ID's

                                      adata     <- list("_FillValue" = -9999.9,
                                                        "units" = "",
                                                        # "long_name" = "building id",
                                                        "long_name" = "building id numbers",
                                                        "res_origin" = dx,
                                                        "source" = "",
                                                        "vals" = fill_9999,
                                                        "type" = "integer")

                                      dat$building_id  <- adata
                                      v.whichdim <- "building_id"
                                      whichdimension[[v.whichdim]]  <- c(1,2)

                                      # Type's
                                      adata      <- list("_FillValue" = -127,
                                                         "units" = "",
                                                         "long_name" = "building type classification",
                                                         # "long_name" = "building type",
                                                         "res_origin" = dx,
                                                         "source" = "",
                                                         "vals" = fill_127,
                                                         "type" = "integer")

                                      dat$building_type  <- adata
                                      v.whichdim <- "building_type"
                                      whichdimension[[v.whichdim]]  <- c(1,2)

                                      #####
                                      # Vegetation
                                      # Type


                                      adata      <- list("_FillValue" = -127,
                                                         "units" = "",
                                                         "long_name" = "vegetation_type",
                                                         "res_origin" = dx,
                                                         "source" = "DLR Satellite",
                                                         "vals" = fill_127,
                                                         "type" = "byte")

                                      dat$vegetation_type  <- adata
                                      v.whichdim <- "vegetation_type"
                                      whichdimension[[v.whichdim]]  <- c(1,2)
                                      #####
                                      # Water
                                      # Type

                                      adata      <- list("_FillValue" = -127,
                                                         "units" = "",
                                                         "long_name" = "water_type",
                                                         "source" = "DLR Satellite",
                                                         "vals" = fill_127,
                                                         "type" = "byte")

                                      dat$water_type  <- adata
                                      v.whichdim <- "water_type"
                                      whichdimension[[v.whichdim]]  <- c(1,2)

                                      #####
                                      # Pavement
                                      # Type

                                      adata      <- list("_FillValue" = -127,
                                                         "units" = "",
                                                         "long_name" = "pavement_type",
                                                         "source" = "OpenStreetMap",
                                                         "vals" = fill_127,
                                                         "type" = "byte")
                                      dat$pavement_type <- adata
                                      v.whichdim <- "pavement_type"
                                      whichdimension[[v.whichdim]]  <- c(1,2)



                                      self$data <- dat
                                      self$vardimensions  <- whichdimension

                                    },
                                    importfiles = function(...){
                                      print("This is not a supported function in this class.")
                                    },
                                    importbuildings_DUMMY = function(filepath){
                                      ncfile <- ncdf4::nc_open(filepath)
                                      build  <- arcgischeck(
                                        ncdf4::ncvar_get(ncfile, "Band1"),
                                        self$arcgis)
                                      dx <- self$header$head$resolution
                                      #### Fix für Gebäude < Raster!
                                      build[build<dx & build>0]  <- dx
                                      #
                                      build  <- floor(build/dx)*dx
                                      build[which(is.na(build),arr.ind = T)]  <- -9999.9
                                      build[build==0]   <- -9999.9

                                      adata    <- list("_FillValue" = -9999.9,
                                                       "units" = "m",
                                                       "long_name" = "building",
                                                       "res_origin" = dx,
                                                       "source" = "TBD",
                                                       "lod"= 1,
                                                       "vals" = build,
                                                       "type" = "float")
                                      if(self$oldversion){
                                        v.whichdim <- "buildings_2D"
                                        self$data$buildings_2D  <- adata
                                      } else {
                                        v.whichdim <- "buildings_2d"
                                        self$data$buildings_2d  <- adata
                                      }

                                      whichdimension <- list()
                                      whichdimension[[v.whichdim]]  <- c(1,2)
                                      # self$vardimensions  <- whichdimension


                                      buildtype  <- array(-127,dim=dim(build))
                                      buildtype[build>0]  <- 3
                                      adata      <- list("_FillValue" = -127,
                                                         "units" = "",
                                                         # "long_name" = "building type",
                                                         "long_name" = "building type classification",
                                                         "res_origin" = dx,
                                                         "source" = "",
                                                         "vals" = buildtype,
                                                         "type" = "byte")

                                      self$data$building_type  <- adata
                                      whichdimension[["building_type"]]  <- c(1,2)
                                      self$vardimensions  <- whichdimension
                                      ncdf4::nc_close(ncfile)

                                    },
                                    getBuildingID = function(filepath, trustfile = FALSE){
                                      if(trustfile){
                                        ncfile <- ncdf4::nc_open(filepath)
                                        buildid  <- arcgischeck(
                                          ncvar_get(ncfile, "Band1"),
                                          self$arcgis)
                                        buildid[buildid==0] <- -9999.9
                                      } else if(!trustfile) {
                                        # buildid   <- array(-9999.9,dim(self$data$buildings_2d$vals))
                                        # countr <- 1
                                        ncfile <- ncdf4::nc_open(filepath)
                                        buildid  <- arcgischeck(
                                          ncvar_get(ncfile, "Band1"),
                                          self$arcgis)
                                        buildid[buildid==0] <- -9999.9
                                        for(i in seq(dim(self$data$buildings_2d$vals)[1])){
                                          for(j in seq(dim(self$data$buildings_2d$vals)[2])){
                                            if(self$data$buildings_2d$vals[i,j]<0 && buildid[i,j] > 0){
                                              buildid[i,j]  <- -9999.9
                                            }
                                            if(self$data$buildings_2d$vals[i,j]>0 && buildid[i,j] < 0){
                                              buildid[i,j]  <- max(buildid) + 1
                                            }
                                          }
                                        }
                                      }

                                      adata     <- list("_FillValue" = -9999.9,
                                                        "units" = "",
                                                        "long_name" = "building id numbers",
                                                        # "long_name" = "building id",
                                                        "source" = "Munich Data",
                                                        "vals" = buildid,
                                                        "type" = "integer")

                                      self$data$building_id  <- adata
                                      whichdimension <- list()
                                      whichdimension[["building_id"]]  <- c(1,2)
                                      # self$vardimensions  <- whichdimension
                                      ncdf4::nc_close(ncfile)

                                    }
                                  )
)
