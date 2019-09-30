palm_ncdf_shiny   <- R6::R6Class("palm_ncdf_shiny",
                                 inherit = palm_ncdf_berlin,
                                 public = list(
                                   source = NULL,
                                   vardimensions = NULL,
                                   oldversion = NULL,
                                   plotcntr = 0,
                                   initialize = function(topofile, headclass, oldversion = FALSE, gui.arcgis = FALSE){
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
                                     ncfile <- nc_open(topofile)
                                     heightdata <- arcgischeck(
                                       ncvar_get(ncfile, "Band1"),
                                       self$arcgis)
                                     nc_close(ncfile)
                                     if(headclass$head$origin_z>0){
                                       heightdata <- heightdata - headclass$head$origin_z
                                     }
                                     if(all(heightdata>0)){
                                       heightdata  <- heightdata - min(heightdata)
                                     }
                                     if(any(heightdata<0)){
                                       heightdata  <- heightdata + min(heightdata)
                                     }


                                     heightdata     <- round(heightdata/basedistance)*basedistance
                                     ########
                                     # x and y dimension
                                     xvec         <- tryCatch({seq(basedistance*0.5, dim(heightdata)[1]*basedistance, by = basedistance)},
                                                              warning = function(w){
                                                                print("Warning in seq line 2388")
                                                              } , error = function(e){
                                                                print("Error in seq line 2388")
                                                              })
                                     yvec         <-tryCatch({ seq(basedistance*0.5, dim(heightdata)[2]*basedistance, by = basedistance)},
                                                             warning = function(w){
                                                               print("Warning in seq line 2393")
                                                             }, error = function(e){
                                                               print("Error in seq line 2393")
                                                             } )


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
                                                          "vals" = as.matrix(heightdata),
                                                          "type" = "float")
                                     if(self$oldversion){
                                       v.whichdim <- "orography_2D"
                                       dat$orography_2D  <- adata
                                     } else {
                                       v.whichdim <- "zt"
                                       dat$zt  <- adata
                                     }


                                     self$data <- dat
                                     whichdimension <- list()
                                     whichdimension[[v.whichdim]]  <- c(1,2)
                                     self$vardimensions  <- whichdimension

                                   },
                                   importfiles = function(...){
                                     print("This is not a supported function in this class.")
                                   },
                                   importbuildings_DUMMY = function(filepath){
                                     ncfile <- nc_open(filepath)
                                     build  <- arcgischeck(
                                       ncvar_get(ncfile, "Band1"),
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

                                   },
                                   getBuildingID = function(filepath, trustfile = FALSE){
                                     if(trustfile){
                                       ncfile <- nc_open(filepath)
                                       buildid  <- arcgischeck(
                                         ncvar_get(ncfile, "Band1"),
                                         self$arcgis)
                                       buildid[buildid==0] <- -9999.9
                                     } else if(!trustfile) {
                                       # buildid   <- array(-9999.9,dim(self$data$buildings_2d$vals))
                                       # countr <- 1
                                       ncfile <- nc_open(filepath)
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

                                   }
                                 )
)
