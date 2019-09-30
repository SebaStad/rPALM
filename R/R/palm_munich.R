palm_ncdf_munich   <- R6::R6Class("palm_ncdf_munich",
                                  inherit = palm_ncdf_berlin,
                                  public = list(
                                    source = NULL,
                                    vardimensions = NULL,
                                    oldversion = NULL,
                                    initialize = function(pathtodgmfiles, headclass, oldversion = FALSE){
                                      if(oldversion){
                                        self$oldversion <- TRUE
                                      } else {
                                        self$oldversion <- FALSE
                                      }
                                      # Needs an own header file
                                      self$header         <- headclass

                                      # Same structure as berlin!
                                      dimen <- list()
                                      dat   <- list()

                                      # Read in dgm text files
                                      setwd(pathtodgmfiles)
                                      dgmdata <- lapply(X = list.files(pattern="dgm"), function(x){
                                        read.table(x,header = F, dec = ".")
                                      } )
                                      xvar <- c()
                                      yvar <- c()
                                      zcomp <- c()

                                      for(i in seq(length(dgmdata))){
                                        xvar <- c(xvar, dgmdata[[i]][,1])
                                        yvar <- c(yvar, dgmdata[[i]][,2])
                                        zcomp <- c(zcomp, dgmdata[[i]][,3])

                                      }

                                      dgmmelt <- data.frame("xvar" = xvar,
                                                            "yvar" = yvar,
                                                            "height" = zcomp)
                                      basedistance <- abs(dgmmelt[1,1] - dgmmelt[2,1])
                                      baseheight   <- min(dgmmelt$height)

                                      dgmmelt$floorheight <- dgmmelt$height - baseheight
                                      dgmmelt$rasterheight <- round(dgmmelt$floorheight/basedistance)*basedistance

                                      self$header$head$resolution <- basedistance

                                      refpoint <- data.frame("x_0" = min(dgmmelt$xvar),
                                                             "y_0" = min(dgmmelt$yvar))

                                      self$header$head$origin_x <- as.numeric(refpoint[1])
                                      self$header$head$origin_y <- as.numeric(refpoint[2])
                                      self$header$head$origin_z <- baseheight

                                      sputm <- SpatialPoints(refpoint, proj4string=CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m ") )
                                      spgeo <- spTransform(sputm, CRS("+proj=longlat"))

                                      # self$header$head$origin_lat  <- as.numeric(spgeo$x_0)
                                      # self$header$head$origin_lon  <- as.numeric(spgeo$y_0)


                                      dcastdata <- data.frame("xvar" = dgmmelt$xvar,
                                                              "yvar" = dgmmelt$yvar,
                                                              "rasterheight" = dgmmelt$rasterheight)
                                      heightdata <- dcast(data= dcastdata, dcastdata$xvar~dcastdata$yvar, value.var = "rasterheight", fun.aggregate = mean)
                                      heightdata <- heightdata[,2:dim(heightdata)[2]]



                                      ########
                                      # x and y dimension
                                      xvec         <- tryCatch({seq(basedistance*0.5, dim(heightdata)[1]*basedistance, by = basedistance)},
                                                               warning = function(w){
                                                                 print("Warning in seq line 2095")
                                                               }, error = function(e){
                                                                 print("Error in seq line 2095")
                                                               } )
                                      yvec         <- tryCatch({seq(basedistance*0.5, dim(heightdata)[2]*basedistance, by = basedistance)},
                                                               warning = function(w){
                                                                 print("Warning in seq line 2100")
                                                               }, error = function(e){
                                                                 print("Error in seq line 2100")
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
                                                           # "long_name" = "orography",
                                                           "long_name" = "terrain_height",
                                                           "res_orig" = basedistance,
                                                           "source" = "Munich DGM",
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
                                      self$plotcntr       <- 0

                                    },
                                    importfiles = function(...){
                                      print("This is not a supported function in this class.")
                                    },
                                    importbuildings_DUMMY = function(filepath){
                                      ncfile <- nc_open(filepath)
                                      build  <- ncvar_get(ncfile, "Band1")
                                      nc_close(ncfile)
                                      dx <- self$header$head$resolution
                                      build  <- floor(build/dx)*dx
                                      build[which(is.na(build),arr.ind = T)]  <- -9999.9
                                      build[build==0]   <- -9999.9

                                      adata    <- list("_FillValue" = -9999.9,
                                                       "units" = "m",
                                                       "long_name" = "building",
                                                       "res_origin" = dx,
                                                       "source" = "CityGML",
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
                                                         "long_name" = "building type classification",
                                                         # "long_name" = "building type",
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
                                        buildid  <- ncvar_get(ncfile, "Band1")
                                        buildid[is.na(buildid)] <- -9999.9
                                        buildid[buildid==0] <- -9999.9

                                      } else if(!trustfile) {
                                        # buildid   <- array(-9999.9,dim(self$data$buildings_2d$vals))
                                        # countr <- 1
                                        ncfile <- nc_open(filepath)
                                        buildid  <- ncvar_get(ncfile, "Band1")
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

                                    },
                                    implementNAs = function(){
                                      for(i in seq(names(self$data))){
                                        self$data[[names(self$data)[i]]]$vals[self$data[[names(self$data)[i]]]$vals==self$data[[names(self$data)[i]]]$'_FillValue'] <- NA
                                      }
                                    }



                                  )
)
