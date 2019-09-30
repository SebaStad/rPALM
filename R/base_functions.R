
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

palm_ncdf_berlin   <- R6::R6Class("palm_ncdf_berlin",
                              public = list(
                                dims           = NULL,
                                data           = NULL,
                                exportname     = NULL,
                                header         = NULL,
                                path           = NULL,

                                arcgis         = FALSE,

                                savedplots     = NULL,
                                plotcntr       = NULL,

                                vardimensions  = NULL,
                                oldversion = NULL,

                                # Funktion die mit $new aufgerufen wird!
#' Title
#'
#' @param filename Filename
#' @param headclass Variable of the global Class
#' @param pathtofiles Path to the Berlin data provided by the PALM4U Project
#' @param oldversion Leave at FALSE, only set to TRUE if you want to use a PALM version ~ < r2800
#'
#' @return Creates an R6 Class
#' @export
#'
#' @examples
                                initialize = function(filename, headclass, pathtofiles,oldversion = FALSE){
                                  if(oldversion){
                                    self$oldversion <- TRUE
                                  } else {
                                    self$oldversion <- FALSE
                                  }
                                  # filename:    Zukuenftiger Name der ausgegebenen static_driver datei
                                  # headclass:   Object der Klasse palm_global
                                  # pathtofiles: Pfad zu Berlin netCDF Dateien

                                  if(substrRight(filename,3)!=".nc"){
                                    filename <- paste(filename,".nc",sep="")
                                  }
                                  self$exportname     <- filename
                                  self$header         <- headclass
                                  self$path           <- pathtofiles
                                  self$savedplots     <- list()
                                  self$plotcntr       <- 0

                                },
                                # Import der vorhanden Berlin Daten
#' Title
#'
#' @param lengthx Gridpoints in x direction
#' @param lengthy Gridpoints in y direction
#' @param dx Grid spacing
#'
#' @return
#' @export
#'
#' @examples
                                importfiles = function(lengthx, lengthy, dx){

                                  # lengthx:  LÃ¤nge der Domain in x-Richtung
                                  # lengthy:  LÃ¤nge der Domain in y-Richtung
                                  # dx:       Gridabstand. FÃ¼r Berlin 1 oder 10

                                  setwd(self$path)

                                  files                       <- list.files(pattern=paste(dx,"m", sep=""))
                                  self$header$head$resolution <- dx

                                  dat                         <- list()
                                  dimen                       <- list()

                                  # dat:   Liste mit allen Variablen und Werten
                                  # dimen: Liste mit allen Dimensionen

                                  # Ab hier: Oeffnen aller Dateien und Auslesen der Werte + Speichern in der dat-Liste

                                  #####
                                  # Terrain height
                                  ncfile       <- nc_open(files[grep("terrain_height", files)])

                                  xvec         <-tryCatch({ seq(dx*0.5, lengthx*dx, by = dx)},
                                warning = function(w){
                                  print("Warning in seq line 126")
                                }, error = function(e){
                                  print("Error in seq line 126")
                                } )
                                  yvec         <- tryCatch({seq(dx*0.5, lengthy*dx, by = dx)},
                                    warning = function(w){
                                      print("Warning in seq line 131")
                                  }, error = function(e){
                                    print("Error in seq line 131")
                                  } )

                                  orogr        <- ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_y),
                                                            c(lengthx, lengthy))
                                  self$header$head$origin_z         <- min(orogr)
                                  orogr                             <- orogr - min(orogr)

                                  ######
                                  # Coordinates
                                  # X
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
                                  ######
                                  # Orography
                                  orogr        <- floor(orogr/dx)*dx
                                  adata        <- list("_FillValue" = -9999.9,
                                                       "units" = "m",
                                                       "long_name" = "terrain_height",
                                                       # "long_name" = "orography",
                                                       "res_orig" = dx,
                                                       "source" = "Atkis DGM",
                                                       "vals" = orogr,
                                                       "type" = "float")
                                  if(self$oldversion){
                                    dat$orography_2D  <- adata
                                  } else{
                                    dat$zt  <- adata
                                  }

                                  nc_close(ncfile)
                                  ######
                                  # Buildings
                                  # 2D
                                  ncfile <- nc_open(files[grep("building_height", files)])
                                  build  <- ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_y),
                                                      c(lengthx, lengthy))
                                  build  <- floor(build/dx)*dx
                                  build[which(is.na(build),arr.ind = T)]  <- -9999

                                  adata    <- list("_FillValue" = -9999.9,
                                                   "units" = "m",
                                                   "long_name" = "building",
                                                   "res_origin" = dx,
                                                   "source" = "CityGML",
                                                   "lod"= 1,
                                                   "vals" = build,
                                                   "type" = "float")
                                  if(self$oldversion){
                                    dat$buildings_2D  <- adata
                                  } else {
                                    dat$buildings_2d  <- adata
                                  }


                                  # 3D
                                  zmax <- max(build, na.rm = T)
                                  if(zmax<=0){
                                    z <- 0
                                    zmax <- 0
                                  } else {
                                    z   <- tryCatch({seq(0,zmax, by= dx)},
                                  warning = function(w){
                                    print("Warning in seq line 205")
                                  }, error = function(e){
                                    print("Error in seq line 205")
                                  } )
                                    z    <- z - 0.5*dx
                                    z[1] <- 0
                                  }

                                  adata      <- list("long_name" = "z",
                                                     "standard_name" = "z",
                                                     "units" = "m",
                                                     "vals" = z)
                                  #"type" = "float")
                                  dimen$z    <- adata

                                  self$dims  <- dimen

                                  build3d <- array(0, dim=c(dim(build),zmax/dx+1))
                                  for(i in seq(dim(build3d)[1])){
                                    for(j in seq(dim(build3d)[2])){
                                      if(build[i,j]>0){
                                        bheight    <- build[i,j]/dx
                                        build3d[i,j,1:bheight] <- 1
                                      }
                                    }
                                  }

                                  adata     <- list("_FillValue" = -127,
                                                    "units" = "m",
                                                    "long_name" = "building_flag",
                                                    # "long_name" = "buildings_3",
                                                    "res_origin" = dx,
                                                    "source" = "CityGML",
                                                    "lod" = 2,
                                                    "vals" = build3d,
                                                    "type" = "byte")

                                  if(self$oldversion){
                                    dat$buildings_3D  <- adata
                                  } else {
                                    dat$buildings_3d  <- adata
                                  }
                                  nc_close(ncfile)

                                  # ID's
                                  ncfile <- nc_open(files[grep("building_id", files)])
                                  buildid  <- ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_y),
                                                        c(lengthx, lengthy))
                                  adata     <- list("_FillValue" = -9999.9,
                                                    "units" = "",
                                                    # "long_name" = "building id",
                                                    "long_name" = "building id numbers",
                                                    "res_origin" = dx,
                                                    "source" = "",
                                                    "vals" = buildid,
                                                    "type" = "integer")

                                  dat$building_id  <- adata
                                  nc_close(ncfile)

                                  # Type's
                                  ncfile     <- nc_open(files[grep("building_type", files)])
                                  buildtype  <- ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_y),
                                                          c(lengthx, lengthy))
                                  adata      <- list("_FillValue" = -127,
                                                     "units" = "",
                                                     "long_name" = "building type classification",
                                                     # "long_name" = "building type",
                                                     "res_origin" = dx,
                                                     "source" = "",
                                                     "vals" = buildtype,
                                                     "type" = "integer")

                                  dat$building_type  <- adata
                                  nc_close(ncfile)

                                  #####
                                  # Vegetation
                                  # Type
                                  ncfile     <- nc_open(files[grep("vegetation_type", files)])
                                  vegtype    <- ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_y),
                                                          c(lengthx, lengthy))

                                  vegtype[which(is.na(vegtype),arr.ind = T)]  <- 3
                                  adata      <- list("_FillValue" = -127,
                                                     "units" = "",
                                                     "long_name" = "vegetation_type",
                                                     "res_origin" = dx,
                                                     "source" = "DLR Satellite",
                                                     "vals" = vegtype,
                                                     "type" = "byte")

                                  dat$vegetation_type  <- adata
                                  nc_close(ncfile)

                                  #####
                                  # Street
                                  # Type
                                  ncfile     <- nc_open(files[grep("street_type", files)])
                                  streettype    <- ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_y),
                                                             c(lengthx, lengthy))


                                  adata      <- list("_FillValue" = -127,
                                                     "units" = "",
                                                     "long_name" = "street_type",
                                                     "res_origin" = dx,
                                                     "source" = "",
                                                     "vals" = streettype,
                                                     "type" = "byte")

                                  dat$street_type  <- adata
                                  nc_close(ncfile)

                                  # Crossing
                                  ncfile        <- nc_open(files[grep("street_crossing", files)])
                                  streetcross   <- ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_y),
                                                             c(lengthx, lengthy))

                                  adata      <- list("_FillValue" = -127,
                                                     "units" = "",
                                                     "long_name" = "street_type",
                                                     "res_origin" = dx,
                                                     "source" = "",
                                                     "vals" = streetcross,
                                                     "type" = "byte")

                                  dat$street_crossing  <- adata
                                  nc_close(ncfile)

                                  #####
                                  # Water
                                  # Type
                                  ncfile        <- nc_open(files[grep("water_type", files)])
                                  watertype     <- ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_y),
                                                             c(lengthx, lengthy))

                                  adata      <- list("_FillValue" = -127,
                                                     "units" = "",
                                                     "long_name" = "water_type",
                                                     "source" = "DLR Satellite",
                                                     "vals" = watertype,
                                                     "type" = "byte")

                                  dat$water_type  <- adata
                                  nc_close(ncfile)

                                  #####
                                  # Pavement
                                  # Type
                                  ncfile        <- nc_open(files[grep("pavement_type", files)])
                                  pavementtype     <- ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_y),
                                                                c(lengthx, lengthy))

                                  adata      <- list("_FillValue" = -127,
                                                     "units" = "",
                                                     "long_name" = "pavement_type",
                                                     "source" = "OpenStreetMap",
                                                     "vals" = pavementtype,
                                                     "type" = "byte")

                                  dat$pavement_type  <- adata
                                  nc_close(ncfile)

                                  #######
                                  # CHECK FOR OVERLAPPING DATA
                                  # VEGETATION
                                  dat$vegetation_type$vals[which(!is.na(dat$pavement_type$vals), arr.ind = T)]   <- NA
                                  dat$vegetation_type$vals[which(!is.na(dat$building_type$vals), arr.ind = T)]   <- NA
                                  dat$vegetation_type$vals[which(!is.na(dat$water_type$vals), arr.ind = T)]      <- NA
                                  # PAVEMENT
                                  dat$pavement_type$vals[which(!is.na(dat$building_type$vals), arr.ind = T)]     <- NA
                                  dat$pavement_type$vals[which(!is.na(dat$water_type$vals), arr.ind = T)]        <- NA
                                  # WATER
                                  dat$water_type$vals[which(!is.na(dat$building_type$vals), arr.ind = T)]        <- NA


                                  # consistency check
                                  consistency_check  <- array(TRUE, dim = c(lengthx, lengthy))

                                  consistency_check[which(!is.na(dat$pavement_type$vals), arr.ind = T)]    <- FALSE
                                  consistency_check[which(!is.na(dat$building_type$vals), arr.ind = T)]    <- FALSE
                                  consistency_check[which(!is.na(dat$water_type$vals), arr.ind = T)]       <- FALSE
                                  consistency_check[which(!is.na(dat$vegetation_type$vals), arr.ind = T)]  <- FALSE

                                  # Dummy variable, see original ncl file
                                  dat$vegetation_type$vals[consistency_check]   <- 2


                                  #####
                                  # Soil type
                                  soiltype   <- dat$vegetation_type$vals
                                  soiltype[which(!is.na(dat$pavement_type$vals), arr.ind = T)]   <- 1

                                  adata      <- list("_FillValue" = -127,
                                                     "units" = "",
                                                    # "long_name" = "soil type",
                                                     "long_name" = "soil type classification",
                                                     "source" = "First Guess",
                                                     "vals" = soiltype,
                                                     "type" = "byte")
                                  dat$soil_type  <- adata



                                  # Trees not yet supported!

                                  #####
                                  # Green Roofs
                                  #ncfile        <- nc_open(files[grep("vegetation_on_roofs", files)])
                                  #greenroof     <- ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_x),
                                  #                              c(lengthx, lengthy))
                                  #greenroof[greenroof>0]   <- 1

                                  #adata      <- list("_FillValue" = -9999.9,
                                  #                   "units" = "",
                                  #                   "long_name" = "pavement_type",
                                  #                   "source" = "OpenStreetMap",
                                  #                   "vals" = pavementtype)

                                  #dat$pavement_type  <- adata
                                  #nc_close(ncfile)

                                  self$data <- dat

                                },
                                # Export der netCDF Datei
                                exportncdf = function(Path = self$path, EPSGCode = "EPSG:25833"){
                                  ###########
                                  # GUI ANPASSUNG FÜR EPSG
                                  ###########
                                  # HARDCODED!
                                  #
                                  if(EPSGCode=="EPSG:25831"){
                                    centralmeridian = 3.0
                                  } else if(EPSGCode=="EPSG:25832"){
                                    centralmeridian = 9.0
                                  } else if(EPSGCode=="EPSG:25833"){
                                    centralmeridian = 15.0
                                  } else if(EPSGCode=="EPSG:31468"){
                                    centralmeridian = 12.0
                                  } else {
                                    centralmeridian = 15.0
                                  }



                                  ###########
                                  # PIDS 1.9
                                  ###########
                                  # CRS Zeugs
                                  ###########
                                  if(any(names(self$data)=="crs")){
                                    self$data$crs <- NULL
                                  }
                                  adata        <- list("long_name" = "coordinate reference system",
                                                       "grid_mapping_name" = "transverse_mercator",
                                                       "longitude_of_prime_meridian" = 0.0,
                                                       "longitude_of_central_meridian" = centralmeridian,
                                                       "scale_factor_at_central_meridian" = 0.9996,
                                                       "latitude_of_projection_origin" = 0.0,
                                                       "false_easting" = 500000.0,
                                                       "false_northing" = 0.0,
                                                       "semi_major_axis" = 6378137.0,
                                                       "inverse_flattening" = 298.25722,
                                                       "units" = "m",
                                                       "epsg_code" = EPSGCode,
                                                       "vals" = 0,
                                                       "type" = "integer")
                                  self$data[["crs"]] <- adata
                                  self$vardimensions[["crs"]] <- c()

                                  eastval <- seq(self$header$head$origin_x,
                                                 self$header$head$origin_x +
                                                 self$header$head$resolution*(length(self$dims$x$vals)-1),
                                                 self$header$head$resolution)
                                  adata <- list("long_name" = "easting",
                                                "standard_name" = "projection_x_coordinate",
                                                "units" = "m",
                                                "vals" = eastval,
                                                "type" = "float")
                                  self$data[["E_UTM"]] <- adata
                                  self$vardimensions[["E_UTM"]] <- c("x")

                                  westval <- seq(self$header$head$origin_y,
                                                 self$header$head$origin_y +
                                                   self$header$head$resolution*(length(self$dims$y$vals)-1),
                                                 self$header$head$resolution)
                                  adata <- list("long_name" = "northing",
                                                "standard_name" = "projection_y_coordinate",
                                                "units" = "m",
                                                "vals" = westval,
                                                "type" = "float")
                                  self$data[["N_UTM"]] <- adata
                                  self$vardimensions[["N_UTM"]] <- c("y")

                                  e_utm <- eastval
                                  n_utm <- westval

                                  df <- as.data.frame(expand.grid(e_utm,n_utm))

                                  sputm <- SpatialPoints(df, proj4string=CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))# Defining Gauss Krüger)
                                  spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84 +no_defs") )

                                  thedata <- round(as.data.frame(spgeo),6)

                                  longitude_mat <- array( thedata[,1],c(length(e_utm),length(n_utm)))
                                  latitude_mat <- array( thedata[,2],c(length(e_utm),length(n_utm)))

                                  adata <- list("long_name" = "latitude",
                                                "standard_name" = "latitude",
                                                "units" = "degrees_north",
                                                "vals" = latitude_mat,
                                                "type" = "float")
                                  self$data[["lat"]] <- adata
                                  self$vardimensions[["lat"]] <- c("x", "y")

                                  adata <- list("long_name" = "longitude",
                                                "standard_name" = "longitude",
                                                "units" = "degrees_east",
                                                "vals" = longitude_mat,
                                                "type" = "float")
                                  self$data[["lon"]] <- adata
                                  self$vardimensions[["lon"]] <- c("x", "y")


                                  #

                                  # Path = Pfad in dem die Datei abgespeichert wird. Frei waehlbar, standardmaessig im
                                  #        gleichen Ordner wie Quelldateien

                                  #####
                                  # Definition der Dimensionen
                                  nc_dim_list  <- list()

                                  for(zz in seq(self$dims)){
                                    if(is.null(self$dims[[zz]]$standard_name)){
                                      self$dims[[zz]]$standard_name <- names(self$dims)[zz]
                                    }
                                    if(is.null(self$dims[[zz]]$long_name )){
                                      self$dims[[zz]]$long_name <- names(self$dims)[zz]
                                    }
                                    if(is.null(self$dims[[zz]]$units )){
                                      self$dims[[zz]]$units <- " "
                                    }
                                    ####
                                    nc_dim_list[[names(self$dims)[zz]]] <- ncdim_def(self$dims[[zz]]$long_name, self$dims[[zz]]$units, vals = self$dims[[zz]]$vals,
                                                                                     longname = self$dims[[zz]]$long_name)

                                  }

                                  # Definition aller Variablen Ã¼ber eine Schleife in eine Liste.
                                  # Muss an nc_create sowieso als Liste Ã¼bergeben werden
                                  ncvariables <- list()
                                  ncvarvector <- c()

                                  xvar <- nc_dim_list$x
                                  yvar <- nc_dim_list$y

                                  for(t in seq(self$data)){
                                    # Quick'n'dirty hotfix. Einlesen bestehender static driver belegt type mit
                                    # "int" statt "integer"
                                    if(self$data[[t]]$type=="int"){
                                      self$data[[t]]$type  <- "integer"
                                    }
                                    # Falls 3D GrÃ¶Ãe:
                                    if(length(dim(self$data[[t]]$vals))==3){
                                      # Vardimensions wird nur beschrieben, wenn wir die static Datei einlesen
                                      # bisher sollte das funktionieren, spÃ¤ter evtl mal genauer!
                                      if(is.null(self$vardimensions)){
                                        zcoord <- dim(self$data[[t]]$vals)[3]
                                        find_dim <- which(unlist(lapply(nc_dim_list, function(x){x$len==zcoord})))
                                        if(length(find_dim)>=2){
                                          print("Mehrere \"z-Koordinaten\" sind gleich lang. Erstes Element genutzt")
                                          print("HÃ¶chst warscheinlich Fehler enthalten")
                                          find_dim <- find_dim[1]
                                        }
                                        zvar     <- nc_dim_list[[find_dim]]
                                      } else {
                                        #if(!oldversion & names(self$data)[t]=="surface_fraction"){
                                        #  zvar     <- nc_dim_list[[self$vardimensions[[names(self$data)[t]]][1]]]
                                        #} else {
                                        zvar     <- nc_dim_list[[self$vardimensions[[names(self$data)[t]]][3]]]
                                        #}
                                      }
                                      #if(oldversion){
                                      dimlist  <- list(xvar,yvar,zvar)
                                      #} else if(!oldversion & names(self$data)[t]=="surface_fraction"){
                                      #  dimlist  <- list(zvar,xvar,yvar)
                                      #} else {
                                      #  dimlist  <- list(xvar,yvar,zvar)
                                      #}
                                    } else if(names(self$data)[t]=="crs"){
                                      dimlist <- list()
                                    } else if(names(self$data)[t]=="E_UTM"){
                                      dimlist <- list(xvar)
                                    } else if(names(self$data)[t]=="N_UTM"){
                                      dimlist <- list(yvar)
                                    } else {
                                      dimlist  <- list(xvar,yvar)
                                    }

                                    tmp     <-  ncvar_def(name    = names(self$data)[t],
                                                          units   = self$data[[t]]$units,
                                                          dim     = dimlist,
                                                          missval = self$data[[t]]$'_FillValue',
                                  ########## Falls probleme auftreten:
                                                          longname = self$data[[t]]$long_name,
                                  ########## Zeile wieder einkommentieren
                                  ########## (dann funktioniet der export von importierten ncdf nicht.)
                                                          prec = self$data[[t]]$type)

                                    ncvariables[[names(self$data)[t]]] <- tmp
                                    ncvarvector <- c(ncvarvector, tmp)

                                  }
                                  # Attribute, die normalerweise nicht mehr extra vergeben werden mÃ¼ssen
                                  # Fuer loop ch in loopnum!
                                  ex_atts  <- c("_FillValue", "units", "long_name", "vals", "type")

                                  # Erstellen der eigentlichen nc_file!
                                  ncfile  <- nc_create(self$exportname,vars = ncvariables ,force_v4=TRUE)

                                  # EinfÃ¼gen aller Attribute aus der Headerdatei palm_global als globale Attribute
                                  for(j in seq(self$header$head)){
                                    ncatt_put(ncfile, 0, names(self$header$head)[j], self$header$head[[j]])
                                  }


                                  # EinfÃ¼gen der ZusÃ¤tzlichen Attribute (ch in loopnum)
                                  # Sowie der eigentlichen Daten (ncvar_put(..., vals = self$data$XXX$vals))
                                  for(t in seq(ncfile$var)){

                                    loopnum <- which(!names(self$data[[t]]) %in% ex_atts)
                                    # simple fix to always get units
                                    loopnum <- c(loopnum, 2)

                                    for(ch in loopnum){
                                      loopvar   <- names(self$data[[t]])[ch]
                                      typething <- typeof(unlist(self$data[[t]][ch]))
                                      ncatt_put(nc= ncfile, varid = names(self$data)[t],attname =  loopvar,
                                                attval = unlist(self$data[[t]][ch]))
                                      }
                                    ncvar_put(ncfile,
                                              varid   = ncfile$var[[t]]$name,
                                              vals    = self$data[[t]]$vals)
                                  }
                                  # SchlieÃen und speichern der Datei
                                  nc_close(ncfile)

                                },
                                # Plot des ausgewÃ¤hlten Areas

                                plot_area = function(xleft, yleft, xl=40, yl= 40){

                                  # xleft:  untere linke ecke in x-Koordinaten (1 = Quelle links unten)
                                  # yleft:  untere linke ecke in y-Koordinaten (1 = Quelle links unten)
                                  # erlaubt theoretisch Zoom in area of interest

                                  # xl:     Anzahl der Punkte in x-Richtung (max nx_max - 1!)
                                  # yl:     Anzahl der Punkte in y-Richtung (max ny_max - 1!)
                                  if(self$oldversion){
                                    checkvar <- "buildings_2D"
                                  } else {
                                    checkvar <- "buildings_2d"
                                  }

                                  if(any(is.na(self$data$pavement_type$vals))){
                                    self$plotcntr     <- self$plotcntr + 1
                                    plot_surf         <- array(0,dim= c(xl,yl))
                                    plot_surf[which(!is.na(self$data$pavement_type$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]), arr.ind = T)]  <- 3

                                    veg_mat_provisorisch <- self$data$vegetation_type$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]
                                    veg_mat_provisorisch[is.na(veg_mat_provisorisch)] <- 0
                                    tree_mat_prov <- array(NA,dim=dim(veg_mat_provisorisch))
                                    tree_mat_prov[which(veg_mat_provisorisch%in%c(4:7,18),arr.ind = T)]  <- 1
                                    veg_mat_provisorisch[which(veg_mat_provisorisch%in%c(4:7),arr.ind = T)]  <- 0
                                    veg_mat_provisorisch[which(veg_mat_provisorisch==0,arr.ind = T)] <- NA

                                    plot_surf[which(!is.na(veg_mat_provisorisch), arr.ind = TRUE)] <- 2

                                    if(any(tree_mat_prov==1, na.rm = T)){
                                      plot_surf[which(!is.na(tree_mat_prov), arr.ind = TRUE)] <- 5
                                    }


                                    if(any(is.na(self$data[[checkvar]]$vals))){
                                      plot_surf[which(!is.na(self$data[[checkvar]]$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]), arr.ind = T)]    <- 1
                                    } else {
                                      plot_surf[which(!(self$data[[checkvar]]$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]<=-9999), arr.ind = T)]    <- 1
                                    }

                                    plot_surf[which(!is.na(self$data$water_type$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]), arr.ind = T)]       <- 4
                                  } else {
                                    self$plotcntr     <- self$plotcntr + 1
                                    plot_surf         <- array(0,dim= c(xl,yl))
                                    plot_surf[which(self$data$pavement_type$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]>0, arr.ind = T)]  <- 3

                                    veg_mat_provisorisch <- self$data$vegetation_type$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]
                                    veg_mat_provisorisch[which(veg_mat_provisorisch<0)] <- 0
                                    tree_mat_prov <- array(0,dim=dim(veg_mat_provisorisch))
                                    tree_mat_prov[which(veg_mat_provisorisch%in%c(4:7,18),arr.ind = T)]  <- 1
                                    veg_mat_provisorisch[which(veg_mat_provisorisch%in%c(4:7),arr.ind = T)]  <- 0

                                    plot_surf[which(veg_mat_provisorisch>0, arr.ind = TRUE)] <- 2

                                    if(any(tree_mat_prov==1, na.rm = T)){
                                      plot_surf[which(tree_mat_prov>0, arr.ind = TRUE)] <- 5
                                    }


                                    if(any(is.na(self$data[[checkvar]]$vals))){
                                      plot_surf[which(!is.na(self$data[[checkvar]]$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]), arr.ind = T)]    <- 1
                                    } else if(any(self$data[[checkvar]]$vals==0)){
                                      plot_surf[which(!(self$data[[checkvar]]$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]==0), arr.ind = T)] <- 1
                                    } else {
                                      plot_surf[which(!(self$data[[checkvar]]$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]<=-9999), arr.ind = T)]    <- 1
                                    }

                                    plot_surf[which(self$data$water_type$vals[xleft:(xleft+xl-1), yleft : (yleft+yl-1)]>0, arr.ind = T)]       <- 4

                                  }
                                  plot_data         <- melt(plot_surf)

                                  plot_data$colour  <- as.factor(plot_data$value)
                                  loopvar           <- levels(plot_data$colour)
                                  colorvec          <- c()
                                  for(jj in loopvar){
                                    if(jj == "0"){
                                      levels(plot_data$colour)[levels(plot_data$colour)==jj]  <- "nothing"
                                      colorvec <- c(colorvec, "#FFFFFF")
                                    }
                                    if(jj == "1"){
                                      levels(plot_data$colour)[levels(plot_data$colour)==jj]  <- "building"
                                      colorvec <- c(colorvec, "#000000")
                                    }
                                    if(jj == "2"){
                                      levels(plot_data$colour)[levels(plot_data$colour)==jj]  <- "vegetation"
                                      colorvec <- c(colorvec, "#00F608")
                                    }
                                    if(jj == "3"){
                                      levels(plot_data$colour)[levels(plot_data$colour)==jj]  <- "street"
                                      colorvec <- c(colorvec, "#8C8C8C")
                                    }
                                    if(jj == "4"){
                                      levels(plot_data$colour)[levels(plot_data$colour)==jj]  <- "water"
                                      colorvec <- c(colorvec, "#0000ff")
                                    }
                                    if(jj == "5"){
                                      levels(plot_data$colour)[levels(plot_data$colour)==jj]  <- "trees"
                                      colorvec <- c(colorvec, "#088A08")
                                    }
                                  }


                                  #levels(plot_data$colour) <- c("nothing", "building", "vegetation", "street", "water")

                                  self$savedplots[[self$plotcntr]] <-ggplot(plot_data, aes(x = Var1, y = Var2, fill=colour)) +
                                    geom_tile() +
                                    scale_fill_manual(values=colorvec) +
                                    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                                                       axis.text.y=element_text(size=9),
                                                       plot.title=element_text(size=11),
                                                       legend.text=element_text(size=7)) +
                                    coord_fixed(ratio=1) + xlab("X") + ylab("Y")
                                  self$savedplots[[self$plotcntr]]

                                },
                                createofficebuilding = function(start, size, height,ignorestreets = TRUE, notnew = c()){
                                  if(length(notnew) > 0){
                                    buildtype <- notnew # muss 4 oder 5 sein. Bezieht sich auf Baujahr!
                                    # quelle: https://palm.muk.uni-hannover.de/trac/attachment/wiki/doc/palmcc/Surface_model.pdf
                                  } else {
                                    buildtype <- 6
                                  }
                                  dx   <- self$header$head$resolution


                                  height <- floor(height/dx)*dx

                                  ### Free Space in not Building_Vars
                                  self$data$vegetation_type$vals[start[1]:(start[1]+size[1]-1),
                                                                 start[2]:(start[2]+size[2]-1)] <- self$data$vegetation_type$`_FillValue`
                                  self$data$street_type$vals[start[1]:(start[1]+size[1]-1),
                                                             start[2]:(start[2]+size[2]-1)] <- self$data$street_type$`_FillValue`
                                  self$data$street_crossing$vals[start[1]:(start[1]+size[1]-1),
                                                                 start[2]:(start[2]+size[2]-1)] <- self$data$street_crossing$`_FillValue`
                                  # Theoretisch richtig! Aber Halt NA
                                  #self$data$water_type$vals[start[1]:(start[1]+size[1]-1),
                                  #                          start[2]:(start[2]+size[2]-1)] <- self$data$water_type$`_FillValue`
                                  self$data$water_type$vals[start[1]:(start[1]+size[1]-1),
                                                            start[2]:(start[2]+size[2]-1)] <- NA
                                  if(ignorestreets){
                                    # self$data$pavement_type$vals[start[1]:(start[1]+size[1]-1),
                                    #                             start[2]:(start[2]+size[2]-1)] <- self$data$pavement_type$`_FillValue`

                                    self$data$pavement_type$vals[start[1]:(start[1]+size[1]-1),
                                                                 start[2]:(start[2]+size[2]-1)] <- NA
                                  }
                                  self$data$soil_type$vals[start[1]:(start[1]+size[1]-1),
                                                           start[2]:(start[2]+size[2]-1)] <- self$data$soil_type$`_FillValue`
                                  #### Create Building in Building Vars
                                  self$data$buildings_2D$vals[start[1]:(start[1]+size[1]-1),
                                                              start[2]:(start[2]+size[2]-1)] <- height

                                  self$data$building_id$vals[start[1]:(start[1]+size[1]-1),
                                                             start[2]:(start[2]+size[2]-1)] <-max(self$data$building_id$vals, na.rm = TRUE)+1

                                  self$data$building_type$vals[start[1]:(start[1]+size[1]-1),
                                                               start[2]:(start[2]+size[2]-1)] <- buildtype

                                  # create new frame for 3d_building data, if new height is bigger than old!
                                  if(exists("self$data$buildings_3D")){
                                    if(dim(self$data$buildings_3D$vals)[3]<=height/dx){
                                      newarray    <- array(0,dim=c(dim(self$data$buildings_3D$vals)[1],dim(self$data$buildings_3D$vals)[2],1+ height/dx))
                                      newarray[,,1:dim(self$data$buildings_3D$vals)[3]] <- self$data$buildings_3D$vals
                                      self$data$buildings_3D$vals  <- newarray
                                      zmax <- max(height,na.rm = T)
                                      if(zmax<=0){
                                        z <- 0
                                        zmax <- 0
                                      } else {
                                        z   <- tryCatch({seq(0,zmax, by= dx)},
                                      warning = function(w){
                                        print("Warning in seq line 828")
                                      }, error = function(e){
                                        print("Error in seq line 828")
                                      } )
                                        z    <- z - 0.5*dx
                                        z[1] <- 0
                                      }
                                      adata      <- list("long_name" = "z",
                                                         "standard_name" = "z",
                                                         "units" = "m",
                                                         "vals" = z)
                                      #"type" = "float")
                                      self$dims$z  <- adata
                                    }
                                  }
                                  for(ii in seq(size[1])){
                                    for(jj in seq(size[2])){
                                      if(!ignorestreets){
                                        if(!is.na(self$data$pavement_type$vals[start[1]+ ii -1 ,start[2]+jj -1]) ){
                                          #| self$data$pavement_type$vals[start[1]+ ii -1 ,start[2]+jj -1]==-127
                                          self$data$buildings_2D$vals[start[1]+ ii -1 ,start[2]+jj -1]  <- self$data$buildings_2D$`_FillValue`
                                          self$data$building_id$vals[start[1]+ ii -1 ,start[2]+jj -1]  <- NA
                                          self$data$building_type$vals[start[1]+ ii -1 ,start[2]+jj -1]     <- NA
                                        } else{
                                          if(exists("self$data$buildings_3D")){
                                            logheight  <- height/dx
                                            self$data$buildings_3D$vals[start[1]+ ii -1 ,start[2]+jj -1,1:logheight] <- 1
                                          }
                                        }
                                      } else{
                                        if(exists("self$data$buildings_3D")){
                                          logheight  <- height/dx
                                          self$data$buildings_3D$vals[start[1]+ ii -1 ,start[2]+jj -1,1:logheight] <- 1
                                        }
                                      }
                                    }
                                  }



                                },
                                createresidentialbuilding =  function(start, size, height,ignorestreets = TRUE, notnew = c()){
                                  if(length(notnew) > 0){
                                    buildtype <- notnew # muss 1 oder 2 sein. Bezieht sich auf Baujahr!
                                    # quelle: https://palm.muk.uni-hannover.de/trac/attachment/wiki/doc/palmcc/Surface_model.pdf
                                  } else {
                                    buildtype <- 3
                                  }
                                  dx   <- self$header$head$resolution


                                  height <- floor(height/dx)*dx

                                  ### Free Space in not Building_Vars
                                  self$data$vegetation_type$vals[start[1]:(start[1]+size[1]-1),
                                                                 start[2]:(start[2]+size[2]-1)] <- self$data$vegetation_type$`_FillValue`
                                  self$data$street_type$vals[start[1]:(start[1]+size[1]-1),
                                                             start[2]:(start[2]+size[2]-1)] <- self$data$street_type$`_FillValue`
                                  self$data$street_crossing$vals[start[1]:(start[1]+size[1]-1),
                                                                 start[2]:(start[2]+size[2]-1)] <- self$data$street_crossing$`_FillValue`
                                  # Theoretisch richtig! Aber Halt NA
                                  #self$data$water_type$vals[start[1]:(start[1]+size[1]-1),
                                  #                          start[2]:(start[2]+size[2]-1)] <- self$data$water_type$`_FillValue`
                                  if(ignorestreets){
                                    # self$data$pavement_type$vals[start[1]:(start[1]+size[1]-1),
                                    #                             start[2]:(start[2]+size[2]-1)] <- self$data$pavement_type$`_FillValue`

                                    self$data$pavement_type$vals[start[1]:(start[1]+size[1]-1),
                                                                 start[2]:(start[2]+size[2]-1)] <- NA
                                  }
                                  self$data$soil_type$vals[start[1]:(start[1]+size[1]-1),
                                                           start[2]:(start[2]+size[2]-1)] <- self$data$soil_type$`_FillValue`
                                  #### Create Building in Building Vars
                                  self$data$buildings_2D$vals[start[1]:(start[1]+size[1]-1),
                                                              start[2]:(start[2]+size[2]-1)] <- height

                                  self$data$building_id$vals[start[1]:(start[1]+size[1]-1),
                                                             start[2]:(start[2]+size[2]-1)] <- max(self$data$building_id$vals, na.rm = TRUE)+1

                                  self$data$building_type$vals[start[1]:(start[1]+size[1]-1),
                                                               start[2]:(start[2]+size[2]-1)] <- buildtype

                                  # create new frame for 3d_building data, if new height is bigger than old!
                                  if(exists("self$data$buildings_3D")){
                                    if(dim(self$data$buildings_3D$vals)[3]<=height/dx){
                                      newarray    <- array(0,dim=c(dim(self$data$buildings_3D$vals)[1],dim(self$data$buildings_3D$vals)[2],1+ height/dx))
                                      newarray[,,1:dim(self$data$buildings_3D$vals)[3]] <- self$data$buildings_3D$vals
                                      self$data$buildings_3D$vals  <- newarray

                                      zmax <- max(height,na.rm = T)
                                      if(zmax<=0){
                                        z <- 0
                                        zmax <- 0
                                      } else {
                                        z   <- tryCatch({seq(0,zmax, by= dx)},
                                      warning = function(w){
                                        print("Warning in seq line 922")
                                      }, error = function(e){
                                        print("Error in seq line 922")
                                      } )
                                        z    <- z - 0.5*dx
                                        z[1] <- 0
                                      }

                                      adata      <- list("long_name" = "z",
                                                         "standard_name" = "z",
                                                         "units" = "m",
                                                         "vals" = z)
                                      #"type" = "float")
                                      self$dims$z  <- adata
                                    }
                                  }
                                  for(ii in seq(size[1])){
                                    for(jj in seq(size[2])){
                                      if(!ignorestreets){
                                        if(!is.na(self$data$pavement_type$vals[start[1]+ ii -1 ,start[2]+jj -1]) ){
                                          #| self$data$pavement_type$vals[start[1]+ ii -1 ,start[2]+jj -1]==-127
                                          self$data$buildings_2D$vals[start[1]+ ii -1 ,start[2]+jj -1]  <- self$data$buildings_2D$`_FillValue`
                                          self$data$building_id$vals[start[1]+ ii -1 ,start[2]+jj -1]  <- NA
                                          self$data$building_type$vals[start[1]+ ii -1 ,start[2]+jj -1]     <- NA
                                        } else{
                                          if(any(names(self$data)=="buildings_3D")){
                                            logheight  <- height/dx
                                            self$data$buildings_3D$vals[start[1]+ ii -1 ,start[2]+jj -1,1:logheight] <- 1
                                          }
                                        }
                                      } else{
                                        if(any(names(self$data)=="buildings_3D")){
                                          logheight  <- height/dx
                                          self$data$buildings_3D$vals[start[1]+ ii -1 ,start[2]+jj -1,1:logheight] <- 1
                                        }
                                      }
                                    }
                                  }
                                },
                                createlod1area  = function(start, array2d){
                                  size <- dim(array2d)

                                  # initialize single matrices
                                  #veg_mat            <- array(self$data$vegetation_type$`_FillValue`, size)
                                  #streettyp_mat      <- array(self$data$street_type$`_FillValue`,size)
                                  #streetcr_mat       <- array(self$data$street_crossing$`_FillValue`, size)
                                  #pave_mat           <- array(self$data$pavement_type$`_FillValue`, size)

                                  veg_mat            <- matrix(self$data$vegetation_type$`_FillValue`, size[1], size[2])
                                  # veg_par_mat       <- array(NA, size)
                                  soil_mat           <- matrix(self$data$soil_type$`_FillValue`, size[1], size[2])
                                  streettyp_mat      <- matrix(NA, size[1], size[2])
                                  streetcr_mat       <- matrix(NA, size[1], size[2])
                                  pave_mat           <- matrix(self$data$pavement_type$`_FillValue`, size[1], size[2])
                                  water_mat          <- matrix(self$data$water_type$`_FillValue`, size[1], size[2])
                                  #build2d_mat        <- array(self$data$buildings_2D$`_FillValue`, size)
                                  build2D_mat        <- matrix(self$data$buildings_2d$`_FillValue`, size[1], size[2])
                                  #buildid_mat        <- array(self$data$building_id$`_FillValue`, size)
                                  buildid_mat        <- matrix(self$data$building_id$`_FillValue`, size[1], size[2])
                                  # buildtyp_mat       <- array(self$data$building_type$`_FillValue`, size)
                                  buildtyp_mat       <- matrix(self$data$building_type$`_FillValue`, size[1], size[2])
                                  soiltyp_mat        <- matrix(self$data$soil_type$`_FillValue`, size[1], size[2])


                                  # Fill respective positions
                                  veg_mat[which(substr(array2d,1,1)=="V", arr.ind = T)]       <- as.integer(substr(array2d[which(substr(array2d,1,1)=="V", arr.ind = T)],2,3))
                                  # veg_par_mat[which(substr(array2d,1,1)=="V", arr.ind = T)]  <- substr(array2d[which(substr(array2d,1,1)=="V", arr.ind = T)],2,3)
                                  pave_mat[which(substr(array2d,1,1)=="P", arr.ind = T)]      <- as.integer(substr(array2d[which(substr(array2d,1,1)=="P", arr.ind = T)],2,3))
                                  water_mat[which(substr(array2d,1,1)=="W", arr.ind = T)]     <- as.integer(substr(array2d[which(substr(array2d,1,1)=="W", arr.ind = T)],2,3))
                                  build2D_mat[which(substr(array2d,1,1)=="B", arr.ind = T)]   <- as.numeric(substr(array2d[which(substr(array2d,1,1)=="B", arr.ind = T)],4,5))
                                  buildid_mat[which(substr(array2d,1,1)=="B", arr.ind = T)]   <- as.integer(max(self$data$building_id$vals, na.rm = TRUE))+1
                                  # buildtyp_mat[which(substr(array2d,1,1)=="B", arr.ind = T)]  <- substr(array2d[which(substr(array2d,1,1)=="B", arr.ind = T)],3,4)
                                  buildtyp_mat[which(substr(array2d,1,1)=="B", arr.ind = T)]  <- as.integer(substr(array2d[which(substr(array2d,1,1)=="B", arr.ind = T)],2,2))
                                  # streettyp_mat[which(substr(array2d,1,1)=="P", arr.ind = T)]  <- substr(array2d[which(substr(array2d,1,1)=="V", arr.ind = T)],2,3)
                                  streettyp_mat[which(substr(array2d,1,1)=="P", arr.ind = T)]  <- as.integer(8)
                                  # streetcr_mat[which(substr(array2d,1,1)=="V", arr.ind = T)]  <- substr(array2d[which(substr(array2d,1,1)=="V", arr.ind = T)],2,3)

                                  dx   <- self$header$head$resolution

                                  height <- 0
                                  tryCatch(height <- floor(max(build2D_mat,na.rm = T)/dx)*dx , warning = function(w){print("Reminder: No Building was imported by the lod1_array!")})


                                  array_layers         <- array(0,c(size,10))
                                  array_layers[,,1]    <- self$data$vegetation_type$vals[start[1]:(start[1]+size[1]-1),
                                                                                         start[2]:(start[2]+size[2]-1)]
                                  tryCatch(array_layers[,,2]    <- self$data$street_type$vals[start[1]:(start[1]+size[1]-1),
                                                                                              start[2]:(start[2]+size[2]-1)], error = function(e){print("No street_type read")})
                                  tryCatch(array_layers[,,3]    <- self$data$street_crossing$vals[start[1]:(start[1]+size[1]-1),
                                                                                                  start[2]:(start[2]+size[2]-1)], error = function(e){print("No street_crossing read")})
                                  array_layers[,,4]    <- self$data$water_type$vals[start[1]:(start[1]+size[1]-1),
                                                                                    start[2]:(start[2]+size[2]-1)]
                                  array_layers[,,5]    <- self$data$pavement_type$vals[start[1]:(start[1]+size[1]-1),
                                                                                       start[2]:(start[2]+size[2]-1)]
                                  array_layers[,,6]    <- self$data$soil_type$vals[start[1]:(start[1]+size[1]-1),
                                                                                   start[2]:(start[2]+size[2]-1)]
                                  if(self$oldversion){
                                    array_layers[,,7]    <- self$data$buildings_2D$vals[start[1]:(start[1]+size[1]-1),
                                                                                        start[2]:(start[2]+size[2]-1)]
                                  } else if(!self$oldversion){
                                    array_layers[,,7]    <- self$data$buildings_2d$vals[start[1]:(start[1]+size[1]-1),
                                                                                        start[2]:(start[2]+size[2]-1)]
                                  }

                                  array_layers[,,8]     <- self$data$building_id$vals[start[1]:(start[1]+size[1]-1),
                                                                                      start[2]:(start[2]+size[2]-1)]
                                  array_layers[,,9]     <-  self$data$building_type$vals[start[1]:(start[1]+size[1]-1),
                                                                                         start[2]:(start[2]+size[2]-1)]
                                  tryCatch(array_layers[,,10]    <-  self$data$vegetation_pars$vals[start[1]:(start[1]+size[1]-1),
                                                                                                    start[2]:(start[2]+size[2]-1), 2], error = function(e){print("No vegetation pars read!")})
                                  surface_frictions    <-  self$data$surface_fraction$vals[start[1]:(start[1]+size[1]-1),
                                                                                           start[2]:(start[2]+size[2]-1), ]

                                  if(dim(self$data$surface_fraction$vals)[3]==3){
                                    fvec    <- 1
                                    pvec <- 2
                                    wvec <- 3
                                  } else if(dim(self$data$surface_fraction$vals)[3]==4){
                                    fvec    <- c(1,4)
                                    pvec    <- c(2,4)
                                    wvec    <- c(3,4)

                                  }
                                  # newarray <- array(NA, size)
                                  # newarray[which(!is.na(array_layers[,,1]),arr.ind = T)] <- array_layers[,,1][which(!is.na(array_layers[,,1]),arr.ind = T)]
                                  restvec <- seq(9)
                                  for(i in seq(size[1])){
                                    for(j in seq(size[2])){
                                      if(array2d[i,j]==0){

                                      } else {
                                        array_layers[i,j,]  <- c(self$data$vegetation_type$`_FillValue`, NA, NA,
                                                                 self$data$water_type$`_FillValue`,
                                                                 self$data$pavement_type$`_FillValue`,
                                                                 self$data$soil_type$`_FillValue`,
                                                                 self$data$buildings_2d$`_FillValue`,
                                                                 self$data$building_id$`_FillValue`,
                                                                 self$data$building_type$`_FillValue`,
                                                                 NA)
                                        if(veg_mat[i,j]>0){
                                          array_layers[i,j,1]   <- veg_mat[i,j]
                                          ######################################
                                          if(veg_mat[i,j]%in%c(4:7)){
                                            array_layers[i,j,10]  <- 6
                                          } else {
                                            array_layers[i,j,10]  <- 1
                                          }
                                          ######################################
                                          surface_frictions[i,j,fvec]  <- 1
                                          surface_frictions[i,j,2:3]  <- 0
                                          array_layers[i,j,6]   <-  1
                                        }
                                        if(is.na(streettyp_mat[i,j])){
                                          array_layers[i,j,2]   <- streettyp_mat[i,j]
                                          surface_frictions[i,j,]  <- NA
                                        }
                                        if(pave_mat[i,j]>0){
                                          array_layers[i,j,5]   <-pave_mat[i,j]
                                          array_layers[i,j,6]   <-  1
                                          surface_frictions[i,j,pvec]  <- 1
                                          surface_frictions[i,j,c(1,3)]  <- 0
                                        }
                                        if(build2D_mat[i,j]>0){
                                          array_layers[i,j,7]   <- build2D_mat[i,j]
                                          surface_frictions[i,j,]  <- NA
                                        }
                                        if(buildid_mat[i,j]>0){
                                          array_layers[i,j,8]   <- buildid_mat[i,j]
                                          surface_frictions[i,j,]  <- NA
                                        }
                                        if(buildtyp_mat[i,j]>0){
                                          array_layers[i,j,9]   <-buildtyp_mat[i,j]
                                          surface_frictions[i,j,]  <- NA
                                        }
                                        if(water_mat[i,j]>0){
                                          array_layers[i,j,4]   <- water_mat[i,j]
                                          array_layers[i,j,6]   <-  self$data$soil_type$`_FillValue`
                                          surface_frictions[i,j,wvec]  <- 1
                                          surface_frictions[i,j,c(1,2)]  <- 0
                                        }
                                      }
                                    }
                                  }
                                  if(dim(self$data$surface_fraction$vals)[3]==3){
                                    self$data$surface_fraction$vals[start[1]:(start[1]+size[1]-1),
                                                                    start[2]:(start[2]+size[2]-1),]  <- surface_frictions

                                  } else if(dim(self$data$surface_fraction$vals)[3]==4){

                                    self$data$surface_fraction$vals[start[1]:(start[1]+size[1]-1),
                                                                    start[2]:(start[2]+size[2]-1),]  <- surface_frictions
                                  }
                                  self$data$vegetation_type$vals[start[1]:(start[1]+size[1]-1),
                                                                 start[2]:(start[2]+size[2]-1)]   <- as.integer(array_layers[,,1])
                                  tryCatch(self$data$street_type$vals[start[1]:(start[1]+size[1]-1),
                                                                      start[2]:(start[2]+size[2]-1)]       <- as.integer(array_layers[,,2]), error = function(e){print("No street_type")})
                                  tryCatch(self$data$street_crossing$vals[start[1]:(start[1]+size[1]-1),
                                                                          start[2]:(start[2]+size[2]-1)]   <-  as.integer(array_layers[,,3]), error = function(e){print("No street_crossing")})
                                  self$data$water_type$vals[start[1]:(start[1]+size[1]-1),
                                                            start[2]:(start[2]+size[2]-1)]        <- as.integer(array_layers[,,4])
                                  self$data$pavement_type$vals[start[1]:(start[1]+size[1]-1),
                                                               start[2]:(start[2]+size[2]-1)]    <- as.integer(array_layers[,,5])
                                  self$data$soil_type$vals[start[1]:(start[1]+size[1]-1),
                                                           start[2]:(start[2]+size[2]-1)]         <- as.integer(array_layers[,,6])

                                  if(self$oldversion){
                                    self$data$buildings_2D$vals[start[1]:(start[1]+size[1]-1),
                                                                start[2]:(start[2]+size[2]-1)]      <- array_layers[,,7]
                                  } else if(!self$oldversion){
                                    self$data$buildings_2d$vals[start[1]:(start[1]+size[1]-1),
                                                                start[2]:(start[2]+size[2]-1)]      <- array_layers[,,7]

                                  }

                                  self$data$building_id$vals[start[1]:(start[1]+size[1]-1),
                                                             start[2]:(start[2]+size[2]-1)]       <- as.integer(array_layers[,,8])

                                  self$data$building_type$vals[start[1]:(start[1]+size[1]-1),
                                                               start[2]:(start[2]+size[2]-1)]     <- as.integer(array_layers[,,9])

                                  tryCatch(self$data$vegetation_pars$vals[start[1]:(start[1]+size[1]-1),
                                                                          start[2]:(start[2]+size[2]-1), 2] <- array_layers[,,10], error = function(e){print("No vegetation_pars")})
                                  tryCatch(self$data$basal_area_density$vals[start[1]:(start[1]+size[1]-1),
                                                                             start[2]:(start[2]+size[2]-1), ] <- as.numeric(NA), error = function(e){print("No basal area")})
                                  tryCatch(self$data$building_pars$vals[start[1]:(start[1]+size[1]-1),
                                                                        start[2]:(start[2]+size[2]-1), ] <- as.numeric(NA), error = function(e){print("No building_pars")})
                                  tryCatch(self$data$tree_id$vals[start[1]:(start[1]+size[1]-1),
                                                                  start[2]:(start[2]+size[2]-1), ] <- as.integer(NA), error = function(e){print("No tree_ids")})
                                  tryCatch(self$data$leaf_area_density$vals[start[1]:(start[1]+size[1]-1),
                                                                            start[2]:(start[2]+size[2]-1), ] <- as.numeric(NA), error = function(e){print("No leaf area density")})




                                  if(any(names(self$data)=="buildings_3D")){
                                    if(dim(self$data$buildings_3D$vals)[3]<=height/dx){
                                      newarray    <- array(0,dim=c(dim(self$data$buildings_3D$vals)[1],dim(self$data$buildings_3D$vals)[2],1+ height/dx))
                                      newarray[,,1:dim(self$data$buildings_3D$vals)[3]] <- self$data$buildings_3D$vals
                                      self$data$buildings_3D$vals  <- newarray

                                      zmax <- max(height,na.rm = T)
                                      if(zmax<=0){
                                        z <- 0
                                        zmax <- 0
                                      } else {
                                        z   <- tryCatch({seq(0,zmax, by= dx)},
                                      warning = function(w){
                                        print("Warning in seq line 1167")
                                      } , error = function(e){
                                        print("Error in seq line 1167")
                                      })
                                        z    <- z - 0.5*dx
                                        z[1] <- 0
                                      }

                                      adata      <- list("long_name" = "z",
                                                         "standard_name" = "z",
                                                         "units" = "m",
                                                         "vals" = z)
                                      #"type" = "float")
                                      self$dims$z  <- adata
                                    }
                                  }
                                  if(any(names(self$data)=="buildings_3d")){
                                    if(dim(self$data$buildings_3d$vals)[3]<=height/dx){
                                      newarray    <- array(0,dim=c(dim(self$data$buildings_3d$vals)[1],dim(self$data$buildings_3d$vals)[2],1+ height/dx))
                                      newarray[,,1:dim(self$data$buildings_3d$vals)[3]] <- self$data$buildings_3d$vals
                                      self$data$buildings_3d$vals  <- newarray

                                      zmax <- max(height,na.rm = T)
                                      if(zmax<=0){
                                        z <- 0
                                        zmax <- 0
                                      } else {
                                        z   <- tryCatch({seq(0,zmax, by= dx)},
                                      warning = function(w){
                                        print("Warning in seq line 1194")
                                      }, error = function(e){
                                        print("Error in seq line 1194")
                                      } )
                                        z    <- z - 0.5*dx
                                        z[1] <- 0
                                      }

                                      adata      <- list("long_name" = "z",
                                                         "standard_name" = "z",
                                                         "units" = "m",
                                                         "vals" = z)
                                      #"type" = "float")
                                      self$dims$z  <- adata
                                    }
                                  }
                                  for(ii in seq(size[1])){
                                    for(jj in seq(size[2])){
                                      if(self$oldversion){
                                        if(!is.na(self$data$buildings_2D$vals[start[1]+ ii -1,
                                                                              start[2]+ jj -1]) &&
                                           self$data$buildings_2D$vals[start[1]+ ii -1,
                                                                              start[2]+ jj -1]>0){
                                          if(any(names(self$data)=="buildings_3D")){
                                            logheight  <- self$data$buildings_2D$vals[start[1]+ ii -1,
                                                                                      start[2]+ jj -1]/dx
                                            self$data$buildings_3D$vals[start[1]+ ii -1 ,start[2]+jj -1,1:(logheight+1)] <- 1
                                          }
                                        }
                                      } else if(!self$oldversion){
                                        if(!is.na(self$data$buildings_2d$vals[start[1]+ ii -1,
                                                                              start[2]+ jj -1]) &&
                                           self$data$buildings_2d$vals[start[1]+ ii -1,
                                                                       start[2]+ jj -1]>0){
                                          if(any(names(self$data)=="buildings_3d")){
                                            logheight  <- self$data$buildings_2d$vals[start[1]+ ii -1,
                                                                                      start[2]+ jj -1]/dx
                                            self$data$buildings_3d$vals[start[1]+ ii -1 ,start[2]+jj -1,1:(logheight+1)] <- 1
                                          }
                                        }
                                      }
                                    }
                                  }
                                },
                                createbuilding3D = function(force = FALSE, orogrphy3d = FALSE){
                                  if(self$oldversion){
                                    checkvar <- "buildings_3D"
                                    buildings2d <- "buildings_2D"
                                  } else {
                                    checkvar <- "buildings_3d"
                                    buildings2d <- "buildings_2d"
                                  }

                                  if(any(names(self$data)==checkvar) & force == FALSE){
                                    print("A buildings_3D array already exists")
                                  } else{
                                    build <- self$data[[buildings2d]]$vals
                                    build[is.na(build)]  <- -9999.9
                                    dx <- self$header$head$resolution
                                    zmax <- max(build, na.rm = T)
                                    if(zmax<=0){
                                      z <- 0
                                      zmax <- 0
                                    } else {
                                      z   <- tryCatch({seq(0,zmax, by= dx)},
                                    warning = function(w){
                                      print("Warning in seq line 1258")
                                    }, error = function(e){
                                      print("Error in seq line 1258")
                                    } )
                                      z    <- z - 0.5*dx
                                      z[1] <- 0
                                    }


                                    build3d <- array(0, dim=c(dim(build),zmax/dx+1))
                                    for(i in seq(dim(build3d)[1])){
                                      for(j in seq(dim(build3d)[2])){
                                        if(build[i,j]>0){
                                          bheight    <- build[i,j]/dx
                                          build3d[i,j,1:(bheight+1)] <- 1
                                        }
                                      }
                                    }

                                    adata      <- list("long_name" = "z",
                                                       "standard_name" = "z",
                                                       "units" = "m",
                                                       "vals" = z)
                                    #"type" = "float")
                                    self$dims$z  <- adata

                                    self$vardimensions[[checkvar]] <- c(1,2,which(names(self$dims)=="z"))

                                    adata     <- list("_FillValue" = -127,
                                                      "units" = "",
                                                      "long_name" = "building_flag",
                                                      "res_origin" = dx,
                                                      "source" = "CityGML",
                                                      "lod" = 2,
                                                      "vals" = build3d,
                                                      "type" = "byte")

                                    self$data[[checkvar]]  <- adata
                                  }
                                  if(orogrphy3d){
                                    if(self$oldversion){
                                      checkvar <- "orography_2D"

                                    } else {
                                      checkvar <- "zt"
                                    }


                                    build <- self$data[[checkvar]]$vals
                                    dx <- self$header$head$resolution
                                    zmax <- max(build, na.rm = T)

                                    z   <- tryCatch({seq(0,zmax, by= dx)},
                                warning = function(w){
                                  print("Warning in seq line 1309")
                                }, error = function(e){
                                  print("Error in seq line 1309")
                                } )

                                    adata      <- list("long_name" = "z_oro",
                                                       "standard_name" = "z_oro",
                                                       "units" = "m",
                                                       "vals" = z)

                                    self$dims$z_oro  <- adata

                                    self$vardimensions[["orography_3D"]] <- c(1,2,which(names(self$dims)=="z_oro"))


                                    build3d <- array(0, dim=c(dim(build),zmax/dx+1))
                                    for(i in seq(dim(build3d)[1])){
                                      for(j in seq(dim(build3d)[2])){
                                        if(build[i,j]>0){
                                          bheight    <- build[i,j]/dx
                                          build3d[i,j,1:(bheight+1)] <- 1
                                        }
                                      }
                                    }

                                    adata     <- list("_FillValue" = -127,
                                                      "units" = "m",
                                                      "long_name" = "3D orography data",
                                                      "res_origin" = dx,
                                                      "source" = "CityGML",
                                                      "lod" = 2,
                                                      "vals" = build3d,
                                                      "type" = "byte")

                                    self$data$orography_3D  <- adata
                                  }

                                },
                                quickplot = function(variable){

                                  if(any(self$data[[variable]]$vals<0)){
                                    plotmatrix <- self$data[[variable]]$vals
                                    plotmatrix[plotmatrix<0] <- 0
                                  } else {
                                    plotmatrix <- self$data[[variable]]$vals
                                  }

                                  plt.data <- melt(plotmatrix)
                                  if(variable=="zt"){
                                    ggplot(plt.data, aes(x=Var1, y=Var2, fill=value))+
                                      geom_raster()
                                  } else {
                                    ggplot(plt.data, aes(x=Var1, y=Var2, fill=value))+
                                      geom_raster() +
                                      scale_x_continuous(expand = c(0,0), limits = c(0,max(plt.data[,1]))) +
                                      scale_y_continuous(expand = c(0,0), limits = c(0,max(plt.data[,2])))
                                  }
                                },
                                correct_surface_fraction = function(){
                                  self$data$surface_fraction$vals[,,4]  <- NA
                                  lopv <- dim(self$data$surface_fraction$vals)
                                  for(fc in seq(lopv[1])){
                                    for(u in seq(lopv[2])){
                                      if(any(!is.na(self$data$surface_fraction$vals[fc,u,1:3]))){
                                        if(any(self$data$surface_fraction$vals[fc,u,1:3]>=0   )){
                                          self$data$surface_fraction$vals[fc,u,4]  <- 1
                                        }
                                      }
                                    }
                                  }
                                },
                                fill_areal = function(variabletofill, startx, starty, fillvalue,
                                                      valuetobefilled = NULL, boundaryarea = NULL,
                                                      overwrite = TRUE){


                                  areatofill2  <- self$data[[variabletofill]]$vals


                                  if(is.null(boundaryarea)){
                                    boundary.area2 <- areatofill2
                                    boundaryarea   <- variabletofill
                                  } else {
                                    boundary.area2 <- self$data[[boundaryarea]]$vals
                                  }

                                  if(is.null(valuetobefilled) & is.null(boundaryarea)){
                                    value.tobe.filled2  <-  self$data[[variabletofill]]$`_FillValue`
                                  } else if(!is.null(boundaryarea)){
                                    value.tobe.filled2  <-  self$data[[boundaryarea]]$`_FillValue`
                                  } else {
                                    value.tobe.filled2  <-  valuetobefilled
                                  }

                                  boundary_fill_temp <- self$data[[boundaryarea]]$`_FillValue`

                                  stackx <- startx
                                  stacky <- starty

                                  while(length(stackx) > 0){

                                    popx  <- stackx[length(stackx)]
                                    popy  <- stacky[length(stacky)]

                                    stackx <- stackx[-length(stackx)]
                                    stacky <- stacky[-length(stacky)]


                                    if(boundary.area2[popx,popy] == value.tobe.filled2){
                                      boundary.area2[popx,popy] <- boundary_fill_temp - 1
                                      if(overwrite){
                                        areatofill2[popx,popy]  <- fillvalue
                                      } else if(!overwrite){
                                        if(areatofill2[popx,popy]>0){

                                        } else {
                                          areatofill2[popx,popy]  <- fillvalue
                                        }

                                      }

                                      if(popy+1<=dim(areatofill2)[2]){
                                        stackx <- c(stackx, popx)
                                        stacky <- c(stacky, popy+1)
                                      }
                                      if(popy-1>0){
                                        stackx <- c(stackx, popx)
                                        stacky <- c(stacky, popy-1 )
                                      }
                                      if(popx+1<=dim(areatofill2)[1]){
                                        stackx <- c(stackx, popx+1)
                                        stacky <- c(stacky, popy)
                                      }
                                      if(popx-1>0){
                                        stackx <- c(stackx, popx-1)
                                        stacky <- c(stacky, popy )
                                      }

                                    }
                                  }
                                  if(all(boundary.area2!=areatofill2)){
                                    boundary.area2[boundary.area2==(boundary_fill_temp-1)] <- boundary_fill_temp
                                  }


                                  self$data[[variabletofill]]$vals <- areatofill2


                                },
                                generate_lai_array = function(dz, fixed_tree_height = NULL, alpha=5, beta=3,
                                                              startx = NULL, starty =NULL, lengthx = NULL,
                                                              lengthy = NULL, additional_array = NULL){

                                  # Erstellung eines 3D-arrays der leaf area density für 'Baumgruppen'.
                                  #
                                  # lai           - Leaf Area Index (Matrix)
                                  # canopy_height - Vegetationshöhe (Matrix)
                                  # dz 			- räumliche Auflösung in der Höhe
                                  # alpha, beta   - empirische Parameter nach
                                  #                 Markkanen et al. (2003): Footprints and Fetches for Fluxes
                                  #				  over Forest Canopies with varying Structure and Density.
                                  #				  Boundary-Layer Meteorology 106: 437-459
                                  #
                                  # Rückgabewert: lad_array[x,y,z]
                                  if(is.null(fixed_tree_height)){
                                    canopy_height1 <- array(NA,c(dim(self$data$vegetation_type$vals)[1],dim(self$data$vegetation_type$vals)[2]))
                                    canopy_height1[self$data$vegetation_type$vals==4] <- 20
                                    canopy_height1[self$data$vegetation_type$vals==5] <- 20
                                    canopy_height1[self$data$vegetation_type$vals==6] <- 20
                                    canopy_height1[self$data$vegetation_type$vals==7] <- 20
                                    canopy_height1[self$data$vegetation_type$vals==17] <- 20
                                    canopy_height1[self$data$vegetation_type$vals==18] <- 11
                                  } else{
                                    canopy_height1 <- 0
                                  }
                                  # im lazy
                                  if(!is.null(fixed_tree_height)){
                                    tree_height <- fixed_tree_height
                                  } else{
                                    tree_height <- 12 # Random number
                                  }


                                  if(any(is.null(startx),is.null(starty),is.null(lengthy),is.null(lengthx))){
                                    lai <- array(NA,c(dim(self$data$vegetation_type$vals)[1],dim(self$data$vegetation_type$vals)[2]))
                                    lai[self$data$vegetation_type$vals==4] <- 5
                                    lai[self$data$vegetation_type$vals==5] <- 5
                                    lai[self$data$vegetation_type$vals==6] <- 5
                                    lai[self$data$vegetation_type$vals==7] <- 6
                                    lai[self$data$vegetation_type$vals==17] <- 5
                                    lai[self$data$vegetation_type$vals==18] <- 2.5

                                    canopy_height <- array(NA,c(dim(self$data$vegetation_type$vals)[1],dim(self$data$vegetation_type$vals)[2]))
                                    canopy_height[self$data$vegetation_type$vals%in%c(4,5,6,7,17,18)] <- tree_height

                                  } else if( !any(names(self$data)=="lad")){
                                    lai <- array(NA,c(dim(self$data$vegetation_type$vals)[1],dim(self$data$vegetation_type$vals)[2]))
                                    lai[self$data$vegetation_type$vals==4] <- 5
                                    lai[self$data$vegetation_type$vals==5] <- 5
                                    lai[self$data$vegetation_type$vals==6] <- 5
                                    lai[self$data$vegetation_type$vals==7] <- 6
                                    lai[self$data$vegetation_type$vals==17] <- 5
                                    lai[self$data$vegetation_type$vals==18] <- 2.5

                                    canopy_height <- array(NA,c(dim(self$data$vegetation_type$vals)[1],dim(self$data$vegetation_type$vals)[2]))
                                    canopy_height[self$data$vegetation_type$vals%in%c(4,5,6,7,17,18)] <- tree_height

                                  } else {
                                    lai <- array(NA,c(dim(self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)])[1],dim(self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)])[2]))
                                    lai[self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)]==4] <- 5
                                    lai[self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)]==5] <- 5
                                    lai[self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)]==6] <- 5
                                    lai[self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)]==7] <- 6
                                    lai[self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)]==17] <- 5
                                    lai[self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)]==18] <- 2.5

                                    canopy_height <- array(NA,c(dim(self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)])[1],dim(self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)])[2]))
                                    canopy_height[self$data$vegetation_type$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1)]%in%c(4,5,6,7,17,18)] <- tree_height

                                  }
                                  if(length(dim(canopy_height1))>1){
                                    canopy_height <- canopy_height1
                                  }

                                  # hardcoded so far
                                  if(!is.null(additional_array)){
                                    ncfile <- nc_open(additional_array)

                                    data_height <- arcgischeck(
                                      ncvar_get(ncfile, "Band1"),
                                      self$arcgis)
                                    nc_close(ncfile)
                                    data_height <- round(data_height/dz)*dz
                                    data_height[data_height<=0] <- NA

                                    canopy_height <- data_height
                                    lai <- array(NA,c(dim(self$data$vegetation_type$vals)[1],dim(self$data$vegetation_type$vals)[2]))
                                    lai[canopy_height>=0]  <- 5
                                  }


                                  nx <- dim(canopy_height)[1]
                                  ny <- dim(canopy_height)[2]

                                  pch_index <- matrix(as.integer(canopy_height / dz), nrow=dim(canopy_height)[1], ncol=dim(canopy_height)[2])
                                  pch_index[pch_index == 0] <- NA # Wenn canopy_height < dz, dann NA setzen

                                  #############
                                  # Fix for no trees in vegetation file
                                  #############
                                  if(all(is.na(pch_index))){
                                    z <- c(0, dz*0.5)
                                    lad_array  <- array(-9999.9, c(dim(pch_index)[1:2],2))

                                    adata        <- list("long_name" = "zlad",
                                                         "standard_name" = "zlad",
                                                         "units" = "m",
                                                         "vals" = z)

                                    self$dims[["zlad"]] <- adata

                                    adata      <- list("_FillValue" = -9999.9,
                                                       "units" = "m2/m3",
                                                       "long_name" = "leaf area density",
                                                       "source" = "Script by Dirk Pavlik after ncl script by Bjoern Maronga",
                                                       "vals" = lad_array,
                                                       "type" = "float")
                                    self$data[["lad"]] <- adata
                                    self$vardimensions[["lad"]]  <- c("x", "y", "zlad")

                                  } else {
                                  z <- tryCatch({seq(0, max(pch_index,na.rm=TRUE),by=1) * dz},
                                  warning = function(w){
                                    print("Warning in seq line 1557")
                                  }, error = function(e){
                                    print("Error in seq line 1557")
                                  } )
                                  z <- z - (dz/2)
                                  z[1] <- 0

                                  pre_lad <- rep(NA,length(z))

                                  lad_array <- array(NA, c(dim(canopy_height)[1],dim(canopy_height)[2],length(z)))

                                  for (i in 1:nx){
                                    for(j in 1:ny){

                                      # DEBUG ###########
                                      # i <-2
                                      # j <- 3
                                      ###################

                                      #cat("i =",i,"j =",j, "\n")

                                      int_bpdf <- 0
                                      ch <- canopy_height[i,j]
                                      if(!is.na(pch_index[i,j]) & ch >= dz){ # if(!is.na(ch) & ch >= 0.5*dz) <- dies war die originale Abfrage, funktioniert jedoch nicht bei canopy_height < dz !!!
                                        for(k in 1:(pch_index[i,j]+1)){
                                          int_bpdf <- int_bpdf + ((( z[k] / canopy_height[i,j])^( alpha - 1 )) * (( 1.0 - ( z[k] / canopy_height[i,j] ) )^(beta - 1 ) ) * ( dz / canopy_height[i,j] ) )
                                          #cat("int_bpdf =",int_bpdf,"\n")
                                        }

                                        for(k in 1:(pch_index[i,j]+1)){
                                          pre_lad[k] <- lai[i,j] * ( ( ( dz*(k-1) / canopy_height[i,j] )^( alpha - 1 ) ) * ( ( 1.0 - ( dz*(k-1) / canopy_height[i,j] ) )^(beta - 1 ) ) / int_bpdf ) / canopy_height[i,j]
                                          #cat("pre_lad[",k,"] =",pre_lad[k],"\n")
                                        }

                                        lad_array[i,j,1] <- pre_lad[1]
                                        for(k in 2:(pch_index[i,j]+1)){
                                          lad_array[i,j,k] <- 0.5 * ( pre_lad[k-1] + pre_lad[k] )
                                          #cat("lad_array[",i,",",j,", ] =",lad_array[i,j,],"\n")
                                        }
                                      }
                                    }
                                  }


                                  lad_array[is.na(lad_array)] <- -9999.9
                                  if(!is.null(additional_array) && any(names(self$data)=="lad")){

                                    if(dim(lad_array)[3]>dim(self$data$lad$vals)[3]){

                                      new_array <- array(-9999.9, c(dim(self$data$lad$vals)[1:2], dim(lad_array)[3]))
                                      new_array[,,1:dim(self$data$lad$vals)[3]][self$data$lad$vals>=0] <- self$data$lad$vals[self$data$lad$vals>=0]
                                      new_array[lad_array>0] <- lad_array[lad_array>0]

                                      adata        <- list("long_name" = "zlad",
                                                           "standard_name" = "zlad",
                                                           "units" = "m",
                                                           "vals" = z)

                                      self$dims[["zlad"]] <- adata
                                    } else{
                                      new_array <- array(-9999.9, c(dim(self$data$lad$vals)[1:2], dim(lad_array)[3]))
                                      new_array[,,1:dim(self$data$lad$vals)[3]][self$data$lad$vals>=0] <- self$data$lad$vals[self$data$lad$vals>=0]
                                      new_array[lad_array>0] <- lad_array[lad_array>0]
                                    }
                                    self$data$lad$vals<- new_array
                                  } else if(any(is.null(startx),is.null(starty),is.null(lengthy),is.null(lengthx))){
                                    adata        <- list("long_name" = "zlad",
                                                         "standard_name" = "zlad",
                                                         "units" = "m",
                                                         "vals" = z)

                                    self$dims[["zlad"]] <- adata


                                    adata      <- list("_FillValue" = -9999.9,
                                                       "units" = "m2/m3",
                                                       "long_name" = "leaf area density",
                                                       "source" = "Script by Dirk Pavlik after ncl script by Bjoern Maronga",
                                                       "vals" = lad_array,
                                                       "type" = "float")
                                    self$data[["lad"]] <- adata

                                    self$vardimensions[["lad"]]  <- c("x", "y", "zlad")

                                  } else if(!any(names(self$data)=="lad")){
                                    adata        <- list("long_name" = "zlad",
                                                         "standard_name" = "zlad",
                                                         "units" = "m",
                                                         "vals" = z)

                                    self$dims[["zlad"]] <- adata


                                    adata      <- list("_FillValue" = -9999.9,
                                                       "units" = "",
                                                       "long_name" = "lad",
                                                       "source" = "Script by Dirk Pavlik after ncl script by Bjoern Maronga",
                                                       "vals" = lad_array,
                                                       "type" = "float")
                                    self$data[["lad"]] <- adata

                                    self$vardimensions[["lad"]]  <- c("x", "y", "zlad")
                                  } else {
                                    if(dim(lad_array)[3]>dim(self$data$lad$vals)[3]){
                                      new_array <- array(-9999.9, c(dim(self$data$lad$vals)[1:2], dim(lad_array)[3]))
                                      new_array[,,1:dim(self$data$lad$vals)[3]][self$data$lad$vals>0] <- self$data$lad$vals[self$data$lad$vals>0]
                                      new_array[startx:(startx+lengthx-1),starty:(starty+lengthy-1),][lad_array>0] <- lad_array[lad_array>0]

                                      self$data$lad$vals<- new_array

                                      adata        <- list("long_name" = "zlad",
                                                           "standard_name" = "zlad",
                                                           "units" = "m",
                                                           "vals" = z)

                                      self$dims[["zlad"]] <- adata

                                    } else{
                                      self$data$lad$vals[startx:(startx+lengthx-1),starty:(starty+lengthy-1),][lad_array>0] <- lad_array[lad_array>0]
                                    }

                                  }
                                  }
                                },
                                import_data = function(v.file , palmtype, typeid, street= FALSE){
                                  # if(palmtype!=listofvariablesforpalm
                                  if(self$oldversion){
                                    checkvar <- "buildings_2D"
                                  } else {
                                    checkvar <- "buildings_2d"
                                  }

                                  if(palmtype==checkvar){
                                    dtype  <- "float"
                                    fillvalue <- -9999.9
                                  } else {
                                    dtype  <- "byte"
                                    fillvalue <- -127
                                  }




                                  ncfile <- nc_open(v.file)
                                  dimen <- list()

                                  for(zz in seq(ncfile$ndims)){
                                    vec         <- ncvar_get(ncfile, names(ncfile$dim)[zz])
                                    attr        <- ncatt_get(ncfile, names(ncfile$dim)[zz])
                                    adata <- list()
                                    for(ii in seq(attr)){
                                      adata[[names(attr)[ii]]]   <-attr[[ii]]
                                    }
                                    adata[["vals"]]= vec

                                    dimen[[names(ncfile$dim)[zz]]]     <- adata

                                  }
                                  if(any(names(dimen)=="lon")){
                                    names(dimen)[names(dimen)=="lon"] <- "x"
                                    names(dimen)[names(dimen)=="lat"] <- "y"
                                  }



                                  if(!all(names(dimen) %in% names(self$dims))){
                                    newdimensions <- which(!(names(dimen) %in% names(self$dims)))
                                    self$dims[[names(dimen)[newdimensions]]] <- dimen[[newdimensions]]
                                  }
                                  dat <- list()
                                  whichdimensions <- list()


                                  vec         <- arcgischeck(
                                    ncvar_get(ncfile, "Band1"),
                                    self$arcgis)
                                  vec[is.na(vec)]  <- fillvalue
                                  attr        <- ncatt_get(ncfile, "Band1")
                                  if(max(vec, na.rm = T)==1){
                                    vec[vec>0]  <- typeid
                                  }
                                  # } else if(any(vec<0)) {
                                  #   vec[vec<0]   <- typeid
                                  # }
                                  if(is.null(attr$units)){
                                    attr$units <- ""
                                  }


                                  if(any(names(self$data)==palmtype)){
                                    vec2 <- self$data[[palmtype]]$vals
                                    vec2[vec!=0] <- vec[vec!=0]
                                    vec <- vec2
                                  }
                                  if(street==TRUE & any(names(self$data)==checkvar) ){
                                    ###########
                                    # THRESHOLD FOR BUILDINGS
                                    th <- 4    # 4x5 = 20m Abstand von Straßenachse zu gebäude
                                    newvec   <- array(0,dim=dim(vec))
                                    #loop through all dimensions
                                    for(i in seq(dim(vec)[1])){
                                      for(j in seq(dim(vec)[2])){
                                        #if pixel is not zero
                                        if(vec[i,j]!=0){
                                          newvec[i,j] <- typeid
                                        }
                                        if(vec[i,j]!=0 & (i>th & j>th) &  (i<(dim(vec)[1]-th) & (j<dim(vec)[2]-th)) ){
                                          # and pixel in buildings_2D array within threshhold are not zero
                                          if(any(self$data[[checkvar]]$vals[(i-th):i,(j-th):j])!=0){
                                            newvec[(i-th):i,(j-th):j] <- typeid
                                          }
                                          if(any(self$data[[checkvar]]$vals[(i-th):i,j:(j+th)])!=0){
                                            newvec[(i-th):i,j:(j+th)] <- typeid
                                          }
                                          if(any(self$data[[checkvar]]$vals[i:(i+th),(j-th):j])!=0){
                                            newvec[i:(i+th),(j-th):j] <- typeid
                                          }
                                          if(any(self$data[[checkvar]]$vals[i:(i+th),j:(j+th)])!=0){
                                            newvec[i:(i+th),j:(j+th)] <- typeid
                                          }
                                        }

                                      }
                                    }
                                    newvec[self$data[[checkvar]]$vals!=0] <- 0
                                    vec <- newvec
                                  }

                                  vec[vec==0] <- fillvalue

                                  adata        <- list("_FillValue" = fillvalue,
                                                       "units" = attr$units,
                                                       "long_name" = gsub("_", " ",palmtype),
                                                       "source" = "Munich QGIS DATA",
                                                       "vals" = vec,
                                                       "type" = dtype,
                                                       "res_orig" = attr$res_orig,
                                                       "lod" = 1)
                                  #dat[[palmtype]]     <- adata
                                  whichdimensions[[palmtype]]  <- ncfile$var$Band1$dimids + 1

                                  whichdims <- which(names(self$dims) %in% names(dimen))

                                  self$data[[palmtype]]  <- adata
                                  self$vardimensions[[palmtype]]  <- whichdims
                                },
                                SortOverlayingdata = function(inorderof = "BPWV"){
                                  if(self$oldversion){
                                    checkvar <- "buildings_2D"
                                  } else {
                                    checkvar <- "buildings_2d"
                                  }


                                  multidimarray <- array(0,c(dim(self$data$pavement_type$vals),4))
                                  multidimarray[,,1]  <- self$data[[checkvar]]$vals
                                  multidimarray[,,2]  <- self$data$pavement_type$vals
                                  multidimarray[,,3]  <- self$data$water_type$vals
                                  multidimarray[,,4]  <- self$data$vegetation_type$vals

                                  arrayorder          <- c("B","P","W","V")
                                  throwout            <- c(1,2,3,4)
                                  outerfillv          <- c(0,-127,-127,-127)

                                  loopvar <- unlist(strsplit(inorderof,split=""))

                                  # check if first entry of table is always "fill_value!"
                                  for(i in seq(loopvar)){
                                    whicharray  <- which(loopvar[i]==arrayorder)
                                    fillv       <- as.numeric(names(table(multidimarray[,,whicharray])[1]))
                                    wherestuff  <- which(multidimarray[,,whicharray]!=fillv,arr.ind = T)
                                    throwout    <- throwout[-which(throwout==whicharray)]
                                    for(j in throwout){
                                      bridge_1             <- multidimarray[,,j]
                                      bridge_1[wherestuff] <- outerfillv[j]
                                      multidimarray[,,j]   <- bridge_1
                                    }
                                  }
                                  self$data[[checkvar]]$vals    <- multidimarray[,,1]
                                  # self$data$building_id$vals[self$data$building_2d<0] <- -127
                                  self$data$pavement_type$vals   <- multidimarray[,,2]
                                  self$data$water_type$vals      <- multidimarray[,,3]
                                  self$data$vegetation_type$vals <- multidimarray[,,4]

                                },
                                countemptyfields = function(){
                                  if(self$oldversion){
                                    checkvar <- "buildings_2D"
                                  } else {
                                    checkvar <- "buildings_2d"
                                  }
                                  NAarray <- array(-20,dim(self$data[[checkvar]]$vals))
                                  NAarray[self$data[[checkvar]]$vals>0] <- self$data[[checkvar]]$vals[self$data[[checkvar]]$vals>0]
                                  NAarray[self$data$pavement_type$vals>0] <- self$data$pavement_type$vals[self$data$pavement_type$vals>0]
                                  NAarray[self$data$water_type$vals>0] <- self$data$water_type$vals[self$data$water_type$vals>0]
                                  NAarray[self$data$vegetation_type$vals>0] <- self$data$vegetation_type$vals[self$data$vegetation_type$vals>0]
                                  print(table(NAarray)[1])
                                },
                                addsoilandsurfacefraction = function(){
                                  soiltype   <- self$data$vegetation_type$vals
                                  soiltype[which(self$data$pavement_type$vals>0, arr.ind = T)]   <- 1
                                  soiltype[soiltype>0]  <- 1
                                  adata      <- list("_FillValue" = -127,
                                                     "units" = "",
                                                     "long_name" = "soil type classification",
                                                     # "long_name" = "soil type",
                                                     "source" = "First Guess",
                                                     "vals" = soiltype,
                                                     "type" = "byte")
                                  self$data$soil_type  <- adata
                                  self$vardimensions$soil_type <-  c(1,2)


                                  xvec         <- c(0,1,2)

                                  adata        <- list("vals" = xvec)
                                  #"type" = "float")
                                  self$dims$nsurface_fraction      <- adata


                                  surfacefraction   <- array(NA, c(dim(self$data$vegetation_type$vals),4))
                                  temp_array        <- array(-9999.9, dim(self$data$vegetation_type$vals))
                                  temp_array[which(self$data$vegetation_type$vals>0, arr.ind = T)]   <- 1
                                  surfacefraction[,,1]  <- temp_array
                                  #surfacefraction[,,4]  <- temp_array

                                  temp_array        <- array(-9999.9, dim(self$data$vegetation_type$vals))
                                  temp_array[which(self$data$pavement_type$vals>0, arr.ind = T)]   <- 1
                                  #temp_array2       <- temp_array
                                  #temp_array2[surfacefraction[,,4]==1] <- 1
                                  #surfacefraction[,,4]  <- temp_array2
                                  surfacefraction[,,2]  <- temp_array

                                  temp_array        <- array(-9999.9, dim(self$data$vegetation_type$vals))
                                  temp_array[which(self$data$water_type$vals>0, arr.ind = T)]   <- 1
                                  # temp_array2       <- temp_array
                                  # temp_array2[surfacefraction[,,4]==1] <- 1
                                  # surfacefraction[,,4]  <- temp_array2
                                  surfacefraction[,,3]  <- temp_array

                                  surfacefraction[,,2][which(surfacefraction[,,1]==1)] <- 0
                                  surfacefraction[,,3][which(surfacefraction[,,1]==1)] <- 0
                                  surfacefraction[,,1][which(surfacefraction[,,2]==1)] <- 0
                                  surfacefraction[,,3][which(surfacefraction[,,2]==1)] <- 0
                                  surfacefraction[,,2][which(surfacefraction[,,3]==1)] <- 0
                                  surfacefraction[,,1][which(surfacefraction[,,3]==1)] <- 0
                                  surfacefraction[,,4]  <-  surfacefraction[,,1] + surfacefraction[,,2]+ surfacefraction[,,3]
                                  surfacefraction[,,4][which(surfacefraction[,,4]<0)] <- -9999.9


                                  adata      <- list("_FillValue" = -9999.9,
                                                     "units" = "",
                                                     # "long_name" = "surface fraction",
                                                     "long_name" = "surface tile fraction",
                                                     "vals" = surfacefraction[,,1:3],
                                                     "type" = "float")


                                  self$data$surface_fraction  <- adata
                                  self$vardimensions$surface_fraction  <- c(1,2,which(names(self$dims)=="nsurface_fraction"))

                                },
                                add_lod2_variable = function(name, v.data = NULL){
                                  lod <- NULL
                                  if(grepl("water", name)){
                                    varname <- "water_pars"
                                    dimname <- "nwater_pars"
                                    vardim_dim <- seq(0, 6)
                                    longname <- "water parameters"
                                    dattype  <- "float"
                                  } else if(grepl("vegetation", name)){
                                    varname <- "vegetation_pars"
                                    dimname <- "nvegetation_pars"
                                    vardim_dim <- seq(0, 11)
                                    longname <- "vegetation parameters"
                                    dattype  <- "float"
                                  } else if(grepl("pavement", name)){
                                    varname <- "pavement_pars"
                                    dimname <- "npavement_pars"
                                    vardim_dim <- seq(0, 3)
                                    longname <- "pavement parameters"
                                    dattype  <- "float"
                                  } else if(grepl("soil", name)){
                                    varname <- "soil_pars"
                                    dimname <- "nsoil_pars"
                                    vardim_dim <- seq(0, 7)
                                    longname <- "soil parameters"
                                    dattype  <- "float"
                                    lod <- 2
                                  } else if(grepl("building", name)){
                                    varname <- "building_pars"
                                    dimname <- "nbuilding_pars"
                                    vardim_dim <- seq(0, 46)
                                    longname <- "building parameters"
                                    dattype  <- "float"
                                  }
                                  dimdata        <- list("long_name" = dimname,
                                                         "standard_name" = dimname,
                                                         "units" = "1",
                                                         "vals" = vardim_dim)
                                  self$dims[[dimname]] <- dimdata
                                  self$vardimensions[[varname]] <- c("x", "y", dimname)

                                  if(is.null(v.data)){
                                    valvec <- array(-9999.9,c(
                                      dim(self$data$zt$vals)[1],
                                      dim(self$data$zt$vals)[2],
                                      length(vardim_dim)
                                    ))
                                  } else if(dim(v.data)[1] != dim(self$data$zt$vals)[1] |
                                            dim(v.data)[2] != dim(self$data$zt$vals)[2] |
                                            dim(v.data)[3] != length(vardim_dim)){
                                    valvec <- array(-9999.9,c(
                                      dim(self$data$zt$vals)[1],
                                      dim(self$data$zt$vals)[2],
                                      length(vardim_dim)
                                    ))
                                  } else {
                                    valvec <- v.data
                                  }
                                  if(is.null(lod)){
                                    adata     <- list("_FillValue" = -9999.9,
                                                      "units" = "1",
                                                      "long_name" = longname,
                                                      # "long_name" = "building id",
                                                      "source" = "idk made it up",
                                                      "vals" = valvec,
                                                      "type" = dattype)
                                  } else {
                                    adata     <- list("_FillValue" = -9999.9,
                                                      "units" = "1",
                                                      "long_name" = longname,
                                                      # "long_name" = "building id",
                                                      "source" = "idk made it up",
                                                      "lod" = lod,
                                                      "vals" = valvec,
                                                      "type" = dattype)
                                  }
                                  self$data[[varname]]  <- adata

                                },
                                print = function(...){
                                "Sophisticated print function TBD"
                                }
                              ),

                              private = list(
                              )
)



# Vars i have in superclass:
# dims           = NULL,
# data           = NULL,
# exportname     = NULL,
# header         = NULL,
# path           = NULL,

# savedplots     = NULL,
# plotcntr       = NULL,



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
                                  ncfile  <- nc_open(self$source)


                                  #####
                                  # Recreate Header Class for Global Attributes
                                  globattr <- ncatt_get(ncfile,0)
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
                                    dummy_obj$head[[which(names(dummy_obj$head)==tt)]]  <- ncatt_get(ncfile,0,tt)$value
                                  }
                                  self$header  <- dummy_obj

                                  #####
                                  # Get Dimensions
                                  # x
                                  dimen        <- list()

                                  for(zz in seq(ncfile$ndims)){
                                    vec         <- ncvar_get(ncfile, names(ncfile$dim)[zz])
                                    attr        <- ncatt_get(ncfile, names(ncfile$dim)[zz])
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
                                    vec         <- ncvar_get(ncfile, names(ncfile$var)[zz])
                                    attr        <- ncatt_get(ncfile, names(ncfile$var)[zz])
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
                                }

                              )
)

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

palm_ncdf_manual   <- R6::R6Class("palm_ncdf_manual",
                             inherit = palm_ncdf_berlin,
                             public = list(
                               source = NULL,
                               vardimensions = NULL,
                               oldversion = NULL,
                               plotcntr = 0,
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





