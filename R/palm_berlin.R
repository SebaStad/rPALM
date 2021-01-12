palm_ncdf_berlin <- R6::R6Class("palm_ncdf_berlin",
  public = list(
    dims = NULL,
    data = NULL,
    exportname = NULL,
    header = NULL,
    path = NULL,

    arcgis = FALSE,

    savedplots = NULL,
    plotcntr = NULL,

    vardimensions = NULL,
    oldversion = NULL,

    #' Funktion die mit $new aufgerufen wird!
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
    #' #berlin_headm <- palm_global$new(title = "GIT Example",
    #' #author = "sest",
    #' #institute = "IBP",
    #' #location = "Hoki",
    #' #x0 = 0,   # only important for visualization on a map later on
    #' #y0 = 0,   # only important for visualization on a map later on
    #' #z0 = 0,
    #' #t0 = "2018-06-21 12:00:00 +00", # Character with Date in this format,
    #' # might be important to have correct in later releases of PALM!
    #' #lat = 52.502302,   # important for solar radiation
    #' #lon = 13.364862,   # important for solar radiation
    #' #dx = 5)
    #'
    #' berlin_example <- palm_ncdf_berlin$new(filename = "berlin_static.nc",
    #'                    headclass = "berlin_head",
    #'                    pathtofiles = "Path/to/Berlin/data",
    #'                    oldversion = FALSE)
    initialize = function(filename, headclass, pathtofiles, oldversion = FALSE) {
      if (oldversion) {
        self$oldversion <- TRUE
      } else {
        self$oldversion <- FALSE
      }
      # filename:    Zukuenftiger Name der ausgegebenen static_driver datei
      # headclass:   Object der Klasse palm_global
      # pathtofiles: Pfad zu Berlin netCDF Dateien

      if (rPALM:::substrRight(filename, 3) != ".nc") {
        filename <- paste(filename, ".nc", sep = "")
      }
      self$exportname <- filename
      self$header <- headclass
      self$path <- pathtofiles
      self$savedplots <- list()
      self$plotcntr <- 0
    },
    # Import der vorhanden Berlin Daten
    #' Title
    #'
    #' @param lengthx Gridpoints in x direction
    #' @param lengthy Gridpoints in y direction
    #' @param dx Grid spacing
    #'
    #' @return Imports data into PALM class for relevant PALM-4U data.
    #' @export
    #'
    #' @examples
    #' berlin_example$importfiles(lengthx = 100,
    #'                            lengthy = 100,
    #'                            dx = 10)
    importfiles = function(lengthx, lengthy, dx, clear_overlap = FALSE) {

      # lengthx:  LÃ¤nge der Domain in x-Richtung
      # lengthy:  LÃ¤nge der Domain in y-Richtung
      # dx:       Gridabstand. FÃ¼r Berlin 1 oder 10

      setwd(self$path)

      files <- list.files(pattern = paste(dx, "m", sep = ""))
      self$header$head$resolution <- dx

      dat <- list()
      dimen <- list()

      # dat:   Liste mit allen Variablen und Werten
      # dimen: Liste mit allen Dimensionen

      # Ab hier: Oeffnen aller Dateien und Auslesen der Werte + Speichern in der dat-Liste

      #####
      # Terrain height
      ncfile <- ncdf4::nc_open(files[grep("terrain_height", files)])

      xvec <- tryCatch(
        {
          seq(dx * 0.5, lengthx * dx, by = dx)
        },
        warning = function(w) {
          print("Warning in seq line 126")
        },
        error = function(e) {
          print("Error in seq line 126")
        }
      )
      yvec <- tryCatch(
        {
          seq(dx * 0.5, lengthy * dx, by = dx)
        },
        warning = function(w) {
          print("Warning in seq line 131")
        },
        error = function(e) {
          print("Error in seq line 131")
        }
      )

      orogr <- ncdf4::ncvar_get(ncfile, "Band1",
        start = c(self$header$head$origin_x, self$header$head$origin_y),
        c(lengthx, lengthy)
      )
      self$header$head$origin_z <- min(orogr)
      orogr <- orogr - min(orogr)

      ######
      # Coordinates
      # X
      adata <- list(
        "long_name" = "x",
        "standard_name" = "x",
        "units" = "m",
        "vals" = xvec
      )
      # "type" = "float")
      dimen$x <- adata

      # Y
      adata <- list(
        "long_name" = "y",
        "standard_name" = "y",
        "units" = "m",
        "vals" = yvec
      )
      # "type" = "float")
      dimen$y <- adata
      ######
      # Orography
      orogr <- floor(orogr / dx) * dx
      adata <- list(
        "_FillValue" = -9999.9,
        "units" = "m",
        "long_name" = "terrain_height",
        # "long_name" = "orography",
        "res_orig" = dx,
        "source" = "Atkis DGM",
        "vals" = orogr,
        "type" = "float"
      )
      if (self$oldversion) {
        dat$orography_2D <- adata
      } else {
        dat$zt <- adata
      }

      ncdf4::nc_close(ncfile)
      ######
      # Buildings
      # 2D
      ncfile <- ncdf4::nc_open(files[grep("building_height", files)])
      build <- ncdf4::ncvar_get(ncfile, "Band1",
        start = c(self$header$head$origin_x, self$header$head$origin_y),
        c(lengthx, lengthy)
      )
      build <- floor(build / dx) * dx
      build[which(is.na(build), arr.ind = T)] <- -9999

      adata <- list(
        "_FillValue" = -9999.9,
        "units" = "m",
        "long_name" = "building",
        "res_origin" = dx,
        "source" = "CityGML",
        "lod" = 1,
        "vals" = build,
        "type" = "float"
      )
      if (self$oldversion) {
        dat$buildings_2D <- adata
      } else {
        dat$buildings_2d <- adata
      }


      # 3D
      zmax <- max(build, na.rm = T)
      if (zmax <= 0) {
        z <- 0
        zmax <- 0
      } else {
        z <- tryCatch(
          {
            seq(0, zmax, by = dx)
          },
          warning = function(w) {
            print("Warning in seq line 205")
          },
          error = function(e) {
            print("Error in seq line 205")
          }
        )
        z <- z - 0.5 * dx
        z[1] <- 0
      }

      adata <- list(
        "long_name" = "z",
        "standard_name" = "z",
        "units" = "m",
        "vals" = z
      )
      # "type" = "float")
      dimen$z <- adata

      self$dims <- dimen

      build3d <- array(0, dim = c(dim(build), zmax / dx + 1))
      for (i in seq(dim(build3d)[1])) {
        for (j in seq(dim(build3d)[2])) {
          if (build[i, j] > 0) {
            bheight <- build[i, j] / dx
            build3d[i, j, 1:bheight] <- 1
          }
        }
      }

      adata <- list(
        "_FillValue" = -127,
        "units" = "m",
        "long_name" = "building_flag",
        # "long_name" = "buildings_3",
        "res_origin" = dx,
        "source" = "CityGML",
        "lod" = 2,
        "vals" = build3d,
        "type" = "byte"
      )

      if (self$oldversion) {
        dat$buildings_3D <- adata
      } else {
        dat$buildings_3d <- adata
      }
      ncdf4::nc_close(ncfile)

      # ID's
      ncfile <- ncdf4::nc_open(files[grep("building_id", files)])
      buildid <- ncdf4::ncvar_get(ncfile, "Band1",
        start = c(self$header$head$origin_x, self$header$head$origin_y),
        c(lengthx, lengthy)
      )
      adata <- list(
        "_FillValue" = -9999.9,
        "units" = "",
        # "long_name" = "building id",
        "long_name" = "building id numbers",
        "res_origin" = dx,
        "source" = "",
        "vals" = buildid,
        "type" = "integer"
      )

      dat$building_id <- adata
      ncdf4::nc_close(ncfile)

      # Type's
      ncfile <- ncdf4::nc_open(files[grep("building_type", files)])
      buildtype <- ncdf4::ncvar_get(ncfile, "Band1",
        start = c(self$header$head$origin_x, self$header$head$origin_y),
        c(lengthx, lengthy)
      )
      adata <- list(
        "_FillValue" = -127,
        "units" = "",
        "long_name" = "building type classification",
        # "long_name" = "building type",
        "res_origin" = dx,
        "source" = "",
        "vals" = buildtype,
        "type" = "integer"
      )

      dat$building_type <- adata
      ncdf4::nc_close(ncfile)

      #####
      # Vegetation
      # Type
      ncfile <- ncdf4::nc_open(files[grep("vegetation_type", files)])
      vegtype <- ncdf4::ncvar_get(ncfile, "Band1",
        start = c(self$header$head$origin_x, self$header$head$origin_y),
        c(lengthx, lengthy)
      )

      vegtype[which(is.na(vegtype), arr.ind = T)] <- 3
      adata <- list(
        "_FillValue" = -127,
        "units" = "",
        "long_name" = "vegetation_type",
        "res_origin" = dx,
        "source" = "DLR Satellite",
        "vals" = vegtype,
        "type" = "byte"
      )

      dat$vegetation_type <- adata
      ncdf4::nc_close(ncfile)

      #####
      # Street
      # Type
      ncfile <- ncdf4::nc_open(files[grep("street_type", files)])
      streettype <- ncdf4::ncvar_get(ncfile, "Band1",
        start = c(self$header$head$origin_x, self$header$head$origin_y),
        c(lengthx, lengthy)
      )


      adata <- list(
        "_FillValue" = -127,
        "units" = "",
        "long_name" = "street_type",
        "res_origin" = dx,
        "source" = "",
        "vals" = streettype,
        "type" = "byte"
      )

      dat$street_type <- adata
      ncdf4::nc_close(ncfile)

      # Crossing
      # ncfile        <- ncdf4::nc_open(files[grep("street_crossing", files)])
      # streetcross   <- ncdf4::ncvar_get(ncfile, "Band1", start = c(self$header$head$origin_x,self$header$head$origin_y),
      #                           c(lengthx, lengthy))

      # adata      <- list("_FillValue" = -127,
      #                   "units" = "",
      #                   "long_name" = "street_type",
      #                   "res_origin" = dx,
      #                   "source" = "",
      #                   "vals" = streetcross,
      #                   "type" = "byte")

      # dat$street_crossing  <- adata
      # ncdf4::nc_close(ncfile)

      #####
      # Water
      # Type
      ncfile <- ncdf4::nc_open(files[grep("water_type", files)])
      watertype <- ncdf4::ncvar_get(ncfile, "Band1",
        start = c(self$header$head$origin_x, self$header$head$origin_y),
        c(lengthx, lengthy)
      )

      adata <- list(
        "_FillValue" = -127,
        "units" = "",
        "long_name" = "water_type",
        "source" = "DLR Satellite",
        "vals" = watertype,
        "type" = "byte"
      )

      dat$water_type <- adata
      ncdf4::nc_close(ncfile)

      #####
      # Pavement
      # Type
      ncfile <- ncdf4::nc_open(files[grep("pavement_type", files)])
      pavementtype <- ncdf4::ncvar_get(ncfile, "Band1",
        start = c(self$header$head$origin_x, self$header$head$origin_y),
        c(lengthx, lengthy)
      )

      adata <- list(
        "_FillValue" = -127,
        "units" = "",
        "long_name" = "pavement_type",
        "source" = "OpenStreetMap",
        "vals" = pavementtype,
        "type" = "byte"
      )

      dat$pavement_type <- adata
      ncdf4::nc_close(ncfile)

      #######
      # CHECK FOR OVERLAPPING DATA
      # VEGETATION
      if(clear_overlap){
        dat$vegetation_type$vals[which(!is.na(dat$pavement_type$vals), arr.ind = T)] <- NA
        dat$vegetation_type$vals[which(!is.na(dat$building_type$vals), arr.ind = T)] <- NA
        dat$vegetation_type$vals[which(!is.na(dat$water_type$vals), arr.ind = T)] <- NA
        # PAVEMENT
        dat$pavement_type$vals[which(!is.na(dat$building_type$vals), arr.ind = T)] <- NA
        dat$pavement_type$vals[which(!is.na(dat$water_type$vals), arr.ind = T)] <- NA
        # WATER
        dat$water_type$vals[which(!is.na(dat$building_type$vals), arr.ind = T)] <- NA


        # consistency check
        consistency_check <- array(TRUE, dim = c(lengthx, lengthy))

        consistency_check[which(!is.na(dat$pavement_type$vals), arr.ind = T)] <- FALSE
        consistency_check[which(!is.na(dat$building_type$vals), arr.ind = T)] <- FALSE
        consistency_check[which(!is.na(dat$water_type$vals), arr.ind = T)] <- FALSE
        consistency_check[which(!is.na(dat$vegetation_type$vals), arr.ind = T)] <- FALSE

        # Dummy variable, see original ncl file
        dat$vegetation_type$vals[consistency_check] <- 2
      }


      #####
      # Soil type
      soiltype <- dat$vegetation_type$vals
      soiltype[which(!is.na(dat$pavement_type$vals), arr.ind = T)] <- 1

      adata <- list(
        "_FillValue" = -127,
        "units" = "",
        # "long_name" = "soil type",
        "long_name" = "soil type classification",
        "source" = "First Guess",
        "vals" = soiltype,
        "type" = "byte"
      )
      dat$soil_type <- adata


      self$data <- dat
    },
    # Export der netCDF Datei
    #' Title
    #'
    #' Exports a static driver with the data present in the class
    #'
    #' @param Path Path where the driver is to be saved
    #' @param EPSGCode Necessary for correct display on maps. Currently only
    #' usable for few coordinate systems
    #'
    #' @return Exports a static driver
    #' @export
    #'
    #' @examples
    #' berlin_example$exportncdf(Path = "Path/to/be/saved"
    #'                           EPSGCode = "EPSG:25833")
    exportncdf = function(Path = self$path, EPSGCode = "EPSG:25833") {
      ###########
      # GUI ANPASSUNG FueR EPSG
      ###########
      # HARDCODED!
      #
      if (EPSGCode == "EPSG:25831") {
        centralmeridian <- 3.0
      } else if (EPSGCode == "EPSG:25832") {
        centralmeridian <- 9.0
      } else if (EPSGCode == "EPSG:25833") {
        centralmeridian <- 15.0
      } else if (EPSGCode == "EPSG:31468") {
        centralmeridian <- 12.0
      } else {
        centralmeridian <- 15.0
      }



      ###########
      # PIDS 1.9
      ###########
      # CRS Zeugs
      ###########
      if (any(names(self$data) == "crs")) {
        self$data$crs <- NULL
      }
      adata <- list(
        "long_name" = "coordinate reference system",
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
        "type" = "integer"
      )
      self$data[["crs"]] <- adata
      self$vardimensions[["crs"]] <- c()

      eastval <- seq(
        self$header$head$origin_x,
        self$header$head$origin_x +
          self$header$head$resolution * (length(self$dims$x$vals) - 1),
        self$header$head$resolution
      )
      adata <- list(
        "long_name" = "easting",
        "standard_name" = "projection_x_coordinate",
        "units" = "m",
        "vals" = eastval,
        "type" = "float"
      )
      self$data[["E_UTM"]] <- adata
      self$vardimensions[["E_UTM"]] <- c("x")

      westval <- seq(
        self$header$head$origin_y,
        self$header$head$origin_y +
          self$header$head$resolution * (length(self$dims$y$vals) - 1),
        self$header$head$resolution
      )
      adata <- list(
        "long_name" = "northing",
        "standard_name" = "projection_y_coordinate",
        "units" = "m",
        "vals" = westval,
        "type" = "float"
      )
      self$data[["N_UTM"]] <- adata
      self$vardimensions[["N_UTM"]] <- c("y")

      e_utm <- eastval
      n_utm <- westval

      df <- as.data.frame(expand.grid(e_utm, n_utm))

      sputm <- sp::SpatialPoints(df, proj4string = CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")) # Defining Gauss Krueger)
      spgeo <- sp::spTransform(sputm, CRS("+proj=longlat +datum=WGS84 +no_defs"))

      thedata <- round(as.data.frame(spgeo), 6)

      longitude_mat <- array(thedata[, 1], c(length(e_utm), length(n_utm)))
      latitude_mat <- array(thedata[, 2], c(length(e_utm), length(n_utm)))

      adata <- list(
        "long_name" = "latitude",
        "standard_name" = "latitude",
        "units" = "degrees_north",
        "vals" = latitude_mat,
        "type" = "float"
      )
      self$data[["lat"]] <- adata
      self$vardimensions[["lat"]] <- c("x", "y")

      adata <- list(
        "long_name" = "longitude",
        "standard_name" = "longitude",
        "units" = "degrees_east",
        "vals" = longitude_mat,
        "type" = "float"
      )
      self$data[["lon"]] <- adata
      self$vardimensions[["lon"]] <- c("x", "y")


      #

      # Path = Pfad in dem die Datei abgespeichert wird. Frei waehlbar, standardmaessig im
      #        gleichen Ordner wie Quelldateien

      #####
      # Definition der Dimensionen
      nc_dim_list <- list()

      for (zz in seq(self$dims)) {
        if (is.null(self$dims[[zz]]$standard_name)) {
          self$dims[[zz]]$standard_name <- names(self$dims)[zz]
        }
        if (is.null(self$dims[[zz]]$long_name)) {
          self$dims[[zz]]$long_name <- names(self$dims)[zz]
        }
        if (is.null(self$dims[[zz]]$units)) {
          self$dims[[zz]]$units <- " "
        }
        ####
        nc_dim_list[[names(self$dims)[zz]]] <- ncdf4::ncdim_def(self$dims[[zz]]$long_name, self$dims[[zz]]$units,
          vals = self$dims[[zz]]$vals,
          longname = self$dims[[zz]]$long_name
        )
      }

      # Definition aller Variablen Ã¼ber eine Schleife in eine Liste.
      # Muss an nc_create sowieso als Liste Ã¼bergeben werden
      ncvariables <- list()
      ncvarvector <- c()

      xvar <- nc_dim_list$x
      yvar <- nc_dim_list$y

      for (t in seq(self$data)) {
        # Quick'n'dirty hotfix. Einlesen bestehender static driver belegt type mit
        # "int" statt "integer"
        if (self$data[[t]]$type == "int") {
          self$data[[t]]$type <- "integer"
        }
        # Falls 3D GrÃ¶Ãe:
        if (length(dim(self$data[[t]]$vals)) == 3) {
          # Vardimensions wird nur beschrieben, wenn wir die static Datei einlesen
          # bisher sollte das funktionieren, spÃ¤ter evtl mal genauer!
          if (is.null(self$vardimensions)) {
            zcoord <- dim(self$data[[t]]$vals)[3]
            find_dim <- which(unlist(lapply(nc_dim_list, function(x) {
              x$len == zcoord
            })))
            if (length(find_dim) >= 2) {
              print("Mehrere \"z-Koordinaten\" sind gleich lang. Erstes Element genutzt")
              print("HÃ¶chst warscheinlich Fehler enthalten")
              find_dim <- find_dim[1]
            }
            zvar <- nc_dim_list[[find_dim]]
          } else {
            # if(!oldversion & names(self$data)[t]=="surface_fraction"){
            #  zvar     <- nc_dim_list[[self$vardimensions[[names(self$data)[t]]][1]]]
            # } else {
            zvar <- nc_dim_list[[self$vardimensions[[names(self$data)[t]]][3]]]
            # }
          }
          # if(oldversion){
          dimlist <- list(xvar, yvar, zvar)
          # } else if(!oldversion & names(self$data)[t]=="surface_fraction"){
          #  dimlist  <- list(zvar,xvar,yvar)
          # } else {
          #  dimlist  <- list(xvar,yvar,zvar)
          # }
        } else if (names(self$data)[t] == "crs") {
          dimlist <- list()
        } else if (names(self$data)[t] == "E_UTM") {
          dimlist <- list(xvar)
        } else if (names(self$data)[t] == "N_UTM") {
          dimlist <- list(yvar)
        } else {
          dimlist <- list(xvar, yvar)
        }

        tmp <- ncdf4::ncvar_def(
          name = names(self$data)[t],
          units = self$data[[t]]$units,
          dim = dimlist,
          missval = self$data[[t]]$"_FillValue",
          ########## Falls probleme auftreten:
          longname = self$data[[t]]$long_name,
          ########## Zeile wieder einkommentieren
          ########## (dann funktioniet der export von importierten ncdf nicht.)
          prec = self$data[[t]]$type
        )

        ncvariables[[names(self$data)[t]]] <- tmp
        ncvarvector <- c(ncvarvector, tmp)
      }
      # Attribute, die normalerweise nicht mehr extra vergeben werden mÃ¼ssen
      # Fuer loop ch in loopnum!
      ex_atts <- c("_FillValue", "units", "long_name", "vals", "type")

      # Erstellen der eigentlichen nc_file!
      ncfile <- ncdf4::nc_create(self$exportname, vars = ncvariables, force_v4 = TRUE)

      # EinfÃ¼gen aller Attribute aus der Headerdatei palm_global als globale Attribute
      for (j in seq(self$header$head)) {
        ncdf4::ncatt_put(ncfile, 0, names(self$header$head)[j], self$header$head[[j]])
      }


      # EinfÃ¼gen der ZusÃ¤tzlichen Attribute (ch in loopnum)
      # Sowie der eigentlichen Daten (ncvar_put(..., vals = self$data$XXX$vals))
      for (t in seq(ncfile$var)) {
        loopnum <- which(!names(self$data[[t]]) %in% ex_atts)
        # simple fix to always get units
        loopnum <- c(loopnum, 2)

        for (ch in loopnum) {
          loopvar <- names(self$data[[t]])[ch]
          typething <- typeof(unlist(self$data[[t]][ch]))
          ncdf4::ncatt_put(
            nc = ncfile, varid = names(self$data)[t], attname = loopvar,
            attval = unlist(self$data[[t]][ch])
          )
        }
        ncdf4::ncvar_put(ncfile,
          varid = ncfile$var[[t]]$name,
          vals = self$data[[t]]$vals
        )
      }
      # SchlieÃen und speichern der Datei
      ncdf4::nc_close(ncfile)
    },
    # Plot des ausgewÃ¤hlten Areas

    #' Calls ggplot to plot available data in a 2D plot. Contains simple color
    #' coding for pavement, water, buildings and a distinction for small vegetation
    #' and trees. Can display the whole domain or small subareas.
    #' Missing Data is displayed as white and will be marked by the legend.
    #'
    #' @param xleft Distance to lower left corner in x direction. 1 is coordinate origin
    #' @param yleft Distance to lower left corner in y direction. 1 is coordinate origin
    #' @param xl Length of plot in x direction. Cannot exceed overall dimensions of domain
    #' @param yl Length of plot in y direction. Cannot exceed overall dimensions of domain
    #'
    #' @return Simple visualization of the simulation domain
    #' @export
    #'
    #' @examples
    #' berlin_example$plot_area(1,1,200,200)
    plot_area = function(xleft = NULL, yleft = NULL,
                         xl = NULL, yl = NULL) {

      # xleft:  untere linke ecke in x-Koordinaten (1 = Quelle links unten)
      # yleft:  untere linke ecke in y-Koordinaten (1 = Quelle links unten)
      # erlaubt theoretisch Zoom in area of interest

      # xl:     Anzahl der Punkte in x-Richtung (max nx_max !)
      # yl:     Anzahl der Punkte in y-Richtung (max ny_max !)
      if (is.null(xleft)) xleft <- 1
      if (is.null(yleft)) yleft <- 1
      if (is.null(xl)) xl <- dim(self$data$zt$vals)[1]
      if (is.null(yl)) yl <- dim(self$data$zt$vals)[2]


      if (self$oldversion) {
        checkvar <- "buildings_2D"
      } else {
        checkvar <- "buildings_2d"
      }
      # CHECK IF NA OR -9999.9 AS FILL VALUE
      if (any(is.na(self$data$pavement_type$vals))) {
        self$plotcntr <- self$plotcntr + 1
        plot_surf <- array(0, dim = c(xl, yl))

        plot_surf[which(!is.na(self$data$pavement_type$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)]), arr.ind = T)] <- 3

        veg_mat_provisorisch <- self$data$vegetation_type$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)]
        veg_mat_provisorisch[is.na(veg_mat_provisorisch)] <- 0

        tree_mat_prov <- array(NA, dim = dim(veg_mat_provisorisch))
        tree_mat_prov[which(veg_mat_provisorisch %in% c(4:7, 17, 18), arr.ind = T)] <- 1

        veg_mat_provisorisch[which(veg_mat_provisorisch %in% c(4:7), arr.ind = T)] <- 0
        veg_mat_provisorisch[which(veg_mat_provisorisch == 0, arr.ind = T)] <- NA

        plot_surf[which(!is.na(veg_mat_provisorisch), arr.ind = TRUE)] <- 2

        if (any(tree_mat_prov == 1, na.rm = T)) {
          plot_surf[which(!is.na(tree_mat_prov), arr.ind = TRUE)] <- 5
        }


        if (any(is.na(self$data[[checkvar]]$vals))) {
          plot_surf[which(!is.na(self$data[[checkvar]]$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)]), arr.ind = T)] <- 1
        } else {
          plot_surf[which(!(self$data[[checkvar]]$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)] <= -9999), arr.ind = T)] <- 1
        }

        plot_surf[which(!is.na(self$data$water_type$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)]), arr.ind = T)] <- 4
      } else {
        self$plotcntr <- self$plotcntr + 1
        plot_surf <- array(0, dim = c(xl, yl))
        plot_surf[which(self$data$pavement_type$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)] > 0, arr.ind = T)] <- 3

        veg_mat_provisorisch <- self$data$vegetation_type$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)]
        veg_mat_provisorisch[which(veg_mat_provisorisch < 0)] <- 0
        tree_mat_prov <- array(0, dim = dim(veg_mat_provisorisch))
        tree_mat_prov[which(veg_mat_provisorisch %in% c(4:7, 17, 18), arr.ind = T)] <- 1
        veg_mat_provisorisch[which(veg_mat_provisorisch %in% c(4:7), arr.ind = T)] <- 0

        plot_surf[which(veg_mat_provisorisch > 0, arr.ind = TRUE)] <- 2

        if (any(tree_mat_prov == 1, na.rm = T)) {
          plot_surf[which(tree_mat_prov > 0, arr.ind = TRUE)] <- 5
        }


        if (any(is.na(self$data[[checkvar]]$vals))) {
          plot_surf[which(!is.na(self$data[[checkvar]]$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)]), arr.ind = T)] <- 1
        } else if (any(self$data[[checkvar]]$vals == 0)) {
          plot_surf[which(!(self$data[[checkvar]]$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)] <= 0), arr.ind = T)] <- 1
        } else {
          plot_surf[which(!(self$data[[checkvar]]$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)] <= -9999), arr.ind = T)] <- 1
        }

        plot_surf[which(self$data$water_type$vals[xleft:(xleft + xl - 1), yleft:(yleft + yl - 1)] > 0, arr.ind = T)] <- 4
      }
      plot_data <- reshape2::melt(plot_surf)

      plot_data$colour <- as.factor(plot_data$value)
      loopvar <- levels(plot_data$colour)
      colorvec <- c()
      for (jj in loopvar) {
        if (jj == "0") {
          levels(plot_data$colour)[levels(plot_data$colour) == jj] <- "nothing"
          colorvec <- c(colorvec, "#FFFFFF")
        }
        if (jj == "1") {
          levels(plot_data$colour)[levels(plot_data$colour) == jj] <- "building"
          colorvec <- c(colorvec, "#000000")
        }
        if (jj == "2") {
          levels(plot_data$colour)[levels(plot_data$colour) == jj] <- "vegetation"
          colorvec <- c(colorvec, "#00F608")
        }
        if (jj == "3") {
          levels(plot_data$colour)[levels(plot_data$colour) == jj] <- "street"
          colorvec <- c(colorvec, "#8C8C8C")
        }
        if (jj == "4") {
          levels(plot_data$colour)[levels(plot_data$colour) == jj] <- "water"
          colorvec <- c(colorvec, "#0000ff")
        }
        if (jj == "5") {
          levels(plot_data$colour)[levels(plot_data$colour) == jj] <- "trees"
          colorvec <- c(colorvec, "#088A08")
        }
      }


      # levels(plot_data$colour) <- c("nothing", "building", "vegetation", "street", "water")

      self$savedplots[[self$plotcntr]] <- ggplot2::ggplot(plot_data, aes(x = Var1, y = Var2, fill = colour)) +
        geom_tile() +
        scale_fill_manual(values = colorvec) +
        theme_bw() + theme(
          axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
          axis.text.y = element_text(size = 9),
          plot.title = element_text(size = 11),
          legend.text = element_text(size = 7)
        ) +
        coord_fixed(ratio = 1) + xlab("X") + ylab("Y")
      self$savedplots[[self$plotcntr]]
    },
    createbuilding3D = function(force = FALSE, orogrphy3d = FALSE) {
      if (self$oldversion) {
        checkvar <- "buildings_3D"
        buildings2d <- "buildings_2D"
      } else {
        checkvar <- "buildings_3d"
        buildings2d <- "buildings_2d"
      }

      if (any(names(self$data) == checkvar) & force == FALSE) {
        print("A buildings_3D array already exists")
      } else {
        build <- self$data[[buildings2d]]$vals
        build[is.na(build)] <- -9999.9
        dx <- self$header$head$resolution
        zmax <- max(build, na.rm = T)
        if (zmax <= 0) {
          z <- 0
          zmax <- 0
        } else {
          z <- tryCatch(
            {
              seq(0, zmax, by = dx)
            },
            warning = function(w) {
              print("Warning in seq line 1258")
            },
            error = function(e) {
              print("Error in seq line 1258")
            }
          )
          z <- z - 0.5 * dx
          z[1] <- 0
        }


        build3d <- array(0, dim = c(dim(build), zmax / dx + 1))
        for (i in seq(dim(build3d)[1])) {
          for (j in seq(dim(build3d)[2])) {
            if (build[i, j] > 0) {
              bheight <- build[i, j] / dx
              build3d[i, j, 1:(bheight + 1)] <- 1
            }
          }
        }

        adata <- list(
          "long_name" = "z",
          "standard_name" = "z",
          "units" = "m",
          "vals" = z
        )
        # "type" = "float")
        self$dims$z <- adata

        self$vardimensions[[checkvar]] <- c(1, 2, which(names(self$dims) == "z"))

        adata <- list(
          "_FillValue" = -127,
          "units" = "",
          "long_name" = "building_flag",
          "res_origin" = dx,
          "source" = "CityGML",
          "lod" = 2,
          "vals" = build3d,
          "type" = "byte"
        )

        self$data[[checkvar]] <- adata
      }
      if (orogrphy3d) {
        if (self$oldversion) {
          checkvar <- "orography_2D"
        } else {
          checkvar <- "zt"
        }


        build <- self$data[[checkvar]]$vals
        dx <- self$header$head$resolution
        zmax <- max(build, na.rm = T)

        z <- tryCatch(
          {
            seq(0, zmax, by = dx)
          },
          warning = function(w) {
            print("Warning in seq line 1309")
          },
          error = function(e) {
            print("Error in seq line 1309")
          }
        )

        if(length(self$dims$z$vals) < length(z) ){
          new_build_array <- array(-127, c(dim(self$data$buildings_2d$vals), length(z)))
          new_build_array[,,seq(self$dims$z$vals)] <- self$data$buildings_3d$vals

          self$data$buildings_3d$vals <-  new_build_array

          self$dims$z$vals <- z
        }

        # adata <- list(
        #   "long_name" = "z_oro",
        #   "standard_name" = "z_oro",
        #   "units" = "m",
        #   "vals" = z
        # )

        self$dims$z_oro <- adata

        self$vardimensions[["orography_3d"]] <- c(1, 2, which(names(self$dims) == "z"))


        build3d <- array(0, dim = c(dim(build), length(self$dims$z$vals)))

        for (i in seq(dim(build3d)[1])) {
          for (j in seq(dim(build3d)[2])) {
            if (build[i, j] > 0) {
              bheight <- build[i, j] / dx
              build3d[i, j, 1:(bheight + 1)] <- 1
            }
          }
        }

        adata <- list(
          "_FillValue" = -127,
          "units" = "m",
          "long_name" = "3D orography data",
          "res_origin" = dx,
          "source" = "CityGML",
          "lod" = 2,
          "vals" = build3d,
          "type" = "byte"
        )

        self$data$orography_3d <- adata
      }
    },
    quickplot = function(variable) {
      if (any(self$data[[variable]]$vals < 0)) {
        plotmatrix <- self$data[[variable]]$vals
        plotmatrix[plotmatrix < 0] <- 0
      } else {
        plotmatrix <- self$data[[variable]]$vals
      }

      plt.data <- reshape2::melt(plotmatrix)
      if (variable == "zt") {
        ggplot2::ggplot(plt.data, aes(x = Var1, y = Var2, fill = value)) +
          geom_raster()
      } else {
        ggplot2::ggplot(plt.data, aes(x = Var1, y = Var2, fill = value)) +
          geom_raster() +
          scale_x_continuous(expand = c(0, 0), limits = c(0, max(plt.data[, 1]))) +
          scale_y_continuous(expand = c(0, 0), limits = c(0, max(plt.data[, 2])))
      }
    },
    correct_surface_fraction = function() {
      self$data$surface_fraction$vals[, , 4] <- NA
      lopv <- dim(self$data$surface_fraction$vals)
      for (fc in seq(lopv[1])) {
        for (u in seq(lopv[2])) {
          if (any(!is.na(self$data$surface_fraction$vals[fc, u, 1:3]))) {
            if (any(self$data$surface_fraction$vals[fc, u, 1:3] >= 0)) {
              self$data$surface_fraction$vals[fc, u, 4] <- 1
            }
          }
        }
      }
    },
    fill_areal = function(variabletofill, startx, starty, fillvalue,
                          valuetobefilled = NULL, boundaryarea = NULL,
                          overwrite = TRUE) {
      areatofill2 <- self$data[[variabletofill]]$vals


      if (is.null(boundaryarea)) {
        boundary.area2 <- areatofill2
        boundaryarea <- variabletofill
      } else {
        boundary.area2 <- self$data[[boundaryarea]]$vals
      }

      if (is.null(valuetobefilled) & is.null(boundaryarea)) {
        value.tobe.filled2 <- self$data[[variabletofill]]$`_FillValue`
      } else if (!is.null(boundaryarea)) {
        value.tobe.filled2 <- self$data[[boundaryarea]]$`_FillValue`
      } else {
        value.tobe.filled2 <- valuetobefilled
      }

      boundary_fill_temp <- self$data[[boundaryarea]]$`_FillValue`

      stackx <- startx
      stacky <- starty

      while (length(stackx) > 0) {
        popx <- stackx[length(stackx)]
        popy <- stacky[length(stacky)]

        stackx <- stackx[-length(stackx)]
        stacky <- stacky[-length(stacky)]


        if (boundary.area2[popx, popy] == value.tobe.filled2) {
          boundary.area2[popx, popy] <- boundary_fill_temp - 1
          if (overwrite) {
            areatofill2[popx, popy] <- fillvalue
          } else if (!overwrite) {
            if (areatofill2[popx, popy] > 0) {

            } else {
              areatofill2[popx, popy] <- fillvalue
            }
          }

          if (popy + 1 <= dim(areatofill2)[2]) {
            stackx <- c(stackx, popx)
            stacky <- c(stacky, popy + 1)
          }
          if (popy - 1 > 0) {
            stackx <- c(stackx, popx)
            stacky <- c(stacky, popy - 1)
          }
          if (popx + 1 <= dim(areatofill2)[1]) {
            stackx <- c(stackx, popx + 1)
            stacky <- c(stacky, popy)
          }
          if (popx - 1 > 0) {
            stackx <- c(stackx, popx - 1)
            stacky <- c(stacky, popy)
          }
        }
      }
      if (all(boundary.area2 != areatofill2)) {
        boundary.area2[boundary.area2 == (boundary_fill_temp - 1)] <- boundary_fill_temp
      }


      self$data[[variabletofill]]$vals <- areatofill2
    },
    generate_lai_array = function(dz, fixed_tree_height = NULL, alpha = 5, beta = 3,
                                  startx = NULL, starty = NULL, lengthx = NULL,
                                  lengthy = NULL, additional_array = NULL) {

      # Erstellung eines 3D-arrays der leaf area density fuer 'Baumgruppen'.
      #
      # lai           - Leaf Area Index (Matrix)
      # canopy_height - Vegetationshoehe (Matrix)
      # dz 			- raeumliche Aufloesung in der Hoehe
      # alpha, beta   - empirische Parameter nach
      #                 Markkanen et al. (2003): Footprints and Fetches for Fluxes
      # 				  over Forest Canopies with varying Structure and Density.
      # 				  Boundary-Layer Meteorology 106: 437-459
      #
      # Rueckgabewert: lad_array[x,y,z]
      if (is.null(fixed_tree_height)) {
        canopy_height1 <- array(NA, c(dim(self$data$vegetation_type$vals)[1], dim(self$data$vegetation_type$vals)[2]))
        canopy_height1[self$data$vegetation_type$vals == 4] <- 20
        canopy_height1[self$data$vegetation_type$vals == 5] <- 20
        canopy_height1[self$data$vegetation_type$vals == 6] <- 20
        canopy_height1[self$data$vegetation_type$vals == 7] <- 20
        canopy_height1[self$data$vegetation_type$vals == 17] <- 20
        canopy_height1[self$data$vegetation_type$vals == 18] <- 11
      } else {
        canopy_height1 <- 0
      }
      # im lazy
      if (!is.null(fixed_tree_height)) {
        tree_height <- fixed_tree_height
      } else {
        tree_height <- 12 # Random number
      }


      if (any(is.null(startx), is.null(starty), is.null(lengthy), is.null(lengthx))) {
        lai <- array(NA, c(dim(self$data$vegetation_type$vals)[1], dim(self$data$vegetation_type$vals)[2]))
        lai[self$data$vegetation_type$vals == 4] <- 5
        lai[self$data$vegetation_type$vals == 5] <- 5
        lai[self$data$vegetation_type$vals == 6] <- 5
        lai[self$data$vegetation_type$vals == 7] <- 6
        lai[self$data$vegetation_type$vals == 17] <- 5
        lai[self$data$vegetation_type$vals == 18] <- 2.5

        canopy_height <- array(NA, c(dim(self$data$vegetation_type$vals)[1], dim(self$data$vegetation_type$vals)[2]))
        canopy_height[self$data$vegetation_type$vals %in% c(4, 5, 6, 7, 17, 18)] <- tree_height
      } else if (!any(names(self$data) == "lad")) {
        lai <- array(NA, c(dim(self$data$vegetation_type$vals)[1], dim(self$data$vegetation_type$vals)[2]))
        lai[self$data$vegetation_type$vals == 4] <- 5
        lai[self$data$vegetation_type$vals == 5] <- 5
        lai[self$data$vegetation_type$vals == 6] <- 5
        lai[self$data$vegetation_type$vals == 7] <- 6
        lai[self$data$vegetation_type$vals == 17] <- 5
        lai[self$data$vegetation_type$vals == 18] <- 2.5

        canopy_height <- array(NA, c(dim(self$data$vegetation_type$vals)[1], dim(self$data$vegetation_type$vals)[2]))
        canopy_height[self$data$vegetation_type$vals %in% c(4, 5, 6, 7, 17, 18)] <- tree_height
      } else {
        lai <- array(NA, c(dim(self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)])[1], dim(self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)])[2]))
        lai[self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)] == 4] <- 5
        lai[self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)] == 5] <- 5
        lai[self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)] == 6] <- 5
        lai[self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)] == 7] <- 6
        lai[self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)] == 17] <- 5
        lai[self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)] == 18] <- 2.5

        canopy_height <- array(NA, c(dim(self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)])[1], dim(self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)])[2]))
        canopy_height[self$data$vegetation_type$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1)] %in% c(4, 5, 6, 7, 17, 18)] <- tree_height
      }
      if (length(dim(canopy_height1)) > 1) {
        canopy_height <- canopy_height1
      }

      # hardcoded so far
      if (!is.null(additional_array)) {
        ncfile <- ncdf4::nc_open(additional_array)

        data_height <- arcgischeck(
          ncdf4::ncvar_get(ncfile, "Band1"),
          self$arcgis
        )
        ncdf4::nc_close(ncfile)
        data_height <- round(data_height / dz) * dz
        data_height[data_height <= 0] <- NA

        canopy_height <- data_height
        lai <- array(NA, c(dim(self$data$vegetation_type$vals)[1], dim(self$data$vegetation_type$vals)[2]))
        lai[canopy_height >= 0] <- 5
      }


      nx <- dim(canopy_height)[1]
      ny <- dim(canopy_height)[2]

      pch_index <- matrix(as.integer(canopy_height / dz), nrow = dim(canopy_height)[1], ncol = dim(canopy_height)[2])
      pch_index[pch_index == 0] <- NA # Wenn canopy_height < dz, dann NA setzen

      #############
      # Fix for no trees in vegetation file
      #############
      if (all(is.na(pch_index))) {
        z <- c(0, dz * 0.5)
        lad_array <- array(-9999.9, c(dim(pch_index)[1:2], 2))

        adata <- list(
          "long_name" = "zlad",
          "standard_name" = "zlad",
          "units" = "m",
          "vals" = z
        )

        self$dims[["zlad"]] <- adata

        adata <- list(
          "_FillValue" = -9999.9,
          "units" = "m2/m3",
          "long_name" = "leaf area density",
          "source" = "Script by Dirk Pavlik after ncl script by Bjoern Maronga",
          "vals" = lad_array,
          "type" = "float"
        )
        self$data[["lad"]] <- adata
        self$vardimensions[["lad"]] <- c("x", "y", "zlad")
      } else {
        z <- tryCatch(
          {
            seq(0, max(pch_index, na.rm = TRUE), by = 1) * dz
          },
          warning = function(w) {
            print("Warning in seq line 1557")
          },
          error = function(e) {
            print("Error in seq line 1557")
          }
        )
        z <- z - (dz / 2)
        z[1] <- 0

        pre_lad <- rep(NA, length(z))

        lad_array <- array(NA, c(dim(canopy_height)[1], dim(canopy_height)[2], length(z)))

        for (i in 1:nx) {
          for (j in 1:ny) {

            # DEBUG ###########
            # i <-2
            # j <- 3
            ###################

            # cat("i =",i,"j =",j, "\n")

            int_bpdf <- 0
            ch <- canopy_height[i, j]
            if (!is.na(pch_index[i, j]) & ch >= dz) { # if(!is.na(ch) & ch >= 0.5*dz) <- dies war die originale Abfrage, funktioniert jedoch nicht bei canopy_height < dz !!!
              for (k in 1:(pch_index[i, j] + 1)) {
                int_bpdf <- int_bpdf + (((z[k] / canopy_height[i, j])^(alpha - 1)) * ((1.0 - (z[k] / canopy_height[i, j]))^(beta - 1)) * (dz / canopy_height[i, j]))
                # cat("int_bpdf =",int_bpdf,"\n")
              }

              for (k in 1:(pch_index[i, j] + 1)) {
                pre_lad[k] <- lai[i, j] * (((dz * (k - 1) / canopy_height[i, j])^(alpha - 1)) * ((1.0 - (dz * (k - 1) / canopy_height[i, j]))^(beta - 1)) / int_bpdf) / canopy_height[i, j]
                # cat("pre_lad[",k,"] =",pre_lad[k],"\n")
              }

              lad_array[i, j, 1] <- pre_lad[1]
              for (k in 2:(pch_index[i, j] + 1)) {
                lad_array[i, j, k] <- 0.5 * (pre_lad[k - 1] + pre_lad[k])
                # cat("lad_array[",i,",",j,", ] =",lad_array[i,j,],"\n")
              }
            }
          }
        }


        lad_array[is.na(lad_array)] <- -9999.9
        if (!is.null(additional_array) && any(names(self$data) == "lad")) {
          if (dim(lad_array)[3] > dim(self$data$lad$vals)[3]) {
            new_array <- array(-9999.9, c(dim(self$data$lad$vals)[1:2], dim(lad_array)[3]))
            new_array[, , 1:dim(self$data$lad$vals)[3]][self$data$lad$vals >= 0] <- self$data$lad$vals[self$data$lad$vals >= 0]
            new_array[lad_array > 0] <- lad_array[lad_array > 0]

            adata <- list(
              "long_name" = "zlad",
              "standard_name" = "zlad",
              "units" = "m",
              "vals" = z
            )

            self$dims[["zlad"]] <- adata
          } else {
            new_array <- array(-9999.9, c(dim(self$data$lad$vals)[1:2], dim(lad_array)[3]))
            new_array[, , 1:dim(self$data$lad$vals)[3]][self$data$lad$vals >= 0] <- self$data$lad$vals[self$data$lad$vals >= 0]
            new_array[lad_array > 0] <- lad_array[lad_array > 0]
          }
          self$data$lad$vals <- new_array
        } else if (any(is.null(startx), is.null(starty), is.null(lengthy), is.null(lengthx))) {
          adata <- list(
            "long_name" = "zlad",
            "standard_name" = "zlad",
            "units" = "m",
            "vals" = z
          )

          self$dims[["zlad"]] <- adata


          adata <- list(
            "_FillValue" = -9999.9,
            "units" = "m2/m3",
            "long_name" = "leaf area density",
            "source" = "Script by Dirk Pavlik after ncl script by Bjoern Maronga",
            "vals" = lad_array,
            "type" = "float"
          )
          self$data[["lad"]] <- adata

          self$vardimensions[["lad"]] <- c("x", "y", "zlad")
        } else if (!any(names(self$data) == "lad")) {
          adata <- list(
            "long_name" = "zlad",
            "standard_name" = "zlad",
            "units" = "m",
            "vals" = z
          )

          self$dims[["zlad"]] <- adata


          adata <- list(
            "_FillValue" = -9999.9,
            "units" = "",
            "long_name" = "lad",
            "source" = "Script by Dirk Pavlik after ncl script by Bjoern Maronga",
            "vals" = lad_array,
            "type" = "float"
          )
          self$data[["lad"]] <- adata

          self$vardimensions[["lad"]] <- c("x", "y", "zlad")
        } else {
          if (dim(lad_array)[3] > dim(self$data$lad$vals)[3]) {
            new_array <- array(-9999.9, c(dim(self$data$lad$vals)[1:2], dim(lad_array)[3]))
            new_array[, , 1:dim(self$data$lad$vals)[3]][self$data$lad$vals > 0] <- self$data$lad$vals[self$data$lad$vals > 0]
            new_array[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1), ][lad_array > 0] <- lad_array[lad_array > 0]

            self$data$lad$vals <- new_array

            adata <- list(
              "long_name" = "zlad",
              "standard_name" = "zlad",
              "units" = "m",
              "vals" = z
            )

            self$dims[["zlad"]] <- adata
          } else {
            self$data$lad$vals[startx:(startx + lengthx - 1), starty:(starty + lengthy - 1), ][lad_array > 0] <- lad_array[lad_array > 0]
          }
        }
      }
    },
    import_data = function(v.file, palmtype, typeid, street = FALSE) {
      # if(palmtype!=listofvariablesforpalm
      if (self$oldversion) {
        checkvar <- "buildings_2D"
      } else {
        checkvar <- "buildings_2d"
      }

      if (palmtype == checkvar) {
        dtype <- "float"
        fillvalue <- -9999.9
      } else {
        dtype <- "byte"
        fillvalue <- -127
      }




      ncfile <- ncdf4::nc_open(v.file)
      dimen <- list()

      for (zz in seq(ncfile$ndims)) {
        vec <- ncdf4::ncvar_get(ncfile, names(ncfile$dim)[zz])
        attr <- ncdf4::ncatt_get(ncfile, names(ncfile$dim)[zz])
        adata <- list()
        for (ii in seq(attr)) {
          adata[[names(attr)[ii]]] <- attr[[ii]]
        }
        adata[["vals"]] <- vec

        dimen[[names(ncfile$dim)[zz]]] <- adata
      }
      if (any(names(dimen) == "lon")) {
        names(dimen)[names(dimen) == "lon"] <- "x"
        names(dimen)[names(dimen) == "lat"] <- "y"
      }



      if (!all(names(dimen) %in% names(self$dims))) {
        newdimensions <- which(!(names(dimen) %in% names(self$dims)))
        self$dims[[names(dimen)[newdimensions]]] <- dimen[[newdimensions]]
      }
      dat <- list()
      whichdimensions <- list()


      vec <- arcgischeck(
        ncdf4::ncvar_get(ncfile, "Band1"),
        self$arcgis
      )
      vec[is.na(vec)] <- fillvalue
      attr <- ncdf4::ncatt_get(ncfile, "Band1")
      if (max(vec, na.rm = T) == 1) {
        vec[vec > 0] <- typeid
      }
      # } else if(any(vec<0)) {
      #   vec[vec<0]   <- typeid
      # }
      if (is.null(attr$units)) {
        attr$units <- ""
      }


      if (any(names(self$data) == palmtype)) {
        vec2 <- self$data[[palmtype]]$vals
        vec2[vec != 0] <- vec[vec != 0]
        vec <- vec2
      }
      if (street == TRUE & any(names(self$data) == checkvar)) {
        ###########
        # THRESHOLD FOR BUILDINGS
        th <- 4 # 4x5 = 20m Abstand von Straßenachse zu gebaeude
        newvec <- array(0, dim = dim(vec))
        # loop through all dimensions
        for (i in seq(dim(vec)[1])) {
          for (j in seq(dim(vec)[2])) {
            # if pixel is not zero
            if (vec[i, j] != 0) {
              newvec[i, j] <- typeid
            }
            if (vec[i, j] != 0 & (i > th & j > th) & (i < (dim(vec)[1] - th) & (j < dim(vec)[2] - th))) {
              # and pixel in buildings_2D array within threshhold are not zero
              if (any(self$data[[checkvar]]$vals[(i - th):i, (j - th):j]) != 0) {
                newvec[(i - th):i, (j - th):j] <- typeid
              }
              if (any(self$data[[checkvar]]$vals[(i - th):i, j:(j + th)]) != 0) {
                newvec[(i - th):i, j:(j + th)] <- typeid
              }
              if (any(self$data[[checkvar]]$vals[i:(i + th), (j - th):j]) != 0) {
                newvec[i:(i + th), (j - th):j] <- typeid
              }
              if (any(self$data[[checkvar]]$vals[i:(i + th), j:(j + th)]) != 0) {
                newvec[i:(i + th), j:(j + th)] <- typeid
              }
            }
          }
        }
        newvec[self$data[[checkvar]]$vals != 0] <- 0
        vec <- newvec
      }

      vec[vec == 0] <- fillvalue

      adata <- list(
        "_FillValue" = fillvalue,
        "units" = attr$units,
        "long_name" = gsub("_", " ", palmtype),
        "source" = "Rastered QGIS DATA",
        "vals" = vec,
        "type" = dtype,
        "res_orig" = attr$res_orig,
        "lod" = 1
      )
      # dat[[palmtype]]     <- adata
      whichdimensions[[palmtype]] <- ncfile$var$Band1$dimids + 1

      whichdims <- which(names(self$dims) %in% names(dimen))

      self$data[[palmtype]] <- adata
      self$vardimensions[[palmtype]] <- whichdims
    },
    SortOverlayingdata = function(inorderof = "BPWV") {
      if (self$oldversion) {
        checkvar <- "buildings_2D"
      } else {
        checkvar <- "buildings_2d"
      }


      multidimarray <- array(0, c(dim(self$data$pavement_type$vals), 4))
      multidimarray[, , 1] <- self$data[[checkvar]]$vals
      multidimarray[, , 2] <- self$data$pavement_type$vals
      multidimarray[, , 3] <- self$data$water_type$vals
      multidimarray[, , 4] <- self$data$vegetation_type$vals

      # multidimarray[,,1][is.na(multidimarray[,,1])] <- -9999.9
      # multidimarray[,,2][is.na(multidimarray[,,2])] <- -127
      # multidimarray[,,3][is.na(multidimarray[,,3])] <- -127
      # multidimarray[,,4][is.na(multidimarray[,,4])] <- -127

      arrayorder <- c("B", "P", "W", "V")
      throwout <- c(1, 2, 3, 4)
      outerfillv <- c(0, -127, -127, -127)

      loopvar <- unlist(strsplit(inorderof, split = ""))

      # check if first entry of table is always "fill_value!"
      for (i in seq(loopvar)) {
        whicharray <- which(loopvar[i] == arrayorder)
        fillv <- as.numeric(names(table(multidimarray[, , whicharray])[1]))
        wherestuff <- which(multidimarray[, , whicharray] != fillv, arr.ind = T)
        throwout <- throwout[-which(throwout == whicharray)]
        for (j in throwout) {
          bridge_1 <- multidimarray[, , j]
          bridge_1[wherestuff] <- outerfillv[j]
          multidimarray[, , j] <- bridge_1
        }
      }
      self$data[[checkvar]]$vals <- multidimarray[, , 1]
      # self$data$building_id$vals[self$data$building_2d<0] <- -127
      self$data$pavement_type$vals <- multidimarray[, , 2]
      self$data$water_type$vals <- multidimarray[, , 3]
      self$data$vegetation_type$vals <- multidimarray[, , 4]

      if (unlist(strsplit(inorderof, ""))[1] != "B") {
        dx <- self$header$head$resolution

        self$data$building_id$vals[self$data[[checkvar]]$vals <= dx / 2] <- -9999.9
        self$data$building_type$vals[self$data[[checkvar]]$vals <= dx / 2] <- -127
        self$data[[checkvar]]$vals[self$data[[checkvar]]$vals <= dx / 2] <- -9999.9
      }
    },
    countemptyfields = function() {
      if (self$oldversion) {
        checkvar <- "buildings_2D"
      } else {
        checkvar <- "buildings_2d"
      }
      NAarray <- array(-20, dim(self$data[[checkvar]]$vals))
      NAarray[self$data[[checkvar]]$vals > 0] <- self$data[[checkvar]]$vals[self$data[[checkvar]]$vals > 0]
      NAarray[self$data$pavement_type$vals > 0] <- self$data$pavement_type$vals[self$data$pavement_type$vals > 0]
      NAarray[self$data$water_type$vals > 0] <- self$data$water_type$vals[self$data$water_type$vals > 0]
      NAarray[self$data$vegetation_type$vals > 0] <- self$data$vegetation_type$vals[self$data$vegetation_type$vals > 0]
      print(table(NAarray)[1])
    },
    addsoilandsurfacefraction = function(type_soil = 1) {
      soiltype <- self$data$vegetation_type$vals
      soiltype[which(self$data$pavement_type$vals > 0, arr.ind = T)] <- 1
      soiltype[soiltype > 0] <- type_soil
      adata <- list(
        "_FillValue" = -127,
        "units" = "",
        "long_name" = "soil type classification",
        # "long_name" = "soil type",
        "source" = "First Guess",
        "vals" = soiltype,
        "type" = "byte"
      )
      self$data$soil_type <- adata
      self$vardimensions$soil_type <- c(1, 2)


      xvec <- c(0, 1, 2)

      adata <- list("vals" = xvec)
      # "type" = "float")
      self$dims$nsurface_fraction <- adata


      surfacefraction <- array(NA, c(dim(self$data$vegetation_type$vals), 4))
      temp_array <- array(-9999.9, dim(self$data$vegetation_type$vals))
      temp_array[which(self$data$vegetation_type$vals > 0, arr.ind = T)] <- 1
      surfacefraction[, , 1] <- temp_array
      # surfacefraction[,,4]  <- temp_array

      temp_array <- array(-9999.9, dim(self$data$vegetation_type$vals))
      temp_array[which(self$data$pavement_type$vals > 0, arr.ind = T)] <- 1
      # temp_array2       <- temp_array
      # temp_array2[surfacefraction[,,4]==1] <- 1
      # surfacefraction[,,4]  <- temp_array2
      surfacefraction[, , 2] <- temp_array

      temp_array <- array(-9999.9, dim(self$data$vegetation_type$vals))
      temp_array[which(self$data$water_type$vals > 0, arr.ind = T)] <- 1
      # temp_array2       <- temp_array
      # temp_array2[surfacefraction[,,4]==1] <- 1
      # surfacefraction[,,4]  <- temp_array2
      surfacefraction[, , 3] <- temp_array

      surfacefraction[, , 2][which(surfacefraction[, , 1] == 1)] <- 0
      surfacefraction[, , 3][which(surfacefraction[, , 1] == 1)] <- 0
      surfacefraction[, , 1][which(surfacefraction[, , 2] == 1)] <- 0
      surfacefraction[, , 3][which(surfacefraction[, , 2] == 1)] <- 0
      surfacefraction[, , 2][which(surfacefraction[, , 3] == 1)] <- 0
      surfacefraction[, , 1][which(surfacefraction[, , 3] == 1)] <- 0
      surfacefraction[, , 4] <- surfacefraction[, , 1] + surfacefraction[, , 2] + surfacefraction[, , 3]
      surfacefraction[, , 4][which(surfacefraction[, , 4] < 0)] <- -9999.9


      adata <- list(
        "_FillValue" = -9999.9,
        "units" = "",
        # "long_name" = "surface fraction",
        "long_name" = "surface tile fraction",
        "vals" = surfacefraction[, , 1:3],
        "type" = "float"
      )


      self$data$surface_fraction <- adata
      self$vardimensions$surface_fraction <- c(1, 2, which(names(self$dims) == "nsurface_fraction"))
    },
    add_lod2_variable = function(name, v.data = NULL) {
      lod <- NULL
      if (grepl("water", name)) {
        varname <- "water_pars"
        dimname <- "nwater_pars"
        vardim_dim <- seq(0, 6)
        longname <- "water parameters"
        dattype <- "float"
      } else if (grepl("vegetation", name)) {
        varname <- "vegetation_pars"
        dimname <- "nvegetation_pars"
        vardim_dim <- seq(0, 11)
        longname <- "vegetation parameters"
        dattype <- "float"
      } else if (grepl("pavement", name)) {
        varname <- "pavement_pars"
        dimname <- "npavement_pars"
        vardim_dim <- seq(0, 3)
        longname <- "pavement parameters"
        dattype <- "float"
      } else if (grepl("soil", name)) {
        varname <- "soil_pars"
        dimname <- "nsoil_pars"
        vardim_dim <- seq(0, 7)
        longname <- "soil parameters"
        dattype <- "float"
        lod <- 2
      } else if (grepl("building", name)) {
        varname <- "building_pars"
        dimname <- "nbuilding_pars"
        vardim_dim <- seq(0, 46)
        longname <- "building parameters"
        dattype <- "float"
      } else if( grepl("albedo", name)) {
        varname <- "albedo_pars"
        dimname <- "nalbedo_pars"

        vardim_dim <- seq(0,6)
        longname <- "albedo parameters"
        dattype <- "float"

      }
      dimdata <- list(
        "long_name" = dimname,
        "standard_name" = dimname,
        "units" = "1",
        "vals" = vardim_dim
      )
      self$dims[[dimname]] <- dimdata
      self$vardimensions[[varname]] <- c("x", "y", dimname)

      if (is.null(v.data)) {
        valvec <- array(-9999.9, c(
          dim(self$data$zt$vals)[1],
          dim(self$data$zt$vals)[2],
          length(vardim_dim)
        ))
      } else if (dim(v.data)[1] != dim(self$data$zt$vals)[1] |
        dim(v.data)[2] != dim(self$data$zt$vals)[2] |
        dim(v.data)[3] != length(vardim_dim)) {
        valvec <- array(-9999.9, c(
          dim(self$data$zt$vals)[1],
          dim(self$data$zt$vals)[2],
          length(vardim_dim)
        ))
      } else {
        valvec <- v.data
      }
      if (is.null(lod)) {
        adata <- list(
          "_FillValue" = -9999.9,
          "units" = "1",
          "long_name" = longname,
          # "long_name" = "building id",
          "source" = "rscript",
          "vals" = valvec,
          "type" = dattype
        )
      } else {
        adata <- list(
          "_FillValue" = -9999.9,
          "units" = "1",
          "long_name" = longname,
          # "long_name" = "building id",
          "source" = "rscript",
          "lod" = lod,
          "vals" = valvec,
          "type" = dattype
        )
      }
      self$data[[varname]] <- adata
    },
    print = function(...) {
      catch <- character()
      tryCatch(catch <- length(self$dims$x$vals),
        error = function(e) {
          cat("PALM Class \n")
          cat("No data has been input \n")
          invisible(self)
        }
      )
      if (is.numeric(catch)) {
        cat("PALM Class \n")
        cat("Gridpoints in x:", length(self$dims$x$vals), "\n", sep = "")
        cat("Gridpoints in y:", length(self$dims$y$vals), "\n", sep = "")
        cat("Resolution:", self$header$head$resolution, "\n", sep = "")
        cat("Available data: \n", paste(names(self$data), collapse = "\n"), "\n", sep = "")
        invisible(self)
      }
    },
    downscale_resolution = function(factor) {
      dimx <- length(self$dims$x$vals)
      dimy <- length(self$dims$y$vals)

      test <- list()
      newvec <- c()
      for (i in names(self$data)) {
        test[[i]] <- length(dim(self$data[[i]]$vals))
        if (test[[i]] == 2) {
          newvec <- c(newvec, i)
        }
      }

      nelist <- list()
      for (j in newvec) {
        resized <- imager::resize(
          as.cimg(self$data[[j]]$vals),
          dimx * 1 / (factor), dimy * 1 / (factor)
        )[, , 1, 1]
        self$data[[j]]$vals <- resized
      }

      old.dx <- self$header$head$resolution
      new.dx <- old.dx * factor
      self$header$head$resolution <- new.dx

      ## Z KOORDINATE
      self$data$buildings_2d$vals <- floor(self$data$buildings_2d$vals / factor) * factor

      newz.a <- (old.dx / 2) * factor
      newz.b <- max(self$data$buildings_2d$vals) - (old.dx / 2) * factor
      newz <- c(0, seq(newz.a, newz.b, by = new.dx))
      self$dims$z$vals <- newz

      newx <- seq(new.dx / 2,
        dim(self$data$buildings_2d$vals)[1] * new.dx - new.dx / 2,
        by = new.dx
      )

      self$dims$x$vals <- newx


      newy <- seq(new.dx / 2,
        dim(self$data$buildings_2d$vals)[2] * new.dx - new.dx / 2,
        by = new.dx
      )

      self$dims$y$vals <- newy
    },
    cutout_static = function(startp, endp, sure = FALSE) {
      if (!sure) {
        cat("This may loose some data!\n")
        cat("You should be sure, you want to do this!\n")
        cat("Maybe save your current static driver via $clone(deep=TRUE)!\n")
      } else {
        self$dims$x$vals <- self$dims$x$vals[startp[1]:endp[1]]
        self$dims$y$vals <- self$dims$y$vals[startp[2]:endp[2]]

        for (i in names(self$data)) {
          if (length(dim(self$data[[i]]$vals)) == 2) {
            self$data[[i]]$vals <- self$data[[i]]$vals[
              startp[1]:endp[1],
              startp[2]:endp[2]
            ]
          } else if (length(dim(self$data[[i]]$vals)) == 3) {
            self$data[[i]]$vals <- self$data[[i]]$vals[
              startp[1]:endp[1],
              startp[2]:endp[2],
            ]
          }
        }
      }
    },
    add_any2D_variable = function(variablename, variable_list,
                                  print_template = FALSE) {
      if (print_template) {
        cat("Template for variable_list:\n")
        cat("list(\"_FillValue\" = either -127 oder -9999.9,\n")
        cat("\t \"units\" = \"units of data\",\n")
        cat("\t \"long_name\" = \"a long name\",\n")
        cat("\t \"source\" = \"source of data\",\n")
        cat("\t \"lod\" = \"1 or 2 depending on data\",\n")
        cat("\t \"vals\" = data,\n")
        cat("\t \"type\" = \"either byte or float (or int)\")\n")
        cat("\n")
        # cat("Template for dimension_list:\n")
        # cat("list(\"long_name\" = \"a long name\",\n")
        # cat("\t \"standard_name\" = \"standard name\",\n")
        # cat("\t \"units\" = \"units of data\",\n")
        # cat("\t \"vals\" = data)\n")
      } else {
        if (!all(names(variable_list) %in% c(
          "_FillValue", "units", "long_name", "source", "lod",
          "vals", "type"
        ))) {
          cat("Not all necessary data in variable_list!\n")
          stop()
        }
        self$data[[variablename]] <- variable_list
        self$vardimensions[[variablename]] <- c("x", "y")
      }
    },
    add_R6_data = function(data_class) {
      if (!any(class(data_class) == "palm_ncdf_data_template")) {
        stop("Please provide the correct class")
      }

      for (i in names(data_class$data_list)) {
        self$data[[i]] <- data_class$data_list[[i]]
        self$vardimensions[[i]] <- data_class$data_dims[[i]]
      }
    },
    add_R6_dim = function(dim_class) {
      if (!any(class(data_class) == "palm_ncdf_dimension_template")) {
        stop("Please provide the correct class")
      }

      for (i in names(dim_class$datalist)) {
        self$dim[[i]] <- dim_class$dim_list[[i]]
      }
    },
    add_soil = function(type_soil = 1) {
      soiltype <- self$data$vegetation_type$vals
      soiltype[which(self$data$pavement_type$vals > 0, arr.ind = T)] <- 1
      soiltype[soiltype > 0] <- type_soil
      adata <- list(
        "_FillValue" = -127,
        "units" = "",
        "long_name" = "soil type classification",
        "source" = "First Guess",
        "vals" = soiltype,
        "type" = "byte"
      )
      self$data$soil_type <- adata
      self$vardimensions$soil_type <- c(1, 2)
    },
    add_simple_surfacefraction = function() {
      xvec <- c(0, 1, 2)
      adata <- list("vals" = xvec)
      self$dims$nsurface_fraction <- adata


      surfacefraction <- array(NA, c(dim(self$data$vegetation_type$vals), 3))
      temp_array <- array(-9999.9, dim(self$data$vegetation_type$vals))
      temp_array[which(self$data$vegetation_type$vals > 0, arr.ind = T)] <- 1
      surfacefraction[, , 1] <- temp_array
      # surfacefraction[,,4]  <- temp_array

      temp_array <- array(-9999.9, dim(self$data$vegetation_type$vals))
      temp_array[which(self$data$pavement_type$vals > 0, arr.ind = T)] <- 1
      # temp_array2       <- temp_array
      # temp_array2[surfacefraction[,,4]==1] <- 1
      # surfacefraction[,,4]  <- temp_array2
      surfacefraction[, , 2] <- temp_array

      temp_array <- array(-9999.9, dim(self$data$vegetation_type$vals))
      temp_array[which(self$data$water_type$vals > 0, arr.ind = T)] <- 1
      # temp_array2       <- temp_array
      # temp_array2[surfacefraction[,,4]==1] <- 1
      # surfacefraction[,,4]  <- temp_array2
      surfacefraction[, , 3] <- temp_array

      surfacefraction[, , 2][which(surfacefraction[, , 1] == 1)] <- 0
      surfacefraction[, , 3][which(surfacefraction[, , 1] == 1)] <- 0
      surfacefraction[, , 1][which(surfacefraction[, , 2] == 1)] <- 0
      surfacefraction[, , 3][which(surfacefraction[, , 2] == 1)] <- 0
      surfacefraction[, , 2][which(surfacefraction[, , 3] == 1)] <- 0
      surfacefraction[, , 1][which(surfacefraction[, , 3] == 1)] <- 0
      # surfacefraction[,,4]  <-  surfacefraction[,,1] + surfacefraction[,,2]+ surfacefraction[,,3]
      # surfacefraction[,,4][which(surfacefraction[,,4]<0)] <- -9999.9


      adata <- list(
        "_FillValue" = -9999.9,
        "units" = "",
        "long_name" = "surface tile fraction",
        "vals" = surfacefraction[, , 1:3],
        "type" = "float"
      )


      self$data$surface_fraction <- adata
      self$vardimensions$surface_fraction <- c(1, 2, which(names(self$dims) == "nsurface_fraction"))
    },
    generate_lad_single_trees = function(ext_tree_type = NULL,
                                         ext_tree_height = NULL,
                                         ext_crown_diameter = NULL,
                                         ext_tree_shape = NULL,
                                         ext_lai = NULL,
                                         overwrite = TRUE,
                                         ...) {
      #
      # Future idea:
      # Let User Decide, whether to use external data
      # for diameter, height, etc
      # Or Use internal data, that the user specifies!
      #
      dx <- self$header$head$resolution
      # Start with Checks
      #
      # Abort if no information about trees are present
      if (!any(names(self$data) == "tree_id") && !any(names(self$data) == "tree_height")) {
        stop("Neither tree id or tree height are in the dataset!\n Please provide at least one of them!")
      }

      if (any(names(self$data) == "tree_height")) {
        canopy_height <- self$data$tree_height$vals
        canopy_height[is.na(canopy_height)] <- self$data$tree_height$`_FillValue`

        tree_array <- canopy_height
        tree_array[tree_array > 0] <- 1
      } else if (any(names(self$data) == "tree_id")) {
        tree_array <- self$data$tree_id$vals
        tree_array[canopy_heigt > 0] <- 1

        if (is.null(ext_tree_height)) {
          stop("No tree_height given.")
        } else {
          canopy_height <- tree_array * ext_tree_height
        }
      }

      if (is.null(ext_tree_type)) {
        tree_type <- 1 * tree_array
      } else {
        tree_type <- ext_tree_type * tree_array
      }

      if (is.null(ext_crown_diameter)) {
        crown_dia <- 10 * tree_array
      } else {
        crown_dia <- ext_crown_diameter * tree_array
      }

      if (is.null(ext_tree_shape)) {
        tree_shape <- 1
      } else {
        tree_shape <- ext_tree_shape
      }

      if (is.null(ext_lai)) {
        if (!any(names(self$data) == "vegetation_type")) {
          stop("No LAI information available!")
        }
        lai <- array(NA, dim(self$data$vegetation_type$vals))
        lai[self$data$vegetation_type$vals == 4] <- 5
        lai[self$data$vegetation_type$vals == 5] <- 5
        lai[self$data$vegetation_type$vals == 6] <- 5
        lai[self$data$vegetation_type$vals == 7] <- 6
        lai[self$data$vegetation_type$vals == 17] <- 5
        lai[self$data$vegetation_type$vals == 18] <- 2.5
      } else {
        lai <- ext_lai * tree_array
      }

      lad_temp <- array(0, c(dim(lai), 1 + max(canopy_height, na.rm=T) / dx))
      bad_temp <- array(0, c(dim(lai), 1 + max(canopy_height, na.rm=T) / dx))

      if (overwrite) {
        for (i in seq(dim(lai)[1])) {
          for (j in seq(dim(lai)[2])) {
            if (tree_type[i, j] <= 0) {

            } else {
              tmp_dat <- f.calc_single_tree(
                tree_type_b = tree_type[i, j],
                tree_shape = tree_shape,
                crown_ratio = tree_shape_parameters$ratio[tree_type[i, j]],
                crown_diameter = crown_dia[i, j],
                tree_height = canopy_height[i, j],
                lai = lai[i, j],
                dbh = tree_trunk_parameters$dbh[tree_type[i, j]],
                dx = dx
              )

              dat_range_x <- (i - as.integer(dim(tmp_dat$lad)[1] / 2)):(i + as.integer(dim(tmp_dat$lad)[1] / 2))
              dat_range_y <- (j - as.integer(dim(tmp_dat$lad)[2] / 2)):(j + as.integer(dim(tmp_dat$lad)[2] / 2))

              dat_range_x <- dat_range_x[dat_range_x > 0]
              dat_range_y <- dat_range_y[dat_range_y > 0]

              dat_range_x <- dat_range_x[dat_range_x <= dim(lai)[1]]
              dat_range_y <- dat_range_y[dat_range_y <= dim(lai)[2]]

              lad_temp[dat_range_x, dat_range_y, seq(dim(tmp_dat$lad)[3])] <- tmp_dat$lad[dat_range_x - i + ceiling(dim(tmp_dat$lad)[1] / 2), dat_range_y - j + ceiling(dim(tmp_dat$lad)[2] / 2), ]
              bad_temp[dat_range_x, dat_range_y, seq(dim(tmp_dat$bad)[3])] <- tmp_dat$bad[dat_range_x - i + ceiling(dim(tmp_dat$lad)[1] / 2), dat_range_y - j + ceiling(dim(tmp_dat$lad)[2] / 2), ]
            }
          }
        }
      } else {
        for (i in seq(dim(lai)[1])) {
          for (j in seq(dim(lai)[2])) {
            if (tree_type[i, j] <= 0) {

            } else {
              tmp_dat <- f.calc_single_tree(
                tree_type_b = tree_type[i, j],
                tree_shape = tree_shape,
                crown_ratio = tree_shape_parameters$ratio[tree_type[i, j]],
                crown_diameter = crown_dia[i, j],
                tree_height = canopy_height[i, j],
                lai = lai[i, j],
                dbh = tree_trunk_parameters$dbh[tree_type[i, j]],
                dx = dx,
                fillvalue = 0
              )

              dat_range_x <- (i - as.integer(dim(tmp_dat$lad)[1] / 2)):(i + as.integer(dim(tmp_dat$lad)[1] / 2))
              dat_range_y <- (j - as.integer(dim(tmp_dat$lad)[2] / 2)):(j + as.integer(dim(tmp_dat$lad)[2] / 2))

              dat_range_x <- dat_range_x[dat_range_x > 0]
              dat_range_y <- dat_range_y[dat_range_y > 0]

              dat_range_x <- dat_range_x[dat_range_x <= dim(lai)[1]]
              dat_range_y <- dat_range_y[dat_range_y <= dim(lai)[2]]

              lad_temp[dat_range_x, dat_range_y, seq(dim(tmp_dat$lad)[3])] <- lad_temp[dat_range_x, dat_range_y, seq(dim(tmp_dat$lad)[3])] + tmp_dat$lad[dat_range_x - i + ceiling(dim(tmp_dat$lad)[1] / 2), dat_range_y - j + ceiling(dim(tmp_dat$lad)[2] / 2), ]
              bad_temp[dat_range_x, dat_range_y, seq(dim(tmp_dat$bad)[3])] <- bad_temp[dat_range_x, dat_range_y, seq(dim(tmp_dat$bad)[3])] + tmp_dat$bad[dat_range_x - i + ceiling(dim(tmp_dat$lad)[1] / 2), dat_range_y - j + ceiling(dim(tmp_dat$lad)[2] / 2), ]
            }
          }
        }
        lad_temp[, , 2:dim(lad_temp)[3]][lad_temp[, , 2:dim(lad_temp)[3]] == 0] <- -9999.9
        bad_temp[, , 2:dim(bad_temp)[3]][bad_temp[, , 2:dim(bad_temp)[3]] == 0] <- -9999.9
      }

      if (!any(names(self$data) == "lad")) {
        z <- seq(0, dim(lad_temp)[3], by = 1) * dx
        z <- z - (dx / 2)
        z[1] <- 0

        adata <- list(
          "long_name" = "zlad",
          "standard_name" = "zlad",
          "units" = "m",
          "vals" = z
        )

        self$dims[["zlad"]] <- adata

        adata <- list(
          "_FillValue" = -9999.9,
          "units" = "m2/m3",
          "long_name" = "leaf area density",
          "source" = "According to ncl script by Bjoern Maronga",
          "vals" = lad_temp,
          "type" = "float"
        )
        self$data[["lad"]] <- adata
        self$vardimensions[["lad"]] <- c("x", "y", "zlad")

        adata <- list(
          "_FillValue" = -9999.9,
          "units" = "m2/m3",
          "long_name" = "leaf area density",
          "source" = "According to ncl script by Bjoern Maronga",
          "vals" = bad_temp,
          "type" = "float"
        )
        self$data[["bad"]] <- adata
        self$vardimensions[["bad"]] <- c("x", "y", "zlad")
      } else {
        ifelse(dim(self$data$lad$vals)[3] >= dim(lad_temp)[3],
               new_dat <- array(-9999.9, dim(self$data$lad$vals)),
               new_dat <- array(-9999.9, dim(lad_temp))
        )
        new_dat[, , 1:dim(self$data$lad$vals)[3]] <- self$data$lad$vals
        new_dat[, , 1:dim(lad_temp)[3]][lad_temp > 0] <- lad_temp[lad_temp > 0]

        if(dim(self$data$lad$vals)[3] < dim(lad_temp)[3]){
          z <- seq(0, dim(new_dat)[3]-1, by = 1) * dx
          z <- z - (dx / 2)
          z[1] <- 0
        }



        self$dims$zlad$vals <- z
        self$data$lad$vals <- new_dat

        adata <- list(
          "_FillValue" = -9999.9,
          "units" = "m2/m3",
          "long_name" = "leaf area density",
          "source" = "According to ncl script by Bjoern Maronga",
          "vals" = bad_temp,
          "type" = "float"
        )
        self$data[["bad"]] <- adata
        self$vardimensions[["bad"]] <- c("x", "y", "zlad")
      }

    }, generate_lad_patch = function() {
      cat("Not yet implemented.")
    }
  ),

  private = list()
)
