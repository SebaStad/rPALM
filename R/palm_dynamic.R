palm_dynamic <- R6Class("palm_dynamic", public = list(
  data = NULL,
  dims = NULL,
  vardimensions = list(),
  palmgrid = NULL,
  #' Title
  #'
  #' @param dx
  #' @param nx
  #' @param ny
  #' @param nz
  #'
  #' @return
  #' @export
  #'
  #' @examples
  initialize = function(dx, nx, ny, nz) {
    dimen <- list()

    xvec <- seq(dx * 0.5, nx * dx, by = dx)
    yvec <- seq(dx * 0.5, ny * dx, by = dx)
    zvec <- seq(dx * 0.5, nz * dx, by = dx)

    xuvec <- seq(dx, (nx - 1) * dx, by = dx)
    yvvec <- seq(dx, (ny - 1) * dx, by = dx)
    zwvec <- seq(dx, (nz - 1) * dx, by = dx)

    ######
    # Coordinates
    # X
    adata <- list(
      "long_name" = "x",
      "standard_name" = "x coordinate of cell centers",
      "units" = "m",
      "axis" = "X",
      "vals" = xvec
    )

    dimen$x <- adata

    # Y
    adata <- list(
      "long_name" = "y",
      "standard_name" = "y coordinate of cell centers",
      "units" = "m",
      "axis" = "Y",
      "vals" = yvec
    )

    dimen$y <- adata
    # Z
    adata <- list(
      "long_name" = "z",
      "standard_name" = "z coordinate of cell centers",
      "units" = "m",
      "axis" = "Z",
      "positive" = "up",
      "vals" = zvec
    )

    dimen$z <- adata

    adata <- list(
      "long_name" = "zw",
      "standard_name" = "z coordinate of cell faces",
      "name" = "zw",
      "units" = "m",
      "axis" = "Z",
      "positive" = "up",
      "vals" = zwvec
    )
    dimen$zw <- adata

    adata <- list(
      "long_name" = "xu",
      "standard_name" = "x coordinate of cell faces",
      "units" = "m",
      "axis" = "X",
      "vals" = xuvec
    )

    dimen$xu <- adata

    # Y
    adata <- list(
      "long_name" = "yv",
      "standard_name" = "y coordinate of cell faces",
      "units" = "m",
      "axis" = "Y",
      "vals" = yvvec
    )

    dimen$yv <- adata




    self$dims <- dimen


    palm_grid <- list()
    palm_grid$u <- c("xu", "y", "z", "time")
    palm_grid$v <- c("x", "yv", "z", "time")
    palm_grid$w <- c("x", "y", "zw", "time")
    palm_grid$pt <- c("x", "y", "z", "time")
    palm_grid$qv <- c("x", "y", "z", "time")
    palm_grid$init_soil_lod2 <- c("x", "y", "zsoil")
    palm_grid$init_soil_lod1 <- "zsoil"

    self$palmgrid <- palm_grid
  },
  set_initial_profile_lod1 = function(variable, data.vector) {
    dat <- list()
    whichdimensions <- list()
    if (variable == "pt") {
      unit <- "K"
      loopvar <- c("pt")
    } else if (variable == "u" | variable == "v" | variable == "w") {
      unit <- "m/s"
      loopvar <- variable
    } else if (variable == "q" | variable == "qv") {
      loopvar <- variable
      unit <- "kg/kg"
    }

    if (length(data.vector) > length(self$dims$z$vals)) {
      data.vector <- data.vector[seq(self$dims$z$vals)]
    }
    if (length(data.vector) < length(self$dims$z$vals)) {
      new.data <- array(0, length(self$dims$z$vals))
      new.data[1:length(data.vector)] <- data.vector
      new.data[(length(data.vector) + 1):length(new.data)] <- data.vector[length(data.vector)]
      data.vector <- new.data
    }

    for (i in seq(loopvar)) {
      variable <- loopvar[i]
      adata <- list(
        "long_name" = paste0("Initial profile of ", variable),
        "source" = "Synthethic data from calculated profile",
        "units" = unit,
        "_FillValue" = -9999.9,
        "type" = "float",
        "coordinates" = "E_UTM N_UTM lon lat",
        "grid_mapping" = "crsUTM: E_UTM N_UTM crsETRS: lon lat",
        "lod" = 1,
        "vals" = data.vector
      )

      whichdimensions[[variable]] <- "z"

      data.name <- paste("init_atmosphere_", variable, sep = "")

      self$data[[data.name]] <- adata
      self$vardimensions[[data.name]] <- whichdimensions[[variable]]
    }
  },
  set_time = function(duration, steps) {
    if (duration < 100) {
      duration <- duration * 3600
    }
    if (steps < 10) {
      steps <- steps * 3600
    }
    dimen <- list()

    tvec <- seq(0, duration, by = steps)

    ######
    # Coordinates
    # X
    adata <- list(
      "long_name" = "time",
      "standard_name" = "time",
      "units" = "seconds since 2013-07-21 +00",
      "axis" = "T",
      "vals" = tvec
    )

    self$dims[["time"]] <- adata
  },
  set_forcing = function(variable, data,
                         boundary_side,
                         dim_of_data = NULL) {
    if (boundary_side == "north") {
      namevar <- "north"
      not_used_axis <- "y"
    } else if (boundary_side == "south") {
      namevar <- "south"
      not_used_axis <- "y"
    } else if (boundary_side == "left") {
      namevar <- "left"
      not_used_axis <- "x"
    } else if (boundary_side == "right") {
      namevar <- "right"
      not_used_axis <- "x"
    } else if (boundary_side == "top") {
      namevar <- "top"
      not_used_axis <- "z"
    } else {
      print("[ ] Only allowed boundary sides are:\n")
      print("    north, south, left, right, top")
    }
    if (variable == "pt") {
      unit <- "K"
      loopvar <- c("pt")

      selection_dims <- !grepl(not_used_axis, self$palmgrid[[variable]])
      used_dims <- self$palmgrid[[variable]][selection_dims]
    } else if (variable == "u" | variable == "v" | variable == "w") {
      unit <- "m/s"
      loopvar <- variable

      selection_dims <- !grepl(not_used_axis, self$palmgrid[[variable]])
      used_dims <- self$palmgrid[[variable]][selection_dims]
    } else if (variable == "qv") {
      loopvar <- variable
      unit <- "kg/kg"

      selection_dims <- !grepl(not_used_axis, self$palmgrid[[variable]])
      used_dims <- self$palmgrid[[variable]][selection_dims]
    } else {
      print("[ ] Only allowed variables are:\n")
      print("    pt, u, v, w, qv")
    }

    dim_dims <- do.call(rbind, lapply(used_dims, function(x) {
      length(self$dims[[x]]$vals)
    }))


    if (is.null(dim_of_data)) {
      data_vector <- array(data, dim_dims)
    } else if (dim_of_data == "z" & namevar == "top") {
      data_vector <- array(tail(data, 1), dim_dims)
    } else if (dim_of_data == "z") {
      dim_pos <- which(grepl(dim_of_data, used_dims))
      non_pos <- which(!grepl(dim_of_data, used_dims))

      if (dim_pos == 1) {
        print(paste("Variable:", loopvar))
        print(paste("Boundary:", namevar))
        print("dim_pos == 1, check data!")
        data_vector <- array(data, dim_dims)
      } else {
        data_vector <- array(rep(data, each = dim_dims[non_pos[1]]), dim_dims)
      }
    }

    variable <- loopvar
    adata <- list(
      "long_name" = paste0("large scale forcing for ", variable),
      "source" = "Synthethic data from calculated profile",
      "units" = unit,
      "_FillValue" = -9999.9,
      "type" = "float",
      "coordinates" = "E_UTM N_UTM lon lat",
      "grid_mapping" = "crsUTM: E_UTM N_UTM crsETRS: lon lat",
      "vals" = data_vector
    )

    whichdimensions <- used_dims

    data.name <- paste("ls_forcing_", namevar, "_", variable, sep = "")

    self$data[[data.name]] <- adata
    self$vardimensions[[data.name]] <- whichdimensions
  },
  export_dynamic_driver = function(filename) {
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
      nc_dim_list[[names(self$dims)[zz]]] <- ncdim_def(self$dims[[zz]]$long_name, self$dims[[zz]]$units,
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

    nameslist <- names(nc_dim_list)

    for (t in seq(self$data)) {
      dimlist <- list()
      for (ii in seq(self$vardimensions[[t]])) {
        dimlist[[self$vardimensions[[t]][ii]]] <- nc_dim_list[[self$vardimensions[[t]][ii]]]

        nameslist <- nameslist[-which(nameslist == self$vardimensions[[t]][ii])]
      }

      tmp <- ncvar_def(
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

    # Attribute, die normalerweiÃe nicht mehr extra vergeben werden mÃ¼ssen
    # Fuer loop ch in loopnum!
    ex_atts <- c("_FillValue", "units", "long_name", "vals", "type")

    # Erstellen der eigentlichen nc_file!
    ncfile <- nc_create(filename, vars = ncvariables, force_v4 = TRUE)

    # EinfÃ¼gen aller Attribute aus der Headerdatei palm_global als globale Attribute
    for (j in seq(self$header$head)) {
      ncatt_put(ncfile, 0, names(self$header$head)[j], self$header$head[[j]])
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
        ncatt_put(
          nc = ncfile, varid = names(self$data)[t], attname = loopvar,
          attval = unlist(self$data[[t]][ch])
        )
      }
      ncvar_put(ncfile,
        varid = ncfile$var[[t]]$name,
        vals = self$data[[t]]$vals
      )
    }
    # SchlieÃen und speichern der Datei
    nc_close(ncfile)
  },
  create_soil = function(data.vector, variable,
                         depths = c(0.005, 0.020, 0.060, 0.180, 0.540, 1.620, 4.860, 14.580)) {
    if (!"zsoil" %in% names(self$dims)) {
      adata <- list(
        "long_name" = "zsoil",
        "standard_name" = "depth_below_land",
        "axis" = "Z",
        "positive" = "down",
        "vals" = depths
      )

      self$dims[["zsoil"]] <- adata
    }

    if (variable == "pt") {
      lname <- paste("initial profile of soil temp")
      unit <- "K"

      data.name <- "init_soil_t"
    } else if (variable == "qv") {
      lname <- paste("initial profile of soil moisture")
      unit <- "m3/m3"

      data.name <- "init_soil_m"
    } else {
      print("Only pt and qv are allowed as input. Aborting")
      break
    }

    adata <- list(
      "long_name" = lname,
      "source" = "Synthethic data from calculated profile",
      "units" = unit,
      "_FillValue" = -9999.9,
      "type" = "float",
      "coordinates" = "E_UTM N_UTM lon lat",
      "grid_mapping" = "crsUTM: E_UTM N_UTM crsETRS: lon lat",
      "lod" = 1,
      "vals" = data.vector
    )

    whichdimensions <- c("zsoil")

    data.name <- "init_soil_t"

    self$data[[data.name]] <- adata
    self$vardimensions[[data.name]] <- whichdimensions
  },
  create_dummy_vars = function() {
    adata <- list(
      "long_name" = "dummy for zw",
      "source" = "Synthethic data from calculated profile",
      "units" = "K",
      "_FillValue" = -9999.9,
      "type" = "float",
      "coordinates" = "E_UTM N_UTM lon lat",
      "grid_mapping" = "crsUTM: E_UTM N_UTM crsETRS: lon lat",
      "vals" = self$data$init_atmosphere_u$vals[1:(length(self$data$init_atmosphere_u$vals) - 1)]
    )

    whichdimensions <- c("zw")

    data.name <- "dummy_zw"

    self$data[[data.name]] <- adata
    self$vardimensions[[data.name]] <- whichdimensions

    adata <- list(
      "long_name" = "dummy for xu",
      "source" = "Synthethic data from calculated profile",
      "units" = "K",
      "_FillValue" = -9999.9,
      "type" = "float",
      "coordinates" = "E_UTM N_UTM lon lat",
      "grid_mapping" = "crsUTM: E_UTM N_UTM crsETRS: lon lat",
      "vals" = self$data$ls_forcing_north_u$vals[1, 1:(length(self$data$ls_forcing_north_u$vals[1, , 1]) - 1), 1]
    )

    whichdimensions <- c("xu")

    data.name <- "dummy_xu"

    self$data[[data.name]] <- adata
    self$vardimensions[[data.name]] <- whichdimensions

    adata <- list(
      "long_name" = "dummy for yv",
      "source" = "Synthethic data from calculated profile",
      "units" = "K",
      "_FillValue" = -9999.9,
      "type" = "float",
      "coordinates" = "E_UTM N_UTM lon lat",
      "grid_mapping" = "crsUTM: E_UTM N_UTM crsETRS: lon lat",
      "vals" = self$data$ls_forcing_left_u$vals[1, 1:(length(self$data$ls_forcing_left_u$vals[1, , 1]) - 1), 1]
    )

    whichdimensions <- c("yv")

    data.name <- "dummy_yv"

    self$data[[data.name]] <- adata
    self$vardimensions[[data.name]] <- whichdimensions
  },
  prepopoulate_boundaries = function(pt = 300,
                                     qv = 0.001,
                                     u = 0,
                                     v = 0,
                                     w = 0) {
    for (vars in c("pt", "qv", "u", "v", "w")) {
      tmp <- switch(vars,
        "pt" = pt,
        "qv" = qv,
        "u"  = u,
        "v"  = v,
        "w"  = w
      )

      for (sides in c("left", "right", "south", "north", "top")) {
        self$set_forcing(
          variable = vars, boundary_side = sides,
          data = tmp, dim_of_data = NULL
        )
      }
    }
  },
  set_vertical_profile = function(var, data, dim_of_data = "z") {
    for (sides in c("left", "right", "south", "north", "top")) {
      self$set_forcing(
        variable = var, boundary_side = sides,
        data = data, dim_of_data = dim_of_data
      )
    }
  },
  shift_wind_by_topography <- function(zt_array = NULL, static_path = NULL) {
    boundaries <- c("left", "right", "north", "south")
    array_pos <- list(
      left_l = 1:nrow(topografy),
      right_l = 1:nrow(topografy),
      north_l = nrow(topografy),
      south_l = 1,
      left_r = 1,
      right_r = ncol(topografy),
      north_r = 1:ncol(topografy),
      south_r = 1:ncol(topografy)
    )

    if (!is.null(zt_array)) {
      topografy <- zt_array
    } else if (!is.null(static_path)) {
      nc <- ncdf4::nc_open(static_path)
      topografy <- ncdf4::ncvar_get(nc, "zt")
      ncdf4::nc_close(nc)
    }

    for (i in boundaries) {
      name_pos <- intersect(
        grep(paste("forcing_", i, sep = ""), names(self$data)),
        c(grep("_u", names(self$data)), grep("_v", names(self$data)), grep("_w", names(self$data)))
      )

      boundary_names <- names(self$data)[name_pos]
      topo_sclice <- topografy[array_pos[[paste0(i, "_l")]], array_pos[[paste0(i, "_r")]]]
      resolution <- self$dims$x$vals[2] - self$dims$x$vals[1]
      starting_points <- round(topo_sclice / resolution) + 1

      for (j in boundary_names) {
        tmp_array <- self$data[[j]]$vals
        dim_positions <- self$vardimensions[[j]]

        where_z <- grep("z", dim_positions)
        if (where_z == 2) {
          for (k in seq(dim(tmp_array)[1])) {
            tmp_array[k, starting_points[k]:dim(tmp_array)[2], ] <- tmp_array[k, 1:(dim(tmp_array)[2] - starting_points[k] + 1), ]
          }
        } else if (where_z == 1) {
          for (k in seq(dim(tmp_array)[2])) {
            tmp_array[starting_points[k]:dim(tmp_array)[1], k, ] <- tmp_array[1:(dim(tmp_array)[1] - starting_points[k] + 1), k, ]
          }
        }
        self$data[[j]]$vals <- tmp_array
      }
    }
  }
))
