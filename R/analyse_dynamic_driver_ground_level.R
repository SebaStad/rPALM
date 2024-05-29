#' Get ground layer of dynamic driver file as a timeseries of different
#' metrices. Can either just read the lowest z_dim or dependent on the
#' static driver the first layer above topography
#'
#' @param file_path_dynamic character. Path to dynamic driver
#' @param file_path_static character. Path to static driver
#' @param t0 datetime. Must be POSIXct
#' @param parameter character. For now only pt and qv work
#' @param unit character.
#' @param offset numeric. Can offset all parameters, e.g. absolute temperature
#' to degree celsius
#'
#' @return
#' @export
#'
#' @examples
analyse_dynamic_driver_ground_level <- function(
    file_path_dynamic,
    file_path_static = NULL,
    t0,
    parameter = "pt",
    unit = "degree",
    offset = 0
) {
  # Open connection
  nc_dyn <- ncdf4::nc_open(file_path_dynamic)
  # Round timesteps to nearest 10 seconds for easier merging

  if(!is.null(file_path_static)) {
    nc_static <- ncdf4::nc_open(file_path_static)

    z_dim <- ncdf4::ncvar_get(nc_dyn, "z")
    z_res <- diff(z_dim)[1]

    topography_in_gridpoints <- round(
      ncvar_get(nc_static, "zt") / z_res
    ) + 1
    ncdf4::nc_close(nc_static)


    topo_border_list <- list()
    topo_border_list$left <- topography_in_gridpoints[,1]
    topo_border_list$south <- topography_in_gridpoints[1,]
    topo_border_list$right <- topography_in_gridpoints[,ncol(topography_in_gridpoints)]
    topo_border_list$north <- topography_in_gridpoints[nrow(topography_in_gridpoints),]

  }
  directions <- c(
    "left", "right", "south", "north"
  )

  time_steps_dyn <- round(
    ncvar_get(nc_dyn, "time") * 0.1
  ) * 10

  if(!any(grepl("POSIXct", class(t0)))) {
    cli::cli_alert_danger("t0 is not datetime / POSIXct")
    stop("t0 is not datetime / POSIXct")
  }
  plot_ts_info <- t0 + time_steps_dyn

  all_vars <- names(nc_dyn$var)
  pt_vars <- all_vars[grepl(parameter, all_vars)]

  # we only analyze bc variables
  pt_vars_no_top <- pt_vars[-which(grepl("top", pt_vars))]
  pt_bc_vars <- pt_vars_no_top[-which(grepl("init", pt_vars_no_top))]

  all_vars_collection <- list()

  cli::cli_h2("Reading parameters for {parameter}")
  cli::cli_progress_bar(
    "Readind and preparing data",
    total = length(pt_bc_vars) * length(time_steps_dyn)
  )
  for (varname in pt_bc_vars) {
    # this could get troublesome later due to the data amount
    all_dataset <- ncdf4::ncvar_get(
      nc_dyn,
      varname
    ) + offset

    if (is.null(file_path_static)) {
      z_slice <- 1
      ground_slice <- all_dataset[,z_slice,]
    } else {
      which_direction <- stringr::str_detect(varname, directions)
      this_direction <- directions[which_direction]

      z_slice <- topo_border_list[[this_direction]]
      ground_slice <- array(
        0, dim = dim(all_dataset)[c(1,3)]
      )
      for(x in seq_along(z_slice)) {
        ground_slice[x,] <- all_dataset[x,z_slice[x],]
      }

    }

    all_vars_collection[[varname]] <- vector(
      "list",
      length = dim(ground_slice)[2]
    )

    for (i in seq_along(all_vars_collection[[varname]])) {
      all_vars_collection[[varname]][[i]] <- data.frame(
        wrf_min = min(ground_slice[,i], na.rm = TRUE),
        wrf_per5 = quantile(ground_slice[,i], 0.05, na.rm = TRUE),
        wrf_per25 = quantile(ground_slice[,i], 0.25, na.rm = TRUE),
        wrf_median = median(ground_slice[,i], na.rm = TRUE),
        wrf_mean = mean(ground_slice[,i], na.rm = TRUE),
        wrf_per75 = quantile(ground_slice[,i], 0.75, na.rm = TRUE),
        wrf_per95 = quantile(ground_slice[,i], 0.95, na.rm = TRUE),
        wrf_max = max(ground_slice[,i], na.rm = TRUE)
      )
      cli::cli_progress_update()
    }


  }
  cli::cli_h2("Merging data.frames")
  plotable_collection <- list()
  for (varname in pt_bc_vars) {
    plotable_collection[[varname]] <- do.call(rbind, all_vars_collection[[varname]])
  }

  cli::cli_h2("Converting to ggplot data")
  each_side <- lapply(
    plotable_collection,
    function(x){
      x %>%
        mutate(date = plot_ts_info) %>%
        pivot_longer(
          cols = starts_with("wrf"),
          names_to = "metric",
          values_to = unit
        )
    }
  )
  ncdf4::nc_close(nc_dyn)
  return(each_side)
}
