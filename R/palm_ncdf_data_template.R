palm_ncdf_data_template <- R6::R6Class(classname = "palm_ncdf_data_template",
                                       public = list(
                                         data_list = list(),
                                         data_dims = list(),

#' Title
#'
#' @param dat_name  Variable Name for StaticDriver
#' @param FillValue Fill_Value of Data
#' @param d_units   Units of Data
#' @param longname  LongName
#' @param d_source  Source of Data
#' @param lod       Level of Detail
#' @param data      Data!
#' @param d_type    DataType
#' @param dimensions  Which Dimensions correspond to data?
#'
#' @return A R6 Class with the needed Setup for the palm_ncdf_berlin clones.
#' @export
#'
#' @examples
#' x <- 1
                                         initialize = function(dat_name,
                                                               FillValue,
                                                               d_units,
                                                               longname,
                                                               d_source,
                                                               lod,
                                                               data,
                                                               d_type,
                                                               dimensions){

                                           self$data_list[[dat_name]] <- list(
                                             "_FillValue" =  FillValue,
                                             "units" = d_units,
                                             "long_name" = long_name,
                                             "source" = d_source,
                                             "lod" = lod,
                                             "vals" = data,
                                             "type" = dtype
                                           )
                                           self$data_dims[[dat_name]] <- dimension

                                         },
                                         add_another_data = function(dat_name,
                                                                     FillValue,
                                                                     d_units,
                                                                     longname,
                                                                     d_source,
                                                                     lod,
                                                                     data,
                                                                     d_type,
                                                                     dimensions){

                                           self$data_list[[dat_name]] <- list(
                                             "_FillValue" =  FillValue,
                                             "units" = d_units,
                                             "long_name" = long_name,
                                             "source" = d_source,
                                             "lod" = lod,
                                             "vals" = data,
                                             "type" = dtype
                                           )
                                           self$data_dims[[dat_name]] <- dimension

                                         },
                                         print = function(){
                                            cat(paste("Data_set for palm_ncdf"))
                                         }

                                       )
)
