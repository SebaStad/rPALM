#' Calculate Patch LAD fpr PALM-4U
#'
#' @param lai Array. Containts Leaf Area Index for respective patches.
#' @param canopy_heigh Array. Contains tree height for respective patches.
#' @param dz Integer. Grid dimension in y direction
#' @param alpha Numeric. A Parameter for LAD calculation. See source.
#' @param beta Numeric. A Parameter for LAD calculation. See Source.
#' @param fillvalue Numeric. Fillvalue for the array
#'
#' @return Array. Filled with LAD for each tree patch.
#' @export
#'
#' @examples
#' lai <- array(2, c(5,5))
#' canopy <- array(10, c(5,5))
#' dz <- 2
#' lad <- f.calc_tree_patch(lai, canopy, dz)
f.calc_tree_patch <- function(lai = array(), canopy_height = array(),
                              dz, alpha = 5, beta = 3, fillvalue = -9999.9){
  canopy_height[is.na(canopy_height)] <- 0
  canopy_height[canopy_height<0]      <- 0

  pch_index <- array(as.integer(canopy_height/dz), dim(lai))

  z <- seq(0, max(pch_index,na.rm=TRUE),by=1) * dz
  z <- z - (dz/2)
  z[1] <- 0

  pre_lad <- array(0, length(z))

  lad_array <- array(0, c(dim(pch_index),length(pre_lad)))
  for(i in seq(dim(lad_array)[1])){
    for(j in seq(dim(lad_array)[2])){
      int_bpdf <- 0

      if(canopy_height[i,j]!=0 && (canopy_height[i,j]>= 0.5*dz)){

        for(k in seq(pch_index[i,j]+1)){
          int_bpdf <- int_bpdf + ( ( ( z[k] / canopy_height[i,j] )^( alpha - 1 ) ) * ( ( 1.0 - ( z[k] / canopy_height[i,j] ) )^(beta - 1 ) ) * ( dz / canopy_height[i,j] ) )
        }

        for(k in seq(pch_index[i,j]+1)){
          pre_lad[k] <-   lai[i,j] * ( ( ( dz*(k-1) / canopy_height[i,j] )^( alpha - 1 ) ) * ( ( 1.0 - ( dz*(k-1) / canopy_height[i,j] ) )^(beta - 1 ) ) / int_bpdf ) / canopy_height[i,j]
        }

        lad_array[i,j,1] <- pre_lad[1]
        for(k in 2:(pch_index[i,j]+1)){
          lad_array[i,j,k] <- 0.5*(pre_lad[k-1]+pre_lad[k])
        }

      }
    }
  }

  return(lad_array)
}
