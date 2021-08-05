#' Generic function to cut off n characters from a string
#'
#' @param x A string of characters
#' @param n Amount of characters to be returned
#'
#' @return The cut off string.
#' @export
#'
#' @examples a <- substrRight("R_is_awesome", 7)
#' a
substrRight <- function(x, n){

  substr(x, nchar(x)-n+1, nchar(x))
}


fillfunc_cheat <- function(startx,starty, array2d, height){
  if(array2d[startx,starty]==-127){
    array2d[startx,starty] <- height
    # image(array2d)
    if(startx+1<=dim(array2d)[1]){
      array2d <- fillfunc_cheat(startx+1, starty, array2d, height)
    }
    if(startx-1>0){
      array2d <-fillfunc_cheat(startx-1, starty, array2d, height)
    }
    if(starty+1<=dim(array2d)[2]){
      array2d <- fillfunc_cheat(startx, starty+1, array2d, height)
    }
    if(starty-1>0){
      array2d <- fillfunc_cheat(startx, starty-1, array2d, height)
    }
  }
  if(array2d[startx,starty]==1){
    array2d[startx,starty] <- height
    if(startx+1<=dim(array2d)[1]){
      if(array2d[startx+1,starty]==1){
        array2d[startx+1,starty] <- height
      }
    }
    if(startx-1>0){
      if(array2d[startx-1,starty]==1){
        array2d[startx-1,starty] <- height
      }
    }
    if(starty+1<=dim(array2d)[2]){
      if(array2d[startx,starty+1]==1){
        array2d[startx,starty+1] <- height
      }
    }
    if(starty-1>0){
      if(array2d[startx,starty-1]==1){
        array2d[startx,starty-1] <- height
      }
    }
  }
  return(array2d)

}

fillfunc <- function(startx,starty, array2d){
  if(array2d[startx,starty]==-127){
    array2d[startx,starty] <- 1
    # image(array2d)
    if(startx+1<=dim(array2d)[1]){
      array2d <- fillfunc(startx+1, starty, array2d)
    }
    if(startx-1>0){
      array2d <-fillfunc(startx-1, starty, array2d)
    }
    if(starty+1<=dim(array2d)[2]){
      array2d <- fillfunc(startx, starty+1, array2d)
    }
    if(starty-1>0){
      array2d <- fillfunc(startx, starty-1, array2d)
    }
  }
  return(array2d)

}

fillfunc_sett <- function(startx,starty, array2d){
  if(array2d[startx,starty]==-127){
    array2d[startx,starty] <- 4
    # image(array2d)
    if(startx+1<=dim(array2d)[1]){
      array2d <- fillfunc_sett(startx+1, starty, array2d)
    }
    if(startx-1>0){
      array2d <-fillfunc_sett(startx-1, starty, array2d)
    }
    if(starty+1<=dim(array2d)[2]){
      array2d <- fillfunc_sett(startx, starty+1, array2d)
    }
    if(starty-1>0){
      array2d <- fillfunc_sett(startx, starty-1, array2d)
    }
  }
  return(array2d)

}


#' Title
#'
#' @param matrix A 2D Matrix
#'
#' @return Returns a ggplot
#' @export
#'
#' @examples
#' x <- matrix(runif(100), ncol = 10, nrow =10 )
#' plot_2d_data(x)
plot_2d_data <- function(matrix){
  require(reshape2)
  require(ggplot2)
  meltvec  <- reshape2::melt(matrix)


  ggplot2::ggplot(meltvec, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11),
                       legend.text=element_text(size=7))
}

#' Helper function that specifies trailing positions of decimals
#'
#' @param x Any numeric
#' @param k Trailing positions of numeric
#'
#' @return A numeric with the chosen trailing positions
#' @export
#'
#' @examples specify_decimal( 3.14159265359, 3)
specify_decimal <- function(x, k) {
  trimws(format(round(x, k), nsmall=k))
}

#' Helper function for the attached shiny function. ArcGIS exports rastered data inverted, this function
#' aligns the data to the usual grid.
#'
#' @param data 2D data as used in the PALM classes
#' @param isarcgis Logical, if data was rastered with ArcGIS set to TRUE
#'
#' @return Returns 2D data, if isarcgis == TRUE, y-axis will be reversed
#' @export
#'
#' @examples a <- matrix(c(1:4),2,2)
#' b <- arcgischeck(a,TRUE)
#' all(a!=b)
arcgischeck     <- function(data, isarcgis){
  if(isarcgis){
    data <- data[,seq(dim(data)[2],by=-1)]
  } else if(!isarcgis){
    data <- data
  }
  return(data)
}


#' Generic function to cut off n characters from a string
#'
#' @param x A string of characters
#' @param n Amount of characters to be returned
#'
#' @return The cut off string.
#' @export
#'
#' @examples a <- substrRight("Awesome_is_R", 7)
#' a
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#' Title
#'
#' @param search_string Character. Which parameter to search in wkt string
#' @param wkt_text Character. Wkt string
#' @param numeric Boolean. Whether value is numeric or not
#'
#' @return
#' @export
#'
#' @examples
get_variable_from_wtk <- function(search_string, wkt_text, numeric = TRUE, trailing_comma = TRUE){
  trail = ""
  if(trailing_comma){
    trail = ","
  }

  pattern2 <-  paste0(".*", search_string, trail)

  wkt_value <- wkt_text %>%
    gsub(
      pattern = "\\\"",
      replacement = "",
      x = .
    ) %>%
    gsub(
      pattern = pattern2,
      replacement = "",
      x = .
    ) %>%
    gsub(
      pattern = ",.*",
      replacement = "",
      x = .
    )

  if(numeric){
    wkt_value <- as.numeric(wkt_value)
  }

  return(wkt_value)
}

