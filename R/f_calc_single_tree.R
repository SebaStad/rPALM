#' Calculates the LAD and BAD of a single tree!
#'
#' @param tree_type_b Integer. TreeType according to a "database" so far, only fillvalues!
#' @param tree_shape Integer. TreeShape, can be set (1-6), or database, then according to TreeType
#' @param crown_ratio Numeric. Ratio of the Crown?
#' @param crown_diameter Numeric. Diameter of the Crown, MUST BE SET as database has 0!
#' @param tree_height Numeric. Database entry is 12
#' @param lai Numeric. Database entry is 2
#' @param dbh Numeric. Database entry is 0.35
#' @param extinktion_sphere Numeric. ?
#' @param extinktion_cone Numeric. =
#' @param fillvalue Numeric. -9999.9
#'
#' @return A list with a 3D LAD array, a 3D BAD array and the species of the tree
#' @export
#'
#' @examples
#' f.calc_single_tree(tree_type_b = 1, crown_diameter = 10, dx = 2)
f.calc_single_tree <- function(tree_type_b = NULL,
                         tree_shape = NULL,
                         crown_ratio = NULL,
                         crown_diameter = NULL,
                         tree_height = NULL,
                         lai = NULL,
                         dbh = NULL,
                         extinktion_sphere = 0.6,
                         extinktion_cone = 0.2,
                         fillvalue = -9999.9,
                         dx){
  dy <- dx
  dz <- dx
  if ( is.null(tree_type_b)) {
    tree_type <- 1
  } else {
    tree_type <- tree_type_b
  }

  if ( is.null(tree_shape) ) {
  tree_shape         <- tree_shape_parameters$shape[tree_type]
  }

  if ( is.null(crown_ratio) ) {
  crown_ratio        <- tree_shape_parameters$ratio[tree_type]
  }

  if ( is.null(crown_diameter) ) {
  crown_diameter     <- tree_shape_parameters$diameter[tree_type]
  }

  if ( is.null(tree_height) ) {
  tree_height        <- tree_shape_parameters$height[tree_type]
  }

  if ( is.null(lai) ) {
  lai                <- tree_leaf_parameters$lai.summer[tree_type]
  }

  if ( is.null(dbh) ) {
  dbh                <- tree_trunk_parameters$dbh[tree_type]
  }

  # Some values are always taken from lookup table
  lad_max            <- tree_leaf_parameters$lad.max[tree_type]
  lad_max_scale      <- tree_leaf_parameters$height.scale.max[tree_type]
  bad_scaling_factor <- tree_trunk_parameters$scale[tree_type]

  # Calculate crown height and height of the crown center
  crown_height   <- crown_ratio * crown_diameter
  crown_center   <- tree_height - crown_height * 0.5


  # Calculate the height where LAD is maximum
  z_lad_max    <- lad_max_scale * tree_height


  # Define tree position and output domain
  nx <- as.integer(crown_diameter / dx)
  ny <- as.integer(crown_diameter / dx)
  nz <- as.integer(tree_height / dz)

  if ( crown_diameter%%2!=0 ){
    nx <- nx + 1
    ny <- ny + 1
  }

  x <- array(0, nx+1)
  for(i in seq(x)){
    x[i] <- dx * i - 0.5 * dx
  }

  y <- array(0, ny+1)
  for(j in seq(y)){
    y[j] <- dx * j - 0.5 * dx
  }

  z <- array(0, nz+1)
  for(k in seq(z)){
    z[k] <- dz * k - 0.5 * dz
  }

  tree_location_x <- x[ny/2 + 1]
  tree_location_y <- y[ny/2 + 1]


  # Calculate LAD profile after Lalic and Mihailovic (2004)
  lad_profile     <-  array(0,nz+1)
  lad_profile[1]  <- 0.0
  lad_profile[nz+1] <- 0.0

  for(k in 2:(nz)){
    if ( z[k] >= 0.0 && z[k] < z_lad_max ){
      n <- 6.0
    } else {
      n <- 0.5
    }
    lad_profile[k] <- lad_max * ( ( tree_height - z_lad_max ) / ( tree_height - z[k] ) )^n * exp( n * (1.0 - ( tree_height - z_lad_max ) / ( tree_height - z[k] ) ) )
  }



  # Create and populate 3D LAD array
  lad_3d <- array(0, c(nx+1, ny+1, nz+1))

  # Branch for spheres and ellipsoids
  if( tree_shape == 1 ){
    for(i in seq(nx+1)){
      for(j in seq(ny+1)){
        for(k in seq(nz+1)){
          r_test <- sqrt( (x[i] - tree_location_x)^2/(crown_diameter * 0.5)^2 + (y[j] - tree_location_y)^2/(crown_diameter * 0.5)^2 + (z[k] - crown_center)^2/(crown_height * 0.5)^2)
          if ( r_test <= 1.0 ){
                     # lad_3d(k,j,i) = lad_profile(k) * exp ( - extinktion_sphere * (1.0 - r_test) )
            lad_3d[i,j,k] <- lad_max * exp( - extinktion_sphere * (1.0 - r_test) )
          } else {
            lad_3d[i,j,k] <- fillvalue
          }
        }
        if ( !(all(lad_3d[i,j,]==0)) ){
          lad_3d[i,j,1] <- 0.0
        }
      }
    }
  }

  test <- which(lad_3d==fillvalue)
  lad_3d[test] <- 0


  for(k in seq(nz+1)){
    if ( any(lad_3d[,,k] > 0.0) ){
      lad_3d[,,k] <- ( lad_3d[,,k] / sum(lad_3d[,,k]) )* lad_profile[k]
    }
  }
  lad_3d[test] <- fillvalue

  # Branch for cylinder shapes
  if ( tree_shape == 2 ){
    k_min = as.integer((crown_center - crown_height * 0.5) / dz)
    k_max = as.integer((crown_center + crown_height * 0.5) / dz)
    for(i in seq(nx+1)){
      for(j in seq(ny+1)){
        for(k in k_min:k_max){ # do k = k_min, k_max
          r_test = sqrt( (x[i] - tree_location_x)^2/(crown_diameter * 0.5)^2 + (y[j] - tree_location_y)^2/(crown_diameter * 0.5)^2)
          if ( r_test <= 1.0 ){
            r_test3 = sqrt( (z[k] - crown_center)^2/(crown_height * 0.5)^2)
            lad_3d[i,j,k] = lad_max * exp ( - extinktion_sphere * (1.0 - max(c(r_test,r_test3)) ) )
          } else {
            lad_3d[i,j,k] <- fillvalue
          }
        }
        if ( !(all(lad_3d[i,j,]==0)) ){
          lad_3d[i,j,1] <- 0.0
        }
      }
    }
  }


  # Branch for cone shapes
  if ( tree_shape == 3 ) {
    k_min <- as.integer((crown_center - crown_height * 0.5) / dz)
    k_max <- as.integer((crown_center + crown_height * 0.5) / dz)
    for(i in seq(nx+1)){
      for(j in seq(ny+1)){
        for(k in k_min:k_max){
          k_rel  <- k - k_min + 1
          r_test <- (x[i] - tree_location_x)^2 + (y[j] - tree_location_y)^2 - ( (crown_diameter * 0.5)^2 / crown_height^2 ) * ( z[k_rel] - crown_height)^2
          if(r_test <= 0.0){
            r_test2 <- sqrt( (x[i] - tree_location_x)^2/(crown_diameter * 0.5)^2 + (y[j] - tree_location_y)^2/(crown_diameter * 0.5)^2)
            r_test3 <- sqrt( (z[k] - crown_center)^2/(crown_height * 0.5)^2)
            lad_3d[i,j,k]  <- lad_max * exp ( - extinktion_cone * (1.0 - max(c((r_test+1.0),r_test2,r_test3)) )   )
          } else {
            lad_3d[i,j,k] <- fillvalue
          }
        }
        if( !(all(lad[i,j,] == 0 ))){
          lad_3d[i,j,1] <- 0
        }
      }
    }
  }

  # Branch for inverted cone shapes
  if ( tree_shape == 4 ){
    k_min = as.integer((crown_center - crown_height * 0.5) / dz)
    k_max = as.integer((crown_center + crown_height * 0.5) / dz)
    for(i in seq(nx+1)){
      for(j in seq(ny+1)){
        for(k in k_min:k_max){
          k_rel  <- k_max - k
          r_test <- (x[i] - tree_location_x)^2 + (y[j] - tree_location_y)^2 - ( (crown_diameter * 0.5)^2 / crown_height^2 ) * ( z[k_rel] - crown_height)^2
          if(r_test <= 0){
            r_test3       <- sqrt( (x[i] - tree_location_x)^2/(crown_diameter * 0.5)^2 + (y[j] - tree_location_y)^2/(crown_diameter * 0.5)^2)
            lad_3d[i,j,k] <- lad_max * exp ( - extinktion_cone * ( - r_test ) )
          } else {
            lad_3d[i,j,k] <- fillvalue
          }
        }
        if( !(all(lad[i,j,] == 0 ))){
          lad_3d[i,j,1] <- 0
        }
      }
    }
  }

  # Branch for paraboloid shapes
  if ( tree_shape == 5 ){
    k_min = as.integer((crown_center - crown_height * 0.5) / dz)
    k_max = as.integer((crown_center + crown_height * 0.5) / dz)
    for(i in seq(nx+1)){
      for(j in seq(ny+1)){
        for(k in k_min:k_max){
          k_rel  <- k - k_min
          r_test <- ((x[i] - tree_location_x)^2 + (y[j] - tree_location_y)^2) * crown_height / (crown_diameter * 0.5)^2 - z[k_rel]
          if(r_test <= 0){
            lad_3d[i,j,k] <- lad_max * exp ( - extinktion_cone * (- r_test) )
          } else {
            lad_3d[i,j,k] <- fillvalue
          }
        }
        if( !(all(lad[i,j,] == 0 ))){
          lad_3d[i,j,1] <- 0
        }
      }
    }
  }

  # Branch for inverted paraboloid shapes
  if ( tree_shape == 6 ){
    k_min = as.integer((crown_center - crown_height * 0.5) / dz)
    k_max = as.integer((crown_center + crown_height * 0.5) / dz)
    for(i in seq(nx+1)){
      for(j in seq(ny+1)){
        for(k in k_min:k_max){
          k_rel <- k_max - k
          r_test <- ((x[i] - tree_location_x)^2 + (y[j] - tree_location_y)^2) * crown_height / (crown_diameter * 0.5)^2 - z[k_rel]
          if(r_test <= 0){
            lad_3d[i,j,k] <- lad_max * exp ( - extinktion_cone * (- r_test) )
          } else {
            lad_3d[i,j,k] <- fillvalue
          }
        }
        if( !(all(lad[i,j,] == 0 ))){
          lad_3d[i,j,1] <- 0
        }
      }
    }
  }



  # Define and populate basal area density array
 #  bad_3d = new( (/nz+1,ny+1,nx+1/), float)


  # First, create BAD field based on LAD field
  tmp <- lad_3d
  tmp[tmp==fillvalue] <- 0
  bad_3d <- tmp * bad_scaling_factor
  rm(tmp)

  # Overwrite grid cells that are occupied by the trunk
  radius = dbh * 0.5

  for(i in seq(nx+1)){
    for(j in seq(ny+1)){
      for( k in seq(nz+1)){
        if(z[k]<= crown_center){
          r_test <- sqrt( (x[i] - tree_location_x)^2 + (y[j] - tree_location_y)^2)
          if(r_test <=radius){
            bad_3d[i,j,k] <- 1
          }
          if(r_test == 0 && dbh <= dx){
            bad_3d[i,j,k] <- radius^2*pi
          }
        }
      }
    }
  }
  bad_3d[bad_3d==0] <- fillvalue



  # Define and output data

  output <- list("lad" = lad_3d,
                 "bad" = bad_3d,
                 "species" = tree_type_names[tree_type])

  # remove data if tree is too shallow
  if ( tree_height <= 0.5*dz ){
    print("    Tree is removed (too shallow)!")
    output = NULL
  }

  return(output)

}
