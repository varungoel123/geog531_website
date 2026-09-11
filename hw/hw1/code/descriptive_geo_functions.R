# ------------------------
# 1. Mean geographic center (centroid of coords)
# ------------------------

calc_mc <- function(sf_dat){
  mean_center <- data.frame(
    X = mean(st_coordinates(sf_dat)[,1]),
    Y = mean(st_coordinates(sf_dat)[,2])
  ) %>% st_as_sf(coords = c("X", "Y"), crs = st_crs(sf_dat))
  return(mean_center)
}

# ------------------------
# 2. Weighted mean geographic center
# ------------------------

calc_wmc <- function(sf_dat,weight_col){
  weighted_center <- data.frame(
    X = weighted.mean(st_coordinates(sf_dat)[,1], weight_col),
    Y = weighted.mean(st_coordinates(sf_dat)[,2], weight_col)
  ) %>% st_as_sf(coords = c("X", "Y"), crs = st_crs(sf_dat))
  return(weighted_center)
}

# ------------------------
# 3. Weighted Standard Distance
# ------------------------

calc_weighted_sd <- function(sf_dat, weight_col)
{
  # formula: sqrt( sum(wi*((xi-x̄)^2 + (yi-ȳ)^2)) / sum(wi) )
  mean_X = weighted.mean(st_coordinates(sf_dat)[,1], weight_col)
  mean_Y = weighted.mean(st_coordinates(sf_dat)[,2], weight_col)
  
  sdist <- sqrt(
    sum(weight_col * ((st_coordinates(sf_dat)[,1] - mean_X)^2 + (st_coordinates(sf_dat)[,2] - mean_Y)^2)) / sum(weight_col)
  )
  
  # Circle around weighted mean center
  circle_coords <- function(center, r, n=100){
    angles <- seq(0, 2*pi, length.out=n)
    x <- center[1] + r*cos(angles)
    y <- center[2] + r*sin(angles)
    data.frame(X=x, Y=y)
  }
  
  circle_df <- circle_coords(c(mean_X, mean_Y), sdist) %>%
    st_as_sf(coords = c("X","Y"), crs=st_crs(sf_dat)) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  return(circle_df)
  
}

# ------------------------
# 4. Weighted Standard Deviational Ellipse
# ------------------------


calc_sde_sf <- function(sf_dat, weight_col)
{
  mean_X = weighted.mean(st_coordinates(sf_dat)[,1], weight_col)
  mean_Y = weighted.mean(st_coordinates(sf_dat)[,2], weight_col)
  # Convert coords to matrix
  X <- cbind(st_coordinates(sf_dat)[,1], st_coordinates(sf_dat)[,2])
  
  # Weighted covariance
  cov_w <- cov.wt(X, wt=weight_col)$cov
  
  # Eigen decomposition (axes of ellipse)
  eig <- eigen(cov_w)
  radii <- sqrt(eig$values) * 2  # scale factor for 1 SD ellipse
  
  theta <- seq(0, 2*pi, length=200)
  ellipse <- t(t(eig$vectors %*% diag(radii)) %*% rbind(cos(theta), sin(theta)))
  ellipse <- sweep(ellipse, 2, c(mean_X, mean_Y), FUN="+")
  
  ellipse_df <- data.frame(X=ellipse[,1], Y=ellipse[,2]) %>%
    st_as_sf(coords=c("X","Y"), crs=st_crs(sf_dat)) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  return(ellipse_df)
}


# ------------------------
# Median geographic center (component-wise median)
# ------------------------
calc_medc <- function(sf_dat){
  median_center <- data.frame(
    X = median(st_coordinates(sf_dat)[,1]),
    Y = median(st_coordinates(sf_dat)[,2])
  ) %>% st_as_sf(coords = c("X", "Y"), crs = st_crs(sf_dat))
  return(median_center)
}

# ------------------------
# Weighted median geographic center
# ------------------------
calc_weighted_medc <- function(sf_dat, weight_col)
{
  weighted_median_center <- data.frame(
    X = Median(st_coordinates(sf_dat)[,1], w = weight_col),
    Y = Median(st_coordinates(sf_dat)[,2], w = weight_col)
  ) %>% st_as_sf(coords = c("X", "Y"), crs = st_crs(sf_dat))
  return(weighted_median_center)
}