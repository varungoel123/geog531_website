
# ------------------------
# NOTE: IF YOU SEE THIS LINE THEN THIS IS THE CORRECT FILE WITH THE CORRECT
# WEIGHTED STANDARD DEVIATIONAL ELLIPSE FUNCTION
# ------------------------

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
# 3. Weighted Standard Deviational Ellipse
# ------------------------


calc_sde_sf <- function(sf_dat, weight_col ) {
  
  ellipse_size = 1
  
  coords <- sf::st_coordinates(sf_dat)[, 1:2, drop = FALSE]
  w <- as.numeric(weight_col)
  
  if (any(!is.finite(w)) || any(w < 0) || sum(w) == 0) {
    stop("Weights must be finite, non-negative, and sum to more than zero.")
  }
  
  # Weighted mean center
  center <- colSums(coords * w) / sum(w)
  
  # Population-weighted covariance matrix
  cov_w <- stats::cov.wt(coords, wt = w, method = "ML")$cov
  
  # Ellipse orientation and axis variances
  eig <- eigen(cov_w)
  
  # Unadjusted ellipse: directly comparable to standard distance
  radii <- ellipse_size * sqrt(pmax(eig$values, 0))
  
  theta <- seq(0, 2 * pi, length.out = 199)
  circle <- rbind(cos(theta), sin(theta))
  
  ellipse_coords <- t(eig$vectors %*% diag(radii) %*% circle)
  ellipse_coords <- sweep(ellipse_coords, 2, center, "+")
  
  # Explicitly close the polygon ring
  ellipse_coords <- rbind(
    ellipse_coords,
    ellipse_coords[1, , drop = FALSE]
  )
  
  sf::st_sf(
    CenterX = center[1],
    CenterY = center[2],
    LongAxis = radii[1],
    ShortAxis = radii[2],
    geometry = sf::st_sfc(
      sf::st_polygon(list(ellipse_coords)),
      crs = sf::st_crs(sf_dat)
    )
  )
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