calculate_stock <- function (df, mod, max_type) {
  
  sites_df <- unique(df[, c('SITE_CODE', 'core')])
  sites_list <- setNames(split(sites_df, seq(nrow(sites_df))), rownames(sites_df))
  all_stock <- lapply(sites_list, stock, df, mod, max_type)
  
  all_stock_df <- dplyr::bind_rows(all_stock) %>%
    merge(df[,c('core', 'SITE_NAME', 'CMA_NAME', 'RESERVE_NAME', 
                'DATE', 'LAT', 'LON', 'C_TYPE')], 
          by = 'core') %>%
    unique()
  
  return(all_stock_df)
  
}

stock <- function (site_core, df, model, max_type) {
  
  ## trapezoidal integration ...
  n <- 1000 ## number of points in trap. approx
  
  site <- site_core$SITE_CODE
  core <- site_core$core
  
  # integrate to maximum depth of core
  if (max_type == 'core') {
    max_depth <- max(df$depth[df$SITE_CODE == site & df$core == core])
  }
  
  # integrate to maximum depth at site
  if (max_type == 'site') {
    max_depth <- max(df$depth[df$SITE_CODE == site])
  }
  
  # integrate to 1 m
  if (max_type == 'IPCC') {
    max_depth <- 100  
  }
  
  site_core_ext <- site_core[rep(seq_len(nrow(site_core)), each = n), ]
  
  dep <- seq(0, max_depth, length = n)
  pd <- cbind(data.frame(depth = dep), site_core_ext)
  dx <- pd$depth[2] - pd$depth[1]
  Xf <- predict.gam(model, pd, se = TRUE, type = "lpmatrix")
  w <- rep(dx, n); w[1] <- w[n] <- w[2]/2 ## trapezoidal weights
  stock <- t(w) %*% exp(Xf %*% coef(model)) ## integral mg/cm2
  
  br <- MASS::mvrnorm(n, coef(model), vcov(model))
  max.dep <- rep(NA, n)
  
  for (i in 1:n) {
    
    fv <- exp(Xf %*% br[i, ])
    max.dep[i] <- dep[fv == max(fv)]
  
  }
  
  ci <- quantile(max.dep, c(.025, .975))

  # stock_error <- sqrt(t(w) %*% exp(Xf %*% model$Vp) %*% t(Xf) %*% w) 
  ## its standard error
  
  return(data.frame(SITE_CODE = site,
                    core = core,
                    max_depth = max_depth,
                    stock_mg_cm2 = stock[1],
                    stock_Mg_h = stock[1] / 10, # change units
                    lower_stock = ci[[1]],
                    upper_stock = ci[[2]]))
  
}

create_model_df <- function (spatial_stock_data, path = '') {
  
  r <- aggregate(shapefile(paste0(path, 'shapefiles/processed/cma.shp')), 
                 dissolve = TRUE) %>%
    as("SpatialPolygonsDataFrame")
  
  ndvi <- raster::mask(raster(x = paste0(path, 
                                         'shapefiles/processed/ndvi.tif')), r)
  temp <- raster::mask(raster(x = paste0(path, 
                                         'shapefiles/processed/meananntemp.tif')), r)
  prec <- raster::mask(raster(x = paste0(path, 
                                         'shapefiles/processed/annprecip.tif')), r)
  twi <- raster::mask(raster(x = paste0(path, 
                                        'shapefiles/processed/twi.tif')), r)
  mvbf <- raster::mask(raster(x = paste0(path, 
                                         'shapefiles/processed/mvbf.tif')), r)
  
  # stack covariates
  covariates <- raster::stack(ndvi,
                              prec,
                              temp,
                              twi, 
                              mvbf)
  
  covariate_values <- raster::extract(covariates, spatial_stock_data)
  
  model_df <- cbind(as.data.frame(spatial_stock_data), 
                    as.data.frame(covariate_values))
  
  return(model_df)
  
}

covs_only <- function (df) {
  na.omit(df[, c('twi', 'mvbf', 'annprecip', 'aridity', 
                       'meananntemp', 'ndvi', 'natveg_prop', 
                       'catchment_area', 'water_obs')])  
}

