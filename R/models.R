
ocd_gam <- function (df, simple) {
  
  if (isTRUE(simple)) {
    mod <- gam(mgOC_cm3 ~ s(core, bs = 're') +
                 s(SITE_CODE, bs = 're') +
                 s(depth, k = 5, m = 2),
               data = df, method = 'REML',
               family = gaussian(link = "log"))
  }
  
  if (!isTRUE(simple)) {
    mod <- gam(mgOC_cm3 ~ s(core, bs = 're') +
                 s(SITE_CODE, bs = 're') +
                 s(depth, k = 5, m = 2) +
                 s(depth, SITE_CODE, k = 5, bs = 'fs', m = 2),
               data = df, method = 'REML',
               family = gaussian(link = "log"))    
  }

  return(mod)  
}

stock_gam <- function (df, re) {
  
  if (isTRUE(re)) {
    mod <- gam(stock_Mg_h ~ s(SITE_CODE, bs = 're') +
                 s(annprecip, k = 5, m = 2) +
                 s(ndvi, k = 5, m = 2) + 
                 s(twi, k = 5, m = 2) + 
                 s(mvbf, k = 5, m = 2) + 
                 s(VGAM::yeo.johnson(water_obs, .2), k = 5, m = 2) + 
                 s(log(catchment_area), k = 5, m = 2) +
                 s(natveg_prop, k = 5, m = 2),
               select = TRUE,
               data = df, method = 'REML',
               family = gaussian(link = "log"))
  }
  
  if (!isTRUE(re)) {
    mod <- gam(stock_Mg_h ~ 
                 s(annprecip, k = 5, m = 2) + 
                 s(ndvi, k = 5, m = 2) + 
                 s(twi, k = 5, m = 2) + 
                 s(mvbf, k = 5, m = 2) + 
                 s(VGAM::yeo.johnson(water_obs, .2), k = 5, m = 2) + 
                 s(log(catchment_area), k = 5, m = 2) +
                 s(natveg_prop, k = 5, m = 2),
               select = TRUE,
               data = df, method = 'REML',
               drop.unused.levels = FALSE,
               family = gaussian(link = "log"))
  }
  
  return(mod)
}

cv_iteration <- function (selected_site, df) {
  
  train <- df[df$SITE_CODE != selected_site,]
  test <- df[df$SITE_CODE == selected_site,]
  
  # create new factor unique for core
  mod <- stock_gam(train, re = FALSE)
  
  pred <- predict.gam(mod, newdata = test, type = 'terms', 
                      terms = c('s(annprecip)', 's(ndvi)', 's(twi)', 's(mvbf)', 
                                's(VGAM::yeo.johnson(water_obs, 0.2))', 's(log(catchment_area))', 's(natveg_prop)'))
  pred <- attr(pred, "constant") + rowSums(pred)
  pred_response <- mod$family$linkinv(pred)
  test$pred <- pred_response
  
  return(test)
}

cv <- function (df) {
  
  sites <- unique(as.character(model_df$SITE_CODE))
  
  cv_mod <- lapply(sites, cv_iteration, model_df) %>%
    dplyr::bind_rows()
  
  return(cv_mod)
  
}

