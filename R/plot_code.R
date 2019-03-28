unused <- function () {
  # plot the effect
  n_pred <- 100
  depth_values <- seq(0, 140, length = n_pred)
  ndvi_values <- c(-1, 0, 1)
  depth_pred <- rep(depth_values, times = length(ndvi_values))
  ndvi_pred <- rep(ndvi_values, each = length(depth_values))
  # expand.grid
  predict(mod, newdata = data.frame(depth = depth_pred, ndvi = ndvi_pred))
  
  
  
  
  smooth_terms <- sapply(stock_mod$smooth, "[[", "label")
  
  
  # how get se? 
  df_dere <- data.frame(SITE_CODE = rep('DERE', 1000),
                        core = rep('DERE1', 1000),
                        depth = seq(min(samples$depth), max(samples$depth), length.out = 1000))
  pred_link <- predict.gam(OCD_mod, df_dere, type = 'link', se = TRUE)#, terms = "s(depth)")
  # pred_link <- attr(pred_link, "constant") + rowSums(pred_link)
  pred_response <- OCD_mod$family$linkinv(pred_link$fit)
  plot(df_dere$depth, pred_response)
  
}



