dev_explained <- function (mod) {
  my_dev <- mgcv::summary.gam(mod)$dev.expl
  return(my_dev)
}

ocd_mod_cor <- function (mod, df) {
  pred <- predict.gam(mod)
  attributes(pred) <- NULL
  cor(pred, df$mgOC_cm3)^2
}

variance_component <- function (mod, term) {
  
  var <- mgcv::gam.vcomp(mod)[term,1]
  return(var)
  
}
