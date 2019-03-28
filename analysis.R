## Analysis file
library(raster)
library(magrittr)
library(mgcv)

path <- 'chapters/Chapter2_carbon/'
R.utils::sourceDirectory(paste0(path, 'R/'))

# data prep
lab_data <- process_lab_results(lab_data = 
                                  paste0(path, "data-raw/VWC_Lab_Results.csv"))

samples <- prepare_master_data(site_data = paste0(path, "data-raw/VWC_SitesCores_Masterlist.csv"),
                               sample_data = paste0(path, "data-raw/VWC_Samples_Masterlist.csv"),
                               wetland_types = paste0(path, "data-raw/PC_carbon.csv"),
                               lab_data = lab_data)

# write.csv(samples[,c('SITE_CODE', 'LAT', 'LON')], paste0(path, 'samples_latlon.csv'))

# stock GAM
OCD_mod <- ocd_gam(samples, simple = FALSE)
# saveRDS(OCD_mod, paste0(path, 'OCD_mod.rds'))
# OCD_mod <- readRDS(paste0(path, 'OCD_mod.rds'))

var_core <- variance_component(OCD_mod, 1)
var_site <- variance_component(OCD_mod, 2)

OCD_mod_simple <- ocd_gam(samples, simple = TRUE)
# saveRDS(OCD_mod_simple, paste0(path, 'OCD_mod_simple.rds'))

# hist(resid(OCD_mod))
# plot(resid(OCD_mod) ~ fitted(OCD_mod))
# gam.check(OCD_mod)
# plot(OCD_mod, pages = 1, residuals = TRUE)

stock_data_site <- calculate_stock(samples, OCD_mod, 'site')

stock <- combine_other_spatial(stock_data_site, 
                               ala_file = 'chapters/Chapter2_carbon/data-raw/ALA_layers.csv',
                               catchments_file = 'chapters/Chapter2_carbon/data-raw/sites_with_catchment_data.csv')

spatial_stock <- make_samples_spatial(stock, '+proj=longlat +datum=WGS84 +no_defs')

sites_only <- samples[!duplicated(samples$SITE_CODE), c('SITE_CODE', 'LAT', 'LON')]
# write.csv(sites_only, paste0(rel_path_output, 'sites_only.csv'))
spatial_sites <- make_samples_spatial(sites_only, '+proj=longlat +datum=WGS84 +no_defs')
# rgdal::writeOGR(spatial_sites, rel_path_output, 'spatial_sites', driver = 'ESRI Shapefile')

model_df <- create_model_df(spatial_stock)
# saveRDS(model_df, paste0(path, 'model_df.rds'))
stock_covs <- covs_only(model_df)

stock_mod <- stock_gam(model_df, re = TRUE)
stock_mod_nore <- stock_gam(model_df, re = FALSE)

# hist(resid(stock_mod))
# plot(resid(stock_mod) ~ fitted(stock_mod))
# gam.check(stock_mod)
# plot(stock_mod, pages = 1, residuals = TRUE)

cv_mod <- cv(model_df)
# saveRDS(cv_mod, paste0(path, 'cv_mod.rds'))
cv_mod <- readRDS(paste0(path, 'cv_mod.rds'))

# gam.vcomp
# residual variance? Scale est. ?

# figures
png(paste0(path, 'modified-figs/ndvi.png'), bg = 'transparent')
covariate_plot('ndvi')
dev.off()

png(paste0(path, 'modified-figs/precip.png'), bg = 'transparent')
covariate_plot('annprecip')
dev.off()

png(paste0(path, 'figs/wetland_type.png'))
bp('C_TYPE')
dev.off()

png(paste0(path, 'figs/aus_map.png'), height = 800, width = 1000, bg = 'transparent')
aus_map(file_location = paste0(path, 'shapefiles/raw'))
dev.off()

png(paste0(path, 'figs/site_map.png'), height = 800, width = 1100)
sites_map(spatial_sites, 
          file_location = paste0(path, 'shapefiles/raw'))
dev.off()

png(paste0(path, 'modified-figs/methods_subplot.png'), 
    height = 400, width = 500, bg = 'transparent')
method_subplot(samples, OCD_mod)
dev.off()

png(paste0(path, 'figs/s_sites_depth.png'), height = 450, width = 1200)
spline_mod_plot(samples, OCD_mod)
dev.off()

png(paste0(path, 'figs/pairplot.png'), height = 1000, width = 1000)
pair_plot(na.omit(model_df[, c('twi', 'mvbf', 'annprecip', 'aridity', 'meananntemp', 'ndvi', 'natveg_prop', 'catchment_area', 'water_obs')]))
dev.off()

png(paste0(path, 'figs/density_mod.png'))
plot(OCD_mod, pages = 1)
dev.off()

png(paste0(path, 'figs/stock_mod_variables.png'), height = 1000, width = 1200)
stock_mod_plot(model_df, stock_mod_nore)
dev.off()
