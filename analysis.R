## Analysis file

library(raster)
library(magrittr)
library(mgcv)

## Load R scripts

R.utils::sourceDirectory('R/')

# Process raw spatial data
## Processed spatial data files are provided in spatial/processed
## library(gdaltools)
## process_spatial_data()

# Prepare data

lab_data <- process_lab_results(lab_data = "data-raw/VWC_Lab_Results.csv")

samples <- prepare_master_data(site_data = "data-raw/VWC_SitesCores_Masterlist.csv",
                               sample_data = "data-raw/VWC_Samples_Masterlist.csv",
                               wetland_types = "data-raw/PC_carbon.csv",
                               lab_data = lab_data)

# Oranic carbon density GAM

OCD_mod <- ocd_gam(samples, simple = FALSE)

var_core <- variance_component(OCD_mod, 1)
var_site <- variance_component(OCD_mod, 2)

OCD_mod_simple <- ocd_gam(samples, simple = TRUE)

# hist(resid(OCD_mod))
# plot(resid(OCD_mod) ~ fitted(OCD_mod))
# gam.check(OCD_mod)
# plot(OCD_mod, pages = 1, residuals = TRUE)

# Soil stock GAM

# Prepare site data

sites_only <- samples[!duplicated(samples$SITE_CODE), c('SITE_CODE', 'LAT', 'LON')]
spatial_sites <- make_samples_spatial(sites_only, '+proj=longlat +datum=WGS84 +no_defs')

# calculate stock
stock_data_site <- calculate_stock(samples, OCD_mod, 'site')

# collate spatial data 
stock <- combine_other_spatial(stock_data_site, 
                               ala_file = 'chapters/Chapter2_carbon/data-raw/ALA_layers.csv',
                               catchments_file = 'chapters/Chapter2_carbon/data-raw/sites_with_catchment_data.csv')

spatial_stock <- make_samples_spatial(stock, '+proj=longlat +datum=WGS84 +no_defs')

# prepare model matrix
model_df <- create_model_df(spatial_stock)
stock_covs <- covs_only(model_df)

# stock GAM
stock_mod <- stock_gam(model_df, re = TRUE)
stock_mod_nore <- stock_gam(model_df, re = FALSE)

# hist(resid(stock_mod))
# plot(resid(stock_mod) ~ fitted(stock_mod))
# gam.check(stock_mod)
# plot(stock_mod, pages = 1, residuals = TRUE)

cv_mod <- cv(model_df)
cv_mod <- readRDS(paste0(path, 'cv_mod.rds'))

## Make figures

png('img/ndvi.png', bg = 'transparent')
covariate_plot('ndvi')
dev.off()

png('img/precip.png', bg = 'transparent')
covariate_plot('annprecip')
dev.off()

png('figs/wetland_type.png')
bp('C_TYPE')
dev.off()

png('figs/aus_map.png', height = 800, width = 1000, bg = 'transparent')
aus_map(file_location = 'shapefiles/raw')
dev.off()

png('figs/site_map.png', height = 800, width = 1100)
sites_map(spatial_sites, 
          file_location = 'shapefiles/raw')
dev.off()

png(paste0(path, 'img/methods_subplot.png'), 
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

# knit article

knitr::knit("ms/manuscript.Rnw", output = "ms/manuscript.tex")
tinytex::pdflatex("ms/manuscript.tex")

