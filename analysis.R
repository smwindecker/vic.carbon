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
OCD_mod_simple <- ocd_gam(samples, simple = TRUE)

var_core <- variance_component(OCD_mod, 1)
var_site <- variance_component(OCD_mod, 2)

# Soil stock GAM
# Prepare site data

sites_only <- samples[!duplicated(samples$SITE_CODE), c('SITE_CODE', 'LAT', 'LON')]
spatial_sites <- make_samples_spatial(sites_only, '+proj=longlat +datum=WGS84 +no_defs')

# calculate stock
stock_data_site <- calculate_stock(samples, OCD_mod, 'site')

# collate spatial data 
stock <- combine_other_spatial(stock_data_site, 
                               ala_file = 'data-raw/ALA_layers.csv',
                               catchments_file = 'data-raw/sites_with_catchment_data.csv')

spatial_stock <- make_samples_spatial(stock, '+proj=longlat +datum=WGS84 +no_defs')

# prepare model matrix
model_df <- create_model_df(spatial_stock)
stock_covs <- covs_only(model_df)

# stock GAM
stock_mod <- stock_gam(model_df, re = TRUE)
stock_mod_nore <- stock_gam(model_df, re = FALSE)

cv_mod <- cv(model_df)

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
          file_location = 'shapefiles/raw', 
          thesis = FALSE)
dev.off()

png('img/methods_subplot.png', 
    height = 400, width = 500, bg = 'transparent')
method_subplot(samples, OCD_mod)
dev.off()

png('figs/s_sites_depth.png', height = 450, width = 1200)
spline_mod_plot(samples, OCD_mod)
dev.off()

png('figs/pairplot.png', height = 1000, width = 1000)
pair_plot(na.omit(model_df[, c('twi', 'mvbf', 'annprecip', 'aridity', 'meananntemp', 'ndvi', 'natveg_prop', 'catchment_area', 'water_obs')]))
dev.off()

png('figs/density_mod.png')
plot(OCD_mod, pages = 1)
dev.off()

png('figs/stock_mod_variables.png', height = 1000, width = 1200)
stock_mod_plot(model_df, stock_mod_nore)
dev.off()

# knit article

knitr::knit("ms/manuscript.Rnw", output = "ms/manuscript.tex")
tinytex::pdflatex("ms/manuscript.tex", pdf_file = 'ms/manuscript.pdf')

