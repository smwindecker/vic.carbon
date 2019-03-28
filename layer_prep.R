# layer prep
library(gdaltools)

crs.geo <- '+proj=longlat +datum=WGS84 +no_defs'

path <- 'chapters/Chapter5_carbon/'
abs_path_raw <- paste0(getwd(), '/', path, 'shapefiles/raw/')
abs_path_output <- paste0(getwd(), '/', path, 'shapefiles/processed/')

rel_path_raw <- paste0(path, 'shapefiles/raw/')
rel_path_output <- paste0(path, 'shapefiles/processed/')

r_res <- .0025
r_ext <- c(141, 150, -39, -33)

reproj_shp(paste0(abs_path_raw, "cma100.shp"),
           paste0(abs_path_output, "cma.shp"),
           crs = crs.geo)

# ndvi
reproj_ras(paste0(abs_path_raw, "bom_ndvi.grid"),
           paste0(abs_path_output, "ndvi.tif"),
           crs = crs.geo,
           res = r_res,
           ext = r_ext)

# mean annual temperature
reproj_ras(paste0(abs_path_raw, "bc01_curr.tif"), 
           paste0(abs_path_output, "meananntemp.tif"),
           crs = crs.geo,
           res = r_res,
           ext = r_ext)

# mean annual precipitation
reproj_ras(paste0(abs_path_raw, "bc12_curr.tif"), 
           paste0(abs_path_output, "annprecip.tif"),
           crs = crs.geo,
           res = r_res,
           ext = r_ext)

# twi median
reproj_ras(paste0(abs_path_raw, "twmd_flt.tif"), 
           paste0(abs_path_output, "twi.tif"),
           crs = crs.geo,
           res = r_res,
           ext = r_ext)

# mean valley bottom flatness
reproj_ras(paste0(abs_path_raw, "mvbf_flt.tif"), 
           paste0(abs_path_output, "mvbf.tif"),
           crs = crs.geo,
           res = r_res,
           ext = r_ext)

# wetlands spdf
reproj_shp(paste0(abs_path_raw, "WETLAND_CURRENT_APR_2015.shp"),
           paste0(abs_path_output, "wetland_current.shp"),
           crs = crs.geo)

# catchments
reproj_shp(paste0(abs_path_raw, "catchments.shp"),
           paste0(abs_path_output, "catchments.shp"),
           crs = crs.geo)

catchments <- shapefile(paste0(rel_path_raw, 'catchments.shp'))

