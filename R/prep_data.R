
process_lab_results <- function (lab_data) {
  
  # load lab results
  lab <- read.csv(lab_data, header = T)
  lab$SAMPLE_ID <- sub('VW', '', as.character(lab$LAB_SAMPLE_ID))
  lab$SAMPLE_ID <- as.numeric(lab$SAMPLE_ID)
  lab$OC <- as.numeric(lab$OC)
  lab
  
}

prepare_master_data <- function (site_data, sample_data, wetland_types, lab_data) {
  
  # creating masterlist of sites/samples
  # BRADSO and Puzz Phrag are extra cores taken within BRAD and PUZZ sites. 
  # LC, PIC, ACI, LON, LMOM, and EWE did not have compaction measurements
  
  sites <- read.csv(site_data, header = T) %>%
    dplyr::filter(STATE == 'VIC' &
                    !CORE_LABEL %in% c('BARA', 'PUZZ Phrag', 'LC', 
                                       'PIC', 'ACI', 'LON', 'LMOM', 'EWE'))
  samples <- read.csv(sample_data, header = T) %>%
    dplyr::filter(NOTES2 != 'ESTIMATED COMPACTION' & 
                    NOTES2 != 'NO LAB DATA' &
                    !CORE_LABEL %in% c('BARA', 'PUZZ Phrag', 'LC', 
                                       'PIC', 'ACI', 'LON', 'LMOM', 'EWE'))
  
  masterlist <- merge(sites, samples, 
                      by = c('CORE_LABEL', 'CORE_NO'))
  
  carbon <- merge(masterlist, lab_data, by = 'SAMPLE_ID', all.x = T)
  
  # sample mid-depth point
  carbon$MID_SAMPLE_DEPTH <- rowMeans(carbon[c('LOWER_SAMPLE_DEPTH', 'UPPER_SAMPLE_DEPTH')],
                           na.rm = TRUE)
  
  # compaction factor
  carbon$COMPACTION_FACTOR <- (carbon$CORE_LENGTH - carbon$HEIGHT_IN) / 
    (carbon$CORE_LENGTH - carbon$HEIGHT_OUT)
  
  # compaction corrected sample depth
  carbon$depth <- carbon$MID_SAMPLE_DEPTH * 
    (1 + (1 - carbon$COMPACTION_FACTOR))

  # dry bulk density g/cm3
  carbon$DBD <- (carbon$SAMPLE_CONTAINER_DRYWEIGHT - 
                   carbon$CONTAINER_WEIGHT) / (pi*2.5^2*2)
  
  carbon$mgOC_cm3 <- carbon$DBD * carbon$OC
  
  carbon$LAT[carbon$SITE_CODE == 'NEWH'] <- -38.51353
  carbon$LON[carbon$SITE_CODE == 'NEWH'] <- 145.35509
  
  types <- read.csv(wetland_types, header = T)[, c('SITE_CODE', 'C_TYPE')]
  carbon_types <- merge(carbon, types, by = 'SITE_CODE')
  
  carbon_types$core <- as.factor(paste0(carbon_types$SITE_CODE, carbon_types$CORE_NO))
  
  # HOWS 2 only had one sample
  final <- carbon_types[carbon_types$core != 'HOWS2',]
  
  final
}

make_samples_spatial <- function (loaded_samples, crs) {
  
  sp::coordinates(loaded_samples) <- ~ LON + LAT
  crs(loaded_samples) <- crs
  
  spatial_samples <- sp::spTransform(loaded_samples, crs)
  spatial_samples

}

combine_other_spatial <- function (stock_df, ala_file, catchments_file) {
  
  ala <- unique(read.csv(ala_file)[, c('Latitude...original', 
                                       'Longitude...original', 
                                       'Aridity.index...annual.mean', 
                                       'Water.Observations.From.Space')])
  colnames(ala) <- c('LAT', 'LON', 'aridity', 'water_obs')
  stock_ala <- merge(ala, stock_df, by = c('LAT', 'LON'))
  
  catchments <- read.csv(catchments_file)[, c('SITE_CODE', 'AlbersArea', 'NatVegProp')]
  colnames(catchments) <- c('SITE_CODE', 'catchment_area', 'natveg_prop')
  stock <- merge(stock_ala, catchments, by = 'SITE_CODE')
  
  return(stock)
}

extract_sites <- function (df) {
  df[!duplicated(df$SITE_CODE), c('SITE_CODE', 'LAT', 'LON')]
}