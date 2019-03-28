

#dont worry about this first stuff- was just for ARCMAP
setwd("D:/Saras paper")
library(raster)
library(rgdal)
sites<- shapefile('spatial_sites.shp')
str(sites)
sitesT<-spTransform(sites,CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs "))
writeOGR(sitesT, ".", "sitesT", driver="ESRI Shapefile")

Sites_with_vast<-shapefile('Sites_with_Area_and_VAST.shp')
#str(Sites_with_vast)
Sites_with_vast@proj4string
#CRS arguments: +proj=longlat +ellps=GRS80 +no_defs 
sites@proj4string
# CRS arguments:
#   +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 

sites_with_vastT<-spTransform(Sites_with_vast,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0  "))
writeOGR(sites_with_vastT, ".", "Sites_with_vast_and_areaT", driver="ESRI Shapefile")


################################################################
#trying again, this time with R
setwd("D:/spatial data/Stratification/data")
sites<- shapefile('spatial_sites.shp')
catchments<-shapefile('catchments.shp')
VAST<-raster("D:/spatial data/Stratification/data/VAST/vastgridv2_1k.tif")
#change CRS of sites as catchments took too long to change 
sitesT<-spTransform(sites,CRS("+proj=longlat +ellps=GRS80 +no_defs "))
save(sitesT, file="sitesT.RData")

library(sp) 
catchments_crop<-crop(catchments, extent(sitesT))
save(catchments_crop, file="catchments_crop.RData")
catchments_with_sites <-over(catchments_crop,sitesT)  #to get OBJECTIDS (catchment ids) and site codes
catchments_with_sites3<-na.omit(catchments_with_sites) #now just the 100 sites
catchments_with_sites2<-intersect(catchments,sitesT) #the spatial files of the catchments that had sites in them 

#catchments_with_sites1<-catchments[which(catchments$OBJECTID==na.omit(catchments_with_sites)),]
#change cropped catchments CRS to match VAST
catchments_with_sites2T<-spTransform(catchments_with_sites2,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
vast_crop<-crop(VAST, extent(catchments_with_sites2T))

#matrix to reclassify raster with
MNative<- matrix(c(-Inf,3,1,
                   3, 6, 0), ncol=3, byrow=TRUE)
MBare<-matrix(c(-Inf,0,1,
              0, 6, 0), ncol=3, byrow=TRUE)
MResidual <- matrix(c(-Inf,0,0,
               0, 1, 1,
               1, 6, 0), ncol=3, byrow=TRUE)
MModified <- matrix(c(-Inf, 1, 0,
               1, 2, 1,
               2,6,0), ncol=3, byrow=TRUE)
MTransformed <- matrix(c(-Inf, 2, 0,
               2, 3, 1,
               3,6,0), ncol=3, byrow=TRUE)
MReplaced <- matrix(c(-Inf, 3, 0,
                 3, 5, 1,
                 5,6,0), ncol=3, byrow=TRUE)

MRemoved <- matrix(c(-Inf, 5, 0,
               5, 6, 1), ncol=3, byrow=TRUE)

vast_native <- reclassify(VAST, MNative)  #only the native veg, the inverse will be the non native veg 
vast_bare <- reclassify(VAST, MBare)
vast_residual<-reclassify(VAST, MResidual)
vast_modified<-reclassify(VAST, MModified)
vast_transformed<-reclassify(VAST, MTransformed) #last of the native veg classes 
vast_replaced<-reclassify(VAST, MReplaced)
vast_removed<-reclassify(VAST, MReplaced)



vast_native_catchment<-list()
#work out the proportion of the catchment polygon covered by the presences (1s) for each landuse
for (i in 1:nrow(catchments_with_sites2T)) { #this is the number of polygons to iterate through 
  single <- catchments_with_sites2T[i,] #selects a single polygon
  clip1 <- crop(vast_native, extent(single)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
 clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
  ext<-extract(vast_native,single) #extracts data from the raster based on the polygon bound
  s<-sum(na.omit(ext[[1]]))  #sums the table for percentage calculation
  vast_native_catchment[[i]]<-s/as.numeric(length(ext[[1]]))
}
save(vast_native_catchment, file="vast_native_catchment.RData")


vast_bare_catchment<-list()
#work out the proportion of the catchment polygon covered by the presences (1s) for each landuse
for (i in 1:nrow(catchments_with_sites2T)) { #this is the number of polygons to iterate through 
  single <- catchments_with_sites2T[i,] #selects a single polygon
  clip1 <- crop(vast_bare, extent(single)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
  clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
  ext<-extract(vast_bare,single) #extracts data from the raster based on the polygon bound
  s<-sum(na.omit(ext[[1]]))  #sums the table for percentage calculation
  vast_bare_catchment[[i]]<-s/as.numeric(length(ext[[1]]))
}
save(vast_bare_catchment, file="vast_bare_catchment.RData")

vast_residual_catchment<-list()
#work out the proportion of the catchment polygon covered by the presences (1s) for each landuse
for (i in 1:nrow(catchments_with_sites2T)) { #this is the number of polygons to iterate through 
  single <- catchments_with_sites2T[i,] #selects a single polygon
  clip1 <- crop(vast_residual, extent(single)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
  clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
  ext<-extract(vast_residual,single) #extracts data from the raster based on the polygon bound
  s<-sum(na.omit(ext[[1]]))  #sums the table for percentage calculation
  vast_residual_catchment[[i]]<-s/as.numeric(length(ext[[1]]))
}
save(vast_residual_catchment, file="vast_residual_catchment.RData")

vast_modified_catchment<-list()
#work out the proportion of the catchment polygon covered by the presences (1s) for each landuse
for (i in 1:nrow(catchments_with_sites2T)) { #this is the number of polygons to iterate through 
  single <- catchments_with_sites2T[i,] #selects a single polygon
  clip1 <- crop(vast_modified, extent(single)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
  clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
  ext<-extract(vast_modified,single) #extracts data from the raster based on the polygon bound
  s<-sum(na.omit(ext[[1]]))  #sums the table for percentage calculation
  vast_modified_catchment[[i]]<-s/as.numeric(length(ext[[1]]))
}
save(vast_modified_catchment, file="vast_modified_catchment.RData")

vast_transformed_catchment<-list()
#work out the proportion of the catchment polygon covered by the presences (1s) for each landuse
for (i in 1:nrow(catchments_with_sites2T)) { #this is the number of polygons to iterate through 
  single <- catchments_with_sites2T[i,] #selects a single polygon
  clip1 <- crop(vast_transformed, extent(single)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
  clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
  ext<-extract(vast_transformed,single) #extracts data from the raster based on the polygon bound
  s<-sum(na.omit(ext[[1]]))  #sums the table for percentage calculation
  vast_transformed_catchment[[i]]<-s/as.numeric(length(ext[[1]]))
}
save(vast_transformed_catchment, file="vast_transformed_catchment.RData")

vast_replaced_catchment<-list()
#work out the proportion of the catchment polygon covered by the presences (1s) for each landuse
for (i in 1:nrow(catchments_with_sites2T)) { #this is the number of polygons to iterate through 
  single <- catchments_with_sites2T[i,] #selects a single polygon
  clip1 <- crop(vast_replaced, extent(single)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
  clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
  ext<-extract(vast_replaced,single) #extracts data from the raster based on the polygon bound
  s<-sum(na.omit(ext[[1]]))  #sums the table for percentage calculation
  vast_replaced_catchment[[i]]<-s/as.numeric(length(ext[[1]]))
}
save(vast_replaced_catchment, file="vast_replaced_catchment.RData")

vast_removed_catchment<-list()
#work out the proportion of the catchment polygon covered by the presences (1s) for each landuse
for (i in 1:nrow(catchments_with_sites2T)) { #this is the number of polygons to iterate through 
  single <- catchments_with_sites2T[i,] #selects a single polygon
  clip1 <- crop(vast_removed, extent(single)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
  clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
  ext<-extract(vast_removed,single) #extracts data from the raster based on the polygon bound
  s<-sum(na.omit(ext[[1]]))  #sums the table for percentage calculation
  vast_removed_catchment[[i]]<-s/as.numeric(length(ext[[1]]))
}
save(vast_removed_catchment, file="vast_removed_catchment.RData")

#adding the proportion data to the spatial catchments 
catchments_with_sites2T@data$NatVegProp<-unlist(vast_native_catchment)
catchments_with_sites2T@data$BareProp<-unlist(vast_bare_catchment)
catchments_with_sites2T@data$ResidProp<-unlist(vast_residual_catchment)
catchments_with_sites2T@data$ModProp<-unlist(vast_modified_catchment)
catchments_with_sites2T@data$TransformedProp<-unlist(vast_transformed_catchment)
catchments_with_sites2T@data$ReplacedProp<-unlist(vast_replaced_catchment)
catchments_with_sites2T@data$removedProp<-unlist(vast_removed_catchment)


#catchments_with_sites3 is from up at the top, where it is just the object ids and the site codes
catchments_with_sites3$objectID<-row.names(catchments_with_sites3)
catchments_with_sites3$objectID<-as.numeric(catchments_with_sites3$objectID)
catchments_with_sites3$objectID<-catchments_with_sites3$objectID+1  #I have no idea why they were 1 off 
sites_with_codes<-merge(sitesT,catchments_with_sites3, by.all="SITE_CODE")  #now merge to get all 100
#2 without objectIDs  POWL and WOOL as there were 2 points in a catchment 
#POWL in same as CARP
#WOOL  in same as king
#POWL
sites_with_codes@data$objectID[77]<-327460
#WOOL
sites_with_codes@data$objectID[97]<-325740
#now merge dataframes to add the site code information to the catchment information
sites_with_catchments<-merge(sites_with_codes@data,catchments_with_sites2T@data, by.x="objectID", by.y="OBJECTID",all.x = TRUE , all.y = TRUE)

#manually change catchment which does not overlap with the VAST layer so it thinks its in the bay- assign to closest value (replaced)
sites_with_catchments$ReplacedProp[89]<-1.00

write.csv(sites_with_catchments, file = "sites_with_catchment_data.csv")
