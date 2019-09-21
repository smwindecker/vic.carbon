aus_map <- function (file_location) {
  
  aus <- rgdal::readOGR(file_location, 'australia')
  e <- as(raster::extent(141, 150, -39, -33), "SpatialPolygons")
  sp::plot(aus, lwd = 4)
  sp::plot(e, add = TRUE, border = 'red', lwd = 7)

}

sites_map <- function (spatial_sample_data, file_location, thesis = TRUE) {
  
  aus_poly <- rgdal::readOGR(file_location, 'australia')
  vic_poly <- subset(aus_poly, NAME == 'Victoria')
  
  rivers <- rgdal::readOGR(file_location, 'flows_river_reaches')
  wetlands <- rgdal::readOGR(file_location, 'WETLAND_CURRENT_APR_2015')

  sp::plot(vic_poly, cex = 2)
  
  t_river <- t_col('steel blue')
  sp::plot(rivers, add = TRUE, col = t_river)
  
  t_wetland <- t_col('dark blue')
  sp::plot(wetlands, add = TRUE, col = t_wetland, border = NA)
  
  sp::plot(spatial_sample_data, add = TRUE, 
           col = 'red1', pch = 20, border = 'black')  
  if (isTRUE(thesis)) aus <- png::readPNG('chapters/Chapter2_carbon/figs/aus_map.png')
  if (!isTRUE(thesis)) aus <- png::readPNG('figs/aus_map.png')
  rasterImage(aus, 146.5, -36.5, 150, -33.5, cex = 4)
  
  raster::scalebar(150, xy = c(147.5, -39), type = 'bar', cex = 2)
  north.arrow(150, -38.7, .2, lab = 'N', lab.pos = 'above')

  legend(x = 141,
         y = -39,
         legend = c('Wetland site', 'Wetlands', 'Rivers'),
         pch = c(20, NA, NA),
         col = c('red1', NA, t_river),
         fill = c(NA, t_wetland, NA),
         lty = c(NA, NA, 1),
         lwd = 3,
         border = c(FALSE, FALSE, FALSE),
         bty = 'n', 
         ncol = 3, 
         cex = 2.2)
  
}

t_col <- function(color, percent = 50, name = NULL) {
  #	  color = color name
  #	percent = % transparency
  #	   name = an optional name for the color
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  ## Save the color
  invisible(t.col)
  
}


# scalebar and north.arrow functions from
# http://www.flutterbys.com.au/stats/tut/tut5.4.html
north.arrow <- function (x, y, h, lab = 'North', lab.pos = 'below') {
  
  polygon(c(x, x, x + h/2), 
          c(y - (1.5*h), y, 
            y - (1 + sqrt(3)/2) * h), 
          col = "black", border = NA)
  polygon(c(x, x + h/2, x, x - h/2), 
          c(y - (1.5*h), 
            y - (1 + sqrt(3)/2) * h, y, 
            y - (1 + sqrt(3)/2) * h))
  
  if (lab.pos == 'below') text(x, y - (2*h), lab, adj = c(0.5, 0), cex = 2)
  else text(x, y + (0.25*h), lab, adj = c(0.5, 0), cex = 2)

}

