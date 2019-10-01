
get_pred_df <- function (depth, core, model) {
  
  df_core <- data.frame(core = core, 
                        SITE_CODE = substr(core, 1, 4), 
                        depth = seq(0, depth, length.out = 100))
  pred_core <- predict.gam(model, df_core, se.fit = TRUE)
  pred_core <- cbind(df_core, 
                     response = pred_core$fit, 
                     lwr = pred_core$fit-2*pred_core$se.fit, 
                     upr = pred_core$fit+2*pred_core$se.fit)
  pred_core
}

method_subplot <- function (df, mod) {
  
  subset_dere2 <- df[df$core == 'DERE2', ]
  plot_max_depth <- max(subset_dere2$depth)
  
  pred_dere2 <- get_pred_df(plot_max_depth, 'DERE2', mod)
  
  par(mar = c(2, 2, 1, 1))
  plot(subset_dere2$depth, subset_dere2$mgOC_cm3, col = 'red', pch = 20, 
       cex = 2, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
       cex.lab = 2, bty = 'n', xaxs = 'i', yaxs = 'i')
  lines(pred_dere2$depth, exp(pred_dere2$response), col = 'red', lwd = 4)

  polygon(x = c(pred_dere2$depth, rev(pred_dere2$depth)),
          y = c(rep(0, nrow(pred_dere2)), rev(exp(pred_dere2$response))),
          col = adjustcolor('red', alpha.f = 0.40), 
          border = NA)
}

covariate_plot <- function (covariate, file_location = '') {
  
  r <- aggregate(shapefile(paste0(file_location, 
                                  'shapefiles/processed/cma.shp')), 
                 dissolve = TRUE) %>%
    as("SpatialPolygonsDataFrame")
  cov <- raster(x = paste0(fil_location, 
                           'shapefiles/processed/', 
                           covariate, '.tif')) %>%
    mask(r)
  
  plot(cov, axes = FALSE, box = FALSE, legend = FALSE)  

}

bp <- function (variable) {
  par(mfrow = c(3,3))
  boxplot(model_df$ndvi ~ model_df[, variable], ylab = 'ndvi')
  boxplot(model_df$annprecip ~ model_df[, variable], ylab = 'prec')
  boxplot(model_df$mvbf ~ model_df[, variable], ylab = 'mvbf')
  boxplot(model_df$twi ~ model_df[, variable], ylab = 'twi')
  boxplot(model_df$stock_Mg_h ~ model_df[, variable], ylab = 'temp')
  boxplot(model_df$stock_Mg_h ~ model_df[, variable], ylab = 'water_obs')
  boxplot(model_df$stock_Mg_h ~ model_df[, variable], ylab = 'natveg_prop')
  boxplot(model_df$stock_Mg_h ~ model_df[, variable], ylab = 'catchment_area')
  boxplot(model_df$stock_Mg_h ~ model_df[, variable], ylab = 'aridity')
}

# Produce pair plot of traits
pair_plot <- function (df) {
  
  panel.cor <- function (x, y, digits = 2, prefix = "", cex.cor = 1.8, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- stats::cor(x, y)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.7/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * abs(r))
    
    p <- stats::cor.test(x, y)$p.value
    if (p < 0.05) sym <- 8
    if (p < 0.01) sym <- c(8,8)
    if (p <0.001) sym <- c(8,8,8)
    if (p < 0.05) legend('topright', legend = '', pch = sym, bty = 'n')
  }
  
  # Customize upper panel
  upper.panel<-function(x, y){
    points(x, y, xlab = '', ylab = '', cex = 2.2)
    mylm <- lm(y ~ x)
    abline(mylm, col = 'red', cex = 2.2)
    newx <- seq(min(x), max(x), length.out = 500)
    prd <- predict(mylm, newdata = data.frame(x = newx), interval = c('confidence'),
                   level = 0.90, type = 'response')
    lines(newx, prd[, 2], col = 'black', lty = 2, cex = 2.2)
    lines(newx, prd[, 3], col = 'black', lty = 2, cex = 2.2)
    
  }
  
  # Create the plot
  par(cex.axis = 2.7)
  pairs(df,
        lower.panel = panel.cor,
        upper.panel = upper.panel, 
        cex.labels = 5)
  
}

s_depth <- function (df, model, subplot) {
  
  df <- data.frame(core = 'DERE1', 
                   SITE_CODE = 'DERE', 
                   depth = seq(0, max(df$depth), length.out = 100))
  pred <- predict.gam(model, df, terms = 's(depth)', se.fit = TRUE)
  
  pred_mean <- exp(pred$fit)
  pred_lwr <- exp(pred$fit - 2*pred$se.fit)
  pred_upr <- exp(pred$fit + 2*pred$se.fit)
  
  plot(df$depth, pred_mean, type = 'l', 
       bty = 'L', xaxs = 'i', yaxs = 'i', ylab = '', xlab = '', 
       lwd = 2, ylim = c(0, 170), xlim = c(0, 130),
       yaxt = 'n', xaxt = 'n')
  lines(df$depth, pred_lwr, lty = 4, lwd = 2)
  lines(df$depth, pred_upr, lty = 4, lwd = 2)
  polygon(x = c(df$depth, rev(df$depth)),
          y = c(pred_lwr, rev(pred_upr)),
          col = adjustcolor('black', alpha.f = 0.10), 
          border = NA)

  axis(side = 1, at = c(0, 65, 130), cex.axis = 2.5, 
       labels = c(0, 65, 130),
       padj = .4)
  
  axis(side = 2, at = c(0, 85, 170), las = 2, cex.axis = 2.5,
       labels = c(0, 85, 170))
  
  legend('topleft', legend = subplot, bty = 'n', cex = 2.5)
}


s_cores_depth <- function (df, mod, subplot) {
  
  dere <- df[df$SITE_CODE == 'DERE', ]
  dere1 <- df[df$core == 'DERE1', ]
  dere2 <- df[df$core == 'DERE2', ]
  
  pred_dere1 <- get_pred_df(max(dere$depth), 'DERE1', mod)
  pred_dere2 <- get_pred_df(max(dere$depth), 'DERE2', mod)
  
  plot(dere1$depth, dere1$mgOC_cm3, col = 'palevioletred', pch = 20,  
       bty = 'L', xaxs = 'i', yaxs = 'i', ylab = '', xlab = '', 
       ylim = c(0, 170), xlim = c(0, 130),
       yaxt = 'n', xaxt = 'n')
  points(dere2$depth, dere2$mgOC_cm3, col = 'red', pch = 20)
  
  lines(pred_dere1$depth, exp(pred_dere1$response), col = 'palevioletred', lwd = 2)
  lines(pred_dere1$depth, exp(pred_dere1$lwr), col = 'palevioletred', lty = 4, lwd = 2)
  lines(pred_dere1$depth, exp(pred_dere1$upr), col = 'palevioletred', lty = 4, lwd = 2)
  polygon(x = c(pred_dere1$depth, rev(pred_dere1$depth)),
          y = c(exp(pred_dere1$lwr), rev(exp(pred_dere1$upr))),
          col = adjustcolor('palevioletred', alpha.f = 0.10),
          border = NA)
  
  lines(pred_dere2$depth, exp(pred_dere2$response), col = 'red', lwd = 2)
  lines(pred_dere2$depth, exp(pred_dere2$lwr), col = 'red', lty = 4, lwd = 2)
  lines(pred_dere2$depth, exp(pred_dere2$upr), col = 'red', lty = 4, lwd = 2)
  polygon(x = c(pred_dere2$depth, rev(pred_dere2$depth)),
          y = c(exp(pred_dere2$lwr), rev(exp(pred_dere2$upr))),
          col = adjustcolor('red', alpha.f = 0.10),
          border = NA)
  
  axis(side = 1, at = c(0, 65, 130), cex.axis = 2.5, 
       labels = c(0, 65, 130),
       padj = .4)
  
  legend('topleft', legend = subplot, bty = 'n', cex = 2.5)
}

s_sites_depth <- function (df, mod, subplot) {
  
  dere <- df[df$SITE_CODE == 'DERE', ]
  dere2 <- df[df$core == 'DERE2', ]
  
  ewin <- df[df$SITE_CODE == 'EWIN', ]
  ewin3 <- df[df$core == 'EWIN3', ]
  
  pred_dere2 <- get_pred_df(max(dere$depth), 'DERE2', mod)
  pred_ewin3 <- get_pred_df(max(ewin$depth), 'EWIN3', mod)
  
  plot(dere2$depth, dere2$mgOC_cm3, col = 'red', pch = 20,  
       bty = 'L', xaxs = 'i', yaxs = 'i', ylab = '', xlab = '', 
       ylim = c(0, 170), xlim = c(0, 130),
       yaxt = 'n', xaxt = 'n')
  points(ewin3$depth, ewin3$mgOC_cm3, col = 'blue', pch = 20)
  
  lines(pred_dere2$depth, exp(pred_dere2$response), col = 'red', lwd = 2)
  lines(pred_dere2$depth, exp(pred_dere2$lwr), col = 'red', lty = 4, lwd = 2)
  lines(pred_dere2$depth, exp(pred_dere2$upr), col = 'red', lty = 4, lwd = 2)
  polygon(x = c(pred_dere2$depth, rev(pred_dere2$depth)),
          y = c(exp(pred_dere2$lwr), rev(exp(pred_dere2$upr))),
          col = adjustcolor('red', alpha.f = 0.10),
          border = NA)
  
  lines(pred_ewin3$depth, exp(pred_ewin3$response), col = 'blue', lwd = 2)
  lines(pred_ewin3$depth, exp(pred_ewin3$lwr), col = 'blue', lty = 4, lwd = 2)
  lines(pred_ewin3$depth, exp(pred_ewin3$upr), col = 'blue', lty = 4, lwd = 2)
  polygon(x = c(pred_ewin3$depth, rev(pred_ewin3$depth)),
          y = c(exp(pred_ewin3$lwr), rev(exp(pred_ewin3$upr))),
          col = adjustcolor('blue', alpha.f = 0.10),
          border = NA)
  
  axis(side = 1, at = c(0, 65, 130), cex.axis = 2.5, 
       labels = c(0, 65, 130),
       padj = .4)
  
  legend('topleft', legend = subplot, bty = 'n', cex = 2.5)
}

spline_mod_plot <- function (df, mod) {
  
  par(mfrow = c(1,3), mar = c(4, 4, 1, 1), oma = c(5, 5, 2, 2))
  s_depth(df, mod, '(a)')
  s_cores_depth(df, mod, '(b)')
  s_sites_depth(df, mod, '(c)')
  mtext(text = 'Depth (cm)', 
        side = 1, 
        line = 0.6, 
        outer = TRUE,
        cex = 2.2)
  mtext(text = expression(paste('Organic carbon density (mg cm'^'-3', ')')), 
        side = 2,
        line = 0.7,
        outer = TRUE, 
        cex = 2.2)
  
}

variable_plot <- function (df, model, variable, subplot, ...) {
  
  variables <- c('annprecip', 'ndvi', 'twi', 'mvbf', 'water_obs', 'catchment_area', 'natveg_prop')
  remaining_vars <- variables[!variables %in% variable]
  
  sim_df <- data.frame(annprecip = 1:100,
                       ndvi = 1:100,
                       twi = 1:100,
                       mvbf = 1:100,
                       water_obs = 1:100,
                       catchment_area = 1:100,
                       natveg_prop = 1:100)
  
  for (i in remaining_vars) {
    sim_df[,i] <- rep(mean(df[,i]), 100)
  }
  
  # log scale catchemtn area
  sim_df[,variable] <- seq(min(df[,variable]), max(df[,variable]), length.out = 100)
  
  pred <- predict.gam(model, sim_df, se.fit = TRUE)
  
  if (variable != 'catchment_area') {
    pred_mean <- exp(pred$fit)
    pred_lwr <- exp(pred$fit - 2*pred$se.fit)
    pred_upr <- exp(pred$fit + 2*pred$se.fit)
    x <- sim_df[,variable]
  }
  
  # catchment area on log scale
  if (variable == 'catchment_area') {
    pred_mean <- pred$fit
    pred_lwr <- pred$fit - 2*pred$se.fit
    pred_upr <- pred$fit + 2*pred$se.fit
    x <- log(sim_df[,variable])
  }
  
  if (max(x) < 10) {
    xhigh <- mceiling(max(x), .1)
    digits <- "%.1f"
    xlow <- 0
  }
  if (max(x) < 1) {
    xhigh <- mceiling(max(x), .01)
    digits <- "%.2f"
    xlow <- 0
  }
  if (max(x) > 2) {
    xhigh <- mceiling(max(x), 1)
    digits <- "%.0f"
    xlow <- mceiling(min(x), 1)
  }
  
  yhigh <- mceiling(max(pred_upr) + .1*max(pred_upr), 1)
  
  plot(x, pred_mean, type = 'l',
       bty = 'L', xaxs = 'i', yaxs = 'i', ylab = '', lwd = 2, 
       ylim = c(0, yhigh), xlim = c(min(x), xhigh), 
       yaxt = 'n', xaxt = 'n', cex.lab = 2.5, ...)
  
  lines(x, pred_lwr, lty = 4, lwd = 2)
  lines(x, pred_upr,  lty = 4, lwd = 2)
  polygon(x = c(x, rev(x)),
          y = c(pred_lwr, rev(pred_upr)),
          col = adjustcolor('black', alpha.f = 0.10),
          border = NA)
  
  axis(side = 1, at = c(xlow, xhigh), cex.axis = 2.5, 
       labels = sprintf(digits, c(xlow, xhigh)),
       padj = .4)
  
  axis(side = 2, at = c(0, yhigh), las = 2, cex.axis = 2.5,
       labels = sprintf("%.0f", c(0, yhigh)))
  
  # text(0.5, 0.5, txt, cex = cex.cor * abs(r))
  
  # x1, x2 just repeat each data point
  # y1, y2 are the vertical start/stop locations
  y2 <- max(pred_upr)*.035
  
  if (variable != 'catchment_area') {
    for (i in df[,variable]) {
      lines(c(i, i), c(0, y2))
    }
  }
  
  if (variable == 'catchment_area') {
    for (i in log(df[,variable])) {
      lines(c(i, i), c(0, y2))
    }
  }
  
  legend('topleft', legend = subplot, bty = 'n', cex = 2.5)
}

stock_mod_plot <- function (df, model) {
  
  par(mfrow = c(3, 3), mar = c(7, 4, 1, 1), oma = c(2, 7, 2, 2))
  variable_plot(df, model, 'annprecip', '(a)', xlab = 'Annual precipitation (mm)')
  variable_plot(df, model, 'ndvi', '(b)', xlab = 'NDVI')
  variable_plot(df, model, 'twi', '(c)', xlab = 'TWI')
  variable_plot(df, model, 'mvbf', '(d)', xlab = 'MVBF')
  variable_plot(df, model, 'water_obs', '(e)', xlab = 'Water Obs. from Space (%)')
  variable_plot(df, model, 'natveg_prop', '(f)', xlab = 'Prop. of native vegetation')
  variable_plot(df, model, 'catchment_area', '(g)', xlab = expression(paste('log catchment area (m'^'2', ')')))
  mtext(text = expression(paste('Soil carbon stock (Mg h'^'-1', ')')), 
        side = 2,
        line = 2,
        outer = TRUE, 
        cex = 3, 
        adj = 0.55)
  
}

# create functions to specify how to round
mfloor <- function (x, base) { 
  base*floor(x/base) 
} 
mceiling <- function (x, base) { 
  base*ceiling(x/base) 
} 
mround <- function (x, base) { 
  base*round(x/base) 
} 

stock_mod_table <- function (mod) {
  
  sum.gam <- stock_mod_nore
  ptab <- as.data.frame(sum.gam$p.table)
  stab <- as.data.frame(sum.gam$s.table)
  colnames(ptab)[4] <- "p-value"
  colnames(ptab)[3] <- "t-value"
  ptab.cnames = colnames(ptab)
  stab.cnames = colnames(stab)
  stab.cnames[3] = "F-value"
  colnames(ptab) = c("A", "B", "C", "D")
  if (ncol(stab) != 0) {
    colnames(stab) = colnames(ptab)
  }
  tab = rbind(ptab, stab)
  colnames(tab) = ptab.cnames
  tab = round(tab, 4)
  m = data.frame(matrix(0, nrow(tab), ncol(tab)))
  for (i in 1:nrow(tab)) {
    for (j in 1:4) {
      if ((j == 4) & (tab[i, j] < 1e-04)) {
        m[i, j] = "< 0.0001"
      }
      else {
        m[i, j] = sprintf("%3.4f", tab[i, j])
      }
    }
  }
  colnames(m) = colnames(tab)
  rownames(m) = rownames(tab)
  tab = m
  tab2 = rbind(c(ptab.cnames), tab[1:nrow(ptab), ])
  if (nrow(stab) > 0) {
    tab2 = rbind(tab2, c(stab.cnames), tab[(nrow(ptab) + 
                                              1):nrow(tab), ])
  }
  if (nrow(stab)) {
    rownames(tab2)[(nrow(ptab) + 2)] = "B. smooth terms"
  }
  rownames(tab2)[1] = "A. parametric coefficients"
  for (i in 1:nrow(tab2)) {
    if (tab2[i, 4] == "0") 
      tab2[i, 4] = "< 0.0001"
    if (length(grep("\\.", tab2[i, 2])) == 0) 
      tab2[i, 2] = paste(tab2[i, 2], ".0000", sep = "")
  }
  
  
  # create xtable
  pca_loadings <- xtable::xtable(loadings)
  
  print(pca_loadings,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        hline.after = NULL,
        file = output_file)
}

gamtabs <- function (model, output_file, caption = " ", label = "tab.gam", pnames = NA, 
          snames = NA, ptab = NA, stab = NA, ...) 
{
  if (!requireNamespace("xtable", quietly = TRUE)) {
    stop("Package 'xtable' needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  sum.gam <- model
  if (!inherits(model, "summary.gam")) {
    sum.gam <- summary(model)
  }
  if (is.na(ptab[1])) {
    ptab = as.data.frame(sum.gam$p.table)
  }
  if (is.na(stab[1])) {
    stab = as.data.frame(sum.gam$s.table)
  }
  if (!is.na(pnames[1])) {
    rownames(ptab) = pnames
  }
  if (!is.na(snames[1])) {
    rownames(stab) = snames
  }
  colnames(ptab)[4] = "p-value"
  colnames(ptab)[3] = "t-value"
  ptab.cnames = colnames(ptab)
  stab.cnames = colnames(stab)
  stab.cnames[3] = "F-value"
  colnames(ptab) = c("A", "B", "C", "D")
  if (ncol(stab) != 0) {
    colnames(stab) = colnames(ptab)
  }
  tab = rbind(ptab, stab)
  colnames(tab) = ptab.cnames
  tab = round(tab, 4)
  m = data.frame(matrix(0, nrow(tab), ncol(tab)))
  for (i in 1:nrow(tab)) {
    for (j in 1:4) {
      if ((j == 4) & (tab[i, j] < 1e-04)) {
        m[i, j] = "< 0.0001"
      }
      else {
        m[i, j] = sprintf("%3.4f", tab[i, j])
      }
    }
  }
  colnames(m) = colnames(tab)
  rownames(m) = rownames(tab)
  tab = m
  tab2 = rbind(c(ptab.cnames), tab[1:nrow(ptab), ])
  if (nrow(stab) > 0) {
    tab2 = rbind(tab2, c(stab.cnames), tab[(nrow(ptab) + 
                                              1):nrow(tab), ])
  }
  if (nrow(stab)) {
    rownames(tab2)[(nrow(ptab) + 2)] = "B. smooth terms"
  }
  rownames(tab2)[1] = "A. parametric coefficients"
  for (i in 1:nrow(tab2)) {
    if (tab2[i, 4] == "0") 
      tab2[i, 4] = "< 0.0001"
    if (length(grep("\\.", tab2[i, 2])) == 0) 
      tab2[i, 2] = paste(tab2[i, 2], ".0000", sep = "")
  }
  
  table_tab2 <- xtable::xtable(tab2)
  
  print(table_tab2,
        include.rownames = TRUE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        hline.after = NULL,
        file = output_file)
  
  # print(xtable::xtable(tab2, caption = caption, label = label, 
  #                      align = "lrrrr"), include.colnames = FALSE, hline.after = c(0, 
  #                                                                                  (nrow(ptab) + 1), nrow(tab2)), ...)
}
