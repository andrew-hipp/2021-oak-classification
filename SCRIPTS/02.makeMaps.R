## make subsection maps
library(magrittr)
library(maps)
library(openxlsx)
# get data used in 2018 New Phyt paper -- slow download!
# wrong data -- sub in cleaned data

if(!exists('dat.specimen'))
  dat.specimen <- read.csv('https://raw.githubusercontent.com/andrew-hipp/oak-convergence-2017/master/data/tables/all.eco.data.exportedFromR.2016-02-03.csv', as.is = T)
dat.spClass <- read.xlsx('../DATA/spClassification.xlsx', 1)

mapEm <- function(x, outName = NA,
                  dat = dat.specimen, col = "Species",
                  lat = 'latitude', lon = 'longitude',
                  basemap = 'usa', overplot = 'state',
                  base.col = 'black', base.lwd = 1,
                  overplot.col = 'white', overplot.lwd = 0.1,
                  pt.col = 'gray', pt.pch = 19) {
  use <- which(dat[[col]] %in% x)
  if(!is.na(outName)) pdf(outName)
  map(basemap, col = base.col, lwd = base.lwd)
  points(dat[use, c(lon, lat)],
         pch = pt.pch, col = pt.col)
  if(!is.na(overplot)) map(overplot, add = TRUE,
                            col = overplot.col, lwd = overplot.lwd)
  map(basemap, col = base.col, lwd = base.lwd, add = TRUE)
  if(!is.na(outName)) dev.off()
  return(ifelse(is.na(outName), 0, paste('saved', outName)))
}

# example -- loop over this
i = c('acerifolia', 'rubra', 'coccinea', 'shumardii') # example
mapEm(i)

# looping
for(i in dat.spClass$subsection %>% unique) {
  spp <- dat.spClass$sp[dat.spClass$subsection == i] %>% unique
  mapEm(spp, outName = paste('../OUT/', i, '.pdf', sep = ''))
}
