library(magrittr)
library(maps)

if(!exists('dat.coccineae')) {
  dat.coccineae <- lapply(dir('../DATA/newCoccineae', full = T), read.csv)
  dat.coccineae <- do.call('rbind', dat.coccineae)
}

mapEm(x = 'all', dat = dat.coccineae,
      lat = 'decimalLatitude', lon = 'decimalLongitude')
