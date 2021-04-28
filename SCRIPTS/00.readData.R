## read in tree and aux data

library(ape)
library(openxlsx)
library(magrittr)
library(tidytree)

if(!exists('weldTaxa')) {
  source('https://raw.githubusercontent.com/andrew-hipp/morton/master/R/weldTaxa.R')}

tr.gambeliiList <- data.frame(spliceTaxa = c('Quercus_lobata',
                                              'Quercus_arizonica',
                                              'Quercus_sinuata',
                                              'Quercus_marilandica',
                                              'Quercus_garryana'
                                            ),
                              spliceRule = rep('sister',5),
                              seqOrSplice = rep('** SPLICE **',5),
                              row.names = c('Quercus_gambelii',
                                            'Quercus_depressipes',
                                            'Quercus_sinuata_var._breviloba',
                                            'Quercus_marilandica_var._ashei',
                                            'Quercus_garryana_var._breweri'
                                          )
                            ) # close data.frame

## tip.dat.raw = original tip data from Hipp et al. 2020, New Phyt
## tip.dat = edited from this tree, used for downstream analyses

tr <- read.tree('../DATA/tr.singletons.correlated.1.taxaGrepStem.tre')
writeLines(tr$tip.label, '../OUT/allTips.orig.txt')
tr <- drop.tip(tr, c("Quercus_arizonica|Mexico|Durango|2015003|QUE001258",
                     "Quercus_laeta|Mexico|Durango|2015040|QUE001294",
                     "Quercus_conzattii|Mexico|Oaxaca|JCB-MX-OA-TE-4|QUE000197"))

tr$tip.label <- sapply(strsplit(tr$tip.label, '|', fixed = T), '[', 1)
tip.dat.raw <- read.xlsx('../DATA/includeTips.2018-10-30-v2.xlsx', 1)
tip.dat.raw$sp <- sapply(strsplit(tip.dat.raw$tip, '_|_', fixed = T), '[', 1)
tip.dat.raw <- tip.dat.raw[which(tip.dat.raw$singleTip), ]

message(paste('Proportion tree tips matching tip.dat.raw:', round(sum(tr$tip.label %in% tip.dat.raw$sp) / length(tr$tip.label), 2)))

tr <- drop.tip(tr, which(!tr$tip.label %in% tip.dat.raw$sp))
tip.dat.raw <- tip.dat.raw[tip.dat.raw$sp %in% tr$tip.label, ]
tip.dat.raw <- tip.dat.raw[!duplicated(tip.dat.raw$sp), ]
tip.dat.raw <- tip.dat.raw[grep('Quercus', tip.dat.raw$subgenus), ]
tr <- drop.tip(tr, setdiff(tr$tip.label, tip.dat.raw$sp))

tr <- weldTaxa(tr = tr, taxa = tr.gambeliiList)

tr$tip.label <- gsub('_', ' ', tr$tip.label, fixed = T)

tip.dat <- read.csv('../DATA/tips.data.csv', row.names = 8, as.is = TRUE)
row.names(tip.dat) <- gsub('_', ' ', row.names(tip.dat), fixed = T)
tip.dat$node <- nodeid(as_tibble(tr), row.names(tip.dat))

pdf('../OUT/tr.checkingNames.pdf', 8.5,15)
plot(tr, cex = 0.5, tip.color = ifelse(tip.dat[tr$tip.label, 'NAm'], 'black', 'gray'))
dev.off()

writeLines(tr$tip.label, '../OUT/allTips.final.txt')
