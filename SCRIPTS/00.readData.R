## read in tree and aux data

library(ape)
library(openxlsx)
library(magrittr)

## tip.dat.raw = original tip data from Hipp et al. 2020, New Phyt
## tip.dat = edited from this tree, used for downstream analyses

tr <- read.tree('../DATA/tr.singletons.correlated.1.taxaGrepStem.tre')
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

pdf('../OUT/tr.checkingNames.pdf', 8.5,15)
plot(tr, cex = 0.5)
dev.off()

tip.dat <- read.csv('../DATA/tips.data.csv', row.names = 7, as.is = TRUE)
