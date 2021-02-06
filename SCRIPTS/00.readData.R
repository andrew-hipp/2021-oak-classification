## read in tree and aux data

library(ape)
library(openxlsx)

tr <- read.tree('../DATA/tr.singletons.correlated.1.taxaGrepStem.tre')
tr <- drop.tip(tr, c("Quercus_arizonica|Mexico|Durango|2015003|QUE001258",
                     "Quercus_laeta|Mexico|Durango|2015040|QUE001294",
                     "Quercus_conzattii|Mexico|Oaxaca|JCB-MX-OA-TE-4|QUE000197"))

tr$tip.label <- sapply(strsplit(tr$tip.label, '|', fixed = T), '[', 1)
tip.dat <- read.xlsx('../DATA/includeTips.2018-10-30-v2.xlsx', 1)
tip.dat$sp <- sapply(strsplit(tip.dat$tip, '_|_', fixed = T), '[', 1)
tip.dat <- tip.dat[which(tip.dat$singleTip), ]

message(paste('Proportion tree tips matching tip.dat:', round(sum(tr$tip.label %in% tip.dat$sp) / length(tr$tip.label), 2)))

tr <- drop.tip(tr, which(!tr$tip.label %in% tip.dat$sp))
tip.dat <- tip.dat[tip.dat$sp %in% tr$tip.label, ]
tip.dat <- tip.dat[!duplicated(tip.dat$sp), ]
tip.dat <- tip.dat[grep('Quercus', tip.dat$subgenus), ]
tr <- drop.tip(tr, setdiff(tr$tip.label, tip.dat$sp))

pdf('../OUT/tr.checkingNames.pdf', 8.5,15)
plot(tr, cex = 0.5)
dev.off()
