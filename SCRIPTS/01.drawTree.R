require(ggtree)
require(ggplot2)
require(dplyr)

lwdLabel = c(clade = 1, section = 2, subgenus = 2)
cexLabel = c(clade = 1, section = 2, subgenus = 2)
offsetLabel = c(clade = 10, section = 15, subgenus = 20)

## make base tree
tr.plot <- full_join(tr, tip.dat, by = 'node')
p <- ggtree(tr.plot, layout = 'fan', open.angle = 180)
p <- p + geom_tiplab2(fontface='italic',
                      size = 1.7,
                      aes(color = NAm)
                    )
p <- p + theme(legend.position='none')
p <- p + scale_color_manual(values=c("gray", "black"))

## add subsections
for(i in c('clade', 'section', 'subgenus')) {
  for(j in unique(tip.dat$clade)) {
    if(is.na(j)) next
    mrcaNode <- getMRCA(tr.plot, tip.dat[which(tip.dat[[i]] == j)])
    p <- p + geom_cladelabel(node = mrcaNode[i], offset = offsetLabel[i], cex = cexLabel[i])
  }
}

## add sections


## add subgenera


pdf('../OUT/prettyTree.pdf', 8.5, 11)

print(p)
dev.off()
