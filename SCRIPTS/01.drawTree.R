require(ggtree)
require(ggplot2)
require(dplyr)

troubleshoot = F

lwdLabel = c(clade = 1, subsection = 1, section = 1)
cexLabel = c(clade = 1, subsection = 2, section = 2)
colLab <- c(Lobatae = 'red', Quercus = 'black',
            Protobalanus = 'blue', Ponticae = 'blue', Virentes = 'blue',
            ## now red oak subsections
            Agrifoliae = 'red',
            Palustres = 'red1',
            Laurifoliae = 'red2',
            Coccineae = 'red3',
            ## ... and white oak subsections
            Roburoids = 'black',
            Dumosae = 'gray10',
            Albae = 'gray20',
            Prinoids = 'gray30',
            Stellatae = 'gray40',
            ### ... and informal clades
            'TX red oaks' = 'red4',
            Erythromexicana = 'orange4',
            'Tx white oaks' = 'gray50',
            Leucomexicana = 'gray60',

          )

offsetLabel = c(subsection = 20, section = 25)
offsetTemp <- min(offsetLabel)
barExtend = -0.2

## make base tree
tr.plot <- full_join(tr, tip.dat, by = 'node')
p <- ggtree(tr.plot, layout = 'fan', open.angle = 180, size = 0.01)
p <- p + geom_tiplab2(fontface='italic',
                      size = 1.2,
                      aes(color = NAm)
                    )
p <- p + theme(legend.position='none')
p <- p + scale_color_manual(values=c("gray", "black"))

## add clade labels
for(i in c('subsection', 'section')) {
  for(j in unique(tip.dat[[i]])) {
    if(is.na(j)) next
    message(paste('doing', i, j))
    if(troubleshoot) {
      offsetTemp <- offsetTemp + 3
    } else offsetTemp <- offsetLabel[i]
    mrcaNode <- getMRCA(tr, row.names(tip.dat)[which(tip.dat[[i]] == j)])
    p <- p + geom_cladelabel(node = mrcaNode,
                             offset = offsetTemp,
                             fontsize = cexLabel[i],
                             barsize = lwdLabel[i],
                             label = j, color = colLab[j],
                             extend = barExtend)
  }
}



pdf('../OUT/prettyTree.pdf', 12, 8)

print(p)
dev.off()
