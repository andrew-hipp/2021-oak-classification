require(ggtree)
require(ggplot2)

tr.plot <- tr
tr.plot$tip.label <- gsub('_', ' ', tr.plot$tip.label, fixed = T)
p <- ggtree(tr.plot, layout = 'fan', open.angle = 180)
p <- p + geom_tiplab2(fontface='italic',
                      size = 1.7
                      # ,
                      # color = ifelse(tip.dat[tr$tip.label, 'NAm'], "black", "gray")
                      )

pdf('../OUT/prettyTree.pdf', 8.5, 11)
layout(matrix(4:1,4,1))
# print(type = 'n')
# print(type = 'n')
# print(type = 'n')
print(p)
dev.off()
