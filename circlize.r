#Migracion de toda la vida
rm(list=ls())
setwd("C:\\Users\\Alvaro\\Desktop\\rfiles")
mt<-read.table("reporte.csv",sep=";",header=T)
library(circlize)
mt$depto<-as.factor(mt$depto)

pdf("mt.pdf",width=20,height=20)
h<-prop.table(margin.table(as.matrix(mt[,2:10]),1))
circos.clear()
par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)
circos.par("default.track.height" = 0.1)
circos.initialize(factors = mt$depto,x=mt$LP,xlim=c(0,2))
circos.trackPlotRegion(factors = mt$depto, y = mt$CH,ylim=c(0,1),bg.border=NA
,bg.col = rainbow(9), track.height = 0.05)
circos.trackText(mt$depto,rep(1,9),rep(3,9),mt$depto)
circos.trackText(mt$depto,rep(0.5,9),rep(0.5,9),rep("Salen",9))
circos.trackText(mt$depto,rep(1.5,9),rep(0.5,9),rep("Entran",9))
for(i in 1:9){
circos.lines(rep(c(1,1),9),rep(c(0,1),9),mt$depto[i],col="white",lwd=4)
}
r<-prop.table(as.matrix(mt[,2:10]),1)
c<-prop.table(as.matrix(mt[,2:10]),2)
for(i in 1:9){
  for(j in 1:9){
    circos.link(mt$depto[i], c(sum(r[i,0:(j-1)]),sum(r[i,0:j])), col=rainbow(9)[i],mt$depto[j], c(1+sum(c[0:(i-1),j]),1+sum(c[0:i,j])), h =h[i]*2.5)
  }
}
dev.off()

circos.clear()
par(mar = c(1, 1, 1, 1))
circos.par(cell.padding = c(0, 0, 0, 0),
              gap.degree = c(2, 2, 10, 2, 2, 2, 2, 2, 10), start.degree = 5)
circos.initialize(factors = factors, xlim = xlim)
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA,
                          bg.col = c("red", "green", "blue", rep("grey", 6)), track.height = 0.05,
panel.fun = function(x, y) {
sector.name = get.cell.meta.data("sector.index")
          xlim = get.cell.meta.data("xlim")
          circos.text(mean(xlim), 1.5, sector.name, adj = c(0.5, 0))
                       1
                           
                              # plot white border in the grids
                              if(sector.name %in% rn) {
                                for(i in seq_len(ncol(mat))) {
                                  circos.lines(rep(sum(mat[sector.name, seq_len(i)]), 2), c(0, 1),
                                                + col = "white")
                                  }
                                } else if(sector.name %in% cn) {
                                  for(i in rev(seq_len(nrow(mat)))) {
                                    circos.lines(rep(sum(mat[seq_len(i), sector.name]), 2), c(0, 1),
                                                  + col = "white")
                                    }
                                  }
                            })