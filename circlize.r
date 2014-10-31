#Migracion de toda la vida
rm(list=ls())
setwd("C:\\Users\\Alvaro\\Documents\\GitHub\\R-files")
mt<-read.table("reporte.csv",sep=";",header=T)
library(circlize)
mt$depto<-as.factor(mt$depto)

pdf("mt12.pdf",width=7,height=7)
h<-prop.table(margin.table(as.matrix(mt[,2:10]),1))
col1<-rainbow(9)[c(8,1,3,2,4,9,5,7,6)]
circos.clear()
par(mar = c(2, 2,2, 2), lwd = 0.1, cex = 0.7)
circos.par("default.track.height" = 0.1)
circos.initialize(factors = mt$depto,xlim=c(0,2))
circos.trackPlotRegion(factors = mt$depto,seq(1:9),ylim=c(0,1),bg.border="black",bg.col = col1, track.height = 0.05)
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
title(main="Migracion de toda la vida, Bolivia 2012",cex.main=2,col.main="brown")
text(-1,-1.07,"Fuente: Elaboración propia en base al Censo Nacional de Poblacion y Vivienda 2012",cex=0.6,pos=4)
dev.off()