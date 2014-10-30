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

