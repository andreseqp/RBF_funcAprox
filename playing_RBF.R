RBF<-function(x,xCenter,sigSq){
  return(exp(-(x-xCenter)^2/(2*sigSq)))
}

totRBF<-function(x,xCenter,sigSq,featWeig){
  tmp<-sapply(xCenter, RBF,x=x,sigSq=sigSq)
  tmp<-apply(tmp, 1, FUN=crossprod,y=featWeig)
  #tmp<-apply(tmp,1,FUN = sum)
  return(tmp)
}

crossprod(seq(1,3,1),seq(1,30,10))

totRBF(rangx,rangCent,rangSigSq)

sum(sapply(rangCent,RBF,rangx,rangSigSq))

numfeat<-4
rangSigSq<-250
rangCent<-seq(0,100,length=numfeat)[1:(numfeat)]
rangx<-seq(0,100,length=1000)
featW<- seq(0.5,3.5,length=numfeat)
c(0.5,2.5,2,1)
 
  runif(numfeat,max=1)
png(paste(plotsdir,"cartoon2.png",sep=""),width = 1000,height = 1000)
par(plt=posPlot())
plot(totRBF(rangx,rangCent,rangSigSq,featW)~rangx,type='l',col=1,
     xlab="x",ylab="response",ylim=c(0,4),lwd=3)
for(i in 1:numfeat){
  lines(RBF(rangx,rangCent[i],rangSigSq)~rangx,col=i+1,lwd=3)  
}

dev.off()

featLoc<-matrix(data = NA,nrow = 4096,ncol = 6)

centers<-c(0,25,50,75)
count<-0
for(a in centers){
  for(b in centers){
    for(c in centers){
      for(d in centers){
        for(e in centers){
          for(f in centers){
            count<-count+1
            featLoc[count,]<-c(a,b,c,d,e,f)
          }
        }
      }
    }
  }
}

state<-matrix(c(runif(6,0,100)),nrow = 1)
state<-matrix(rep(0,6),nrow=1)

RBF(state,featLoc[1,],sigSq = 100)



distM<-dist(rbind(state,featLoc))[1:729]
distM[1:729]^2
distM[730:2*729]

sizeVis<-rnorm(1000,66,10)
sizeRes<-rnorm(1000,33,10)

rewVis<-sizeVis*2/100+rnorm(1000,0,0.1)
rewRes<-sizeRes*2/100+rnorm(1000,0,0.1)

par(plt=posPlot(1,2,1,2))
hist(sizeVis,xlim=c(0,100))
hist(sizeRes,add=TRUE)
par(plt=posPlot(1,2,1,1),new=TRUE)
hist(rewVis,xlim=c(0,2),main = "")
hist(rewRes,add=TRUE)

plot(c(rewVis,rewRes)~c(sizeVis,sizeRes))

png(paste(plotsdir,"cartoon3.png",sep=""),width = 1000,height = 700)
par(plt=posPlot(numplotx = 3,idplotx = 1))
plot(x=c(0,100),y=c(0,4),col=0,xlab="x",ylab="response")
abline(a=0,b=0.04,lwd=3)
par(plt=posPlot(numplotx = 3,idplotx =  2),new=TRUE)
plot(x=c(0,100),y=c(0,4),col=0,xlab="x",ylab="",yaxt="n")
abline(a=4,b=-0.04,lwd=3)
par(plt=posPlot(numplotx = 3,idplotx =  3),new=TRUE)
plot(x=c(0,100),y=c(0,4),col=0,xlab="x",ylab="",yaxt="n")
abline(a=2,b=-0.02,lwd=3)
dev.off()
