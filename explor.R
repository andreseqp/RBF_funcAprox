# ------------------ Exploration ------------------------ #

# Directories --------------------------------------------------------------
genDir<-"S:/quinonesa/Simulations/functionAprox/ActCrit/RBF"
scriptDir<-"d:/quinonesa/learning_models_c++/RBF_funcAprox/"
plotsdir<-"D:/quinonesa/Dropbox/Neuchatel/Results/functAprox/RBF/"


# libraries ----------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(scriptDir,"aesth_par.R",sep=""))
source(paste(scriptDir,"loadData.R",sep = ""))
source('D:/quinonesa/Dropbox/R_files/ternaryAEQP.R')
library('plotrix')




# Load data ------------------------------------------------------------

setwd(genDir)

(listPar<-c("TestRBF","alphC"))
(listVal<-c("",0.01))

(listPar<-rep("LenRewNumSp",4))
(listVal<-c(1,2,3,4))

FIAraw<-rbindlist(lapply(getFilelist(genDir,listPar,listVal)$FIA,
                         loadRawData,folder=genDir))

FIAraw[,idcombSps:=ifelse(test = Type_choice,
                          10*as.numeric(gsub("Sp",Species_choice,
                                             replacement = "")),
                          as.numeric(gsub("Sp",Species_choice,replacement = "")))+
         ifelse(test = Type_discard,
                10*as.numeric(gsub("Sp",Species_discard,replacement = "")),
                as.numeric(gsub("Sp",Species_discard,replacement = "")))]

FIAraw[,sizeDiff:=Height_choice-Height_discard]
                  
param<-getParam(genDir,listparam = listPar,values = listVal)



# FIAagg<-FIAraw[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x),
#                                                              sd = sd(x))))),
#                by=.(Age,AlphaCri,AlphaAct,Gamma,Neta,rdNumSP), 
#                .SDcols=grep("[[:digit:]]",names(FIAraw))]
# FIAtimeInt<-do.call(
#   rbind,lapply(
#     getFilelist(genDir,listPar,listVal)$FIA,
#     file2timeInter,interV=1001))

rm(FIAtimeInt)

FIAtimeIntValue<-rbindlist(lapply(getFilelist(genDir,listPar,listVal)$FIA,
                           file2timeInterValue,interV=5000))

# PIAtimeInt<-do.call(
#   rbind,lapply(
#     getFilelist(genDir,listPar,listVal)$PIA,
#     file2timeInter,interV=1001))

# Set simulation parameters to plot --------------------------------------------

idRep<-0
GammaP<-0.8
NetaP<-1
label<-"punish&future"

  
# Plot the dynamics of the clients values --------------------------------------------------------------

png(paste(plotsdir,listPar[1],"value_",label,".png",sep=""),
    width = 1000,height = 1000)
axisX<-c("n","n","s","s")
axisY<-c("s","n","s","n")
ylabs<-c("Estimated Value","","Estimated Value","")
xlabs<-c("","","trials","trials")

idPlotX<-c(1,2,1,2)
idplotY<-c(2,2,1,1)
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp])+c(0,0,-0.02,-0.02),
      new=TRUE,xaxt='s',yaxt='s')

  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&LenRewNumSp==nSp],{#
    plot(value~Age,type='p',ylim=c(0,5),xaxt=axisX[nSp],yaxt=axisY[nSp],
         xlab=xlabs[nSp],ylab=ylabs[nSp],pch=20,cex=1,
         col=coloptions[match(option,sort(unique(option)))])#
  legend('topleft',col =coloptions,legend = sort(unique(option)),pch =20,
         ncol = 3,cex = 0.8)
  })
}
text(y = 0.2,x=max(FIAraw[,Age])/2,labels = label)
dev.off()

# Plot the dynamics of the preference --------------------------------------------------------------

png(paste(plotsdir,listPar[1],"pref_",label,".png",sep=""),
    width = 1000,height = 1000)
ylabs<-c("Preference","","Preference","")
xlabs<-c("","","trials","trials")
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp])+c(0,0,-0.02,-0.02),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&LenRewNumSp==nSp],{#
    plot(preference~Age,type='p',xaxt=axisX[nSp],yaxt=axisY[nSp],
         xlab=xlabs[nSp],ylab=ylabs[nSp],pch=20,cex=1,
         col=coloptions[match(option,sort(unique(option)))])
    text(y = -8,x=max(Age)/10,labels =paste("num Sp= ",nSp,sep=""))
  })
}
legend('topleft',col =coloptions,legend = sort(unique(FIAraw$option)),pch =20,
       ncol = 3,cex = 0.8)
text(y = -8,x=max(FIAraw[,Age])/2,labels = label)
dev.off()



# Plot RV preference by species ------------------------------------------------

png(paste(plotsdir,listPar[1],"prefRV_",label,".png",sep=""),
    width = 1000,height = 1000)
axisX<-c("n","n","s","s")
axisY<-c("s","n","s","n")
ylabs<-c("preference","","preference","")
xlabs<-c("","","trials","trials")

idPlotX<-c(1,2,1,2)
idplotY<-c(2,2,1,1)
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp])+c(0,0,-0.02,-0.02),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&(option=="RV"&LenRewNumSp==nSp)],{#
    plot(preference~Age,type='p',xlab=xlabs[nSp],ylab=ylabs[nSp],
         pch=20,cex=1,ylim=c(-10,10),xaxt=axisX[nSp],yaxt=axisY[nSp],
         col=coloptions[match(idcombSps,sort(unique(idcombSps)))])
    legend('topleft',col =coloptions,legend = sort(unique(idcombSps)),
           pch =20,ncol = 3,cex = 0.8)
    abline(a=0,b=0,col="grey")
    text(y = -5,x=max(Age)/10,labels =paste("num Sp= ",nSp,sep=""))
  })
}
text(y = -2,x=max(FIAraw[,Age])/2,labels = label)
dev.off()

# Plot RV preference by difference in height -----------------------------------

png(paste(plotsdir,listPar[1],"prefRV_hei_",label,".png",sep=""),
    width = 1000,height = 1000)
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp]),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&(option=="RV")&LenRewNumSp==nSp],{#
    plot(preference~Age,type='p',
         pch=20,cex=1,xlab=xlabs[nSp],ylab=ylabs[nSp],
         xaxt=axisX[nSp],yaxt=axisY[nSp],
         col=paletteCont(100)[findInterval(Height_choice-Height_discard,
                                           seq(min(Height_choice-Height_discard),
                                               max(Height_choice-Height_discard),
                                               length=100))])
    color.bar.aeqp(paletteCont(100),min =round(min(Height_choice-Height_discard),2),
                   max = round(max(Height_choice-Height_discard),2),nticks = 3,
                   title = "",cex.tit = 2,numplotx = 12,
                   numploty = 6,idplotx = ifelse(idPlotX[nSp]==1,1,7),
                   idploty = ifelse(idplotY[nSp]==1,3,6),locAxis = 4)
  })
}
text(y = -2,x=max(FIAraw[,Age])/2,labels = label)
dev.off()
# Is preference correlated with size difference? -------------------------------

png(paste(plotsdir,listPar[1],"prefRV_hei_cor_",label,".png",sep=""),
    width = 1000,height = 1000)

xlabs<-c("","","Size diff","Size diff")
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp])+c(0,0,-0.02,-0.02),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&(option=="RV")
              &(LenRewNumSp==nSp&choice==0)],{#
                plot(preference~sizeDiff,type='p',
                     pch=20,cex=0.8,xlab=xlabs[nSp],ylab=ylabs[nSp],
                     xaxt=axisX[nSp],yaxt=axisY[nSp],xlim=c(-80,80),
                     col=coloptions[match(idcombSps,sort(unique(idcombSps)))])
                legend('topleft',col =coloptions,legend = sort(unique(idcombSps)),
                       pch =20,ncol = 3,cex = 0.8)
                abline(a=0,b=0,col="grey")
              })
}
text(y = -2,x=max(FIAraw[,Age])/2,labels = label)
dev.off()

# Is preference correlated with length difference? -------------------------------


xlabs<-c("","","Length","Length")
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp])+c(0,0,-0.02,-0.02),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&(option=="RV")
              &(LenRewNumSp==nSp&choice==0)],{#
                plot(preference~Length_choice,type='p',
                     pch=20,cex=0.8,xlab=xlabs[nSp],ylab=ylabs[nSp],
                     xaxt=axisX[nSp],yaxt=axisY[nSp],xlim=c(0,100),
                     col=coloptions[match(idcombSps,sort(unique(idcombSps)))])
                legend('topleft',col =coloptions,legend = sort(unique(idcombSps)),
                       pch =20,ncol = 3,cex = 0.8)
              })
  
}

# Preference dynamics for V0 options -------------------------------------------

par(plt=posPlot(numplotx = 1,idplotx = 1),xaxt='s',yaxt='s')
with(FIAraw[(Gamma==GammaP&Neta==NetaP)&(option=="V0"&LenNumSp==nSp)],{
  plot(logist(preference)~Age,type='p',
       xlab='Trials',pch=20,cex=1,
       col=coloptions[match(Species_choice,sort(unique(Species_choice)))])
  legend('topright',col =coloptions,legend = sort(unique(Species_choice)),
         pch =20,ncol = 3,cex = 0.8)
  abline(a=0.5,b=0,col="grey")
})




par(plt=posPlot(1,idplotx = 1),xaxt='s',yaxt='s')
with(FIAraw[(option=="RV")&(Gamma==GammaP&Neta==NetaP)],{
  plot(Type_choice~Age,type='p',
     xlab='Trials',ylab='Choice',pch=20,cex=1,xaxt='n',
     col=coloptions[match(option,sort(unique(option)))])
  axis(4)
  # legend('topright',col =coloptions,legend = unique(option),pch =20,
  #      ncol = 3,cex = 0.8)
})



sum(FIAraw[(option=="RV"&Age>max(Age)*0.8)&(Gamma==GammaP&Neta==NetaP),
           Type_choice==1])/
  sum(FIAraw[(option=="RV"&Age>max(Age)*0.8)&(Gamma==GammaP&Neta==NetaP),
             Type_choice<3])

sum(FIAraw[(option=="R0"&Age>max(Age)*0.9)&(Gamma==GammaP&Neta==NetaP),
           Type_choice==0])/
  sum(FIAraw[(option=="R0"&Age>max(Age)*0.9)&(Gamma==GammaP&Neta==NetaP),
      Type_choice<3])

sum(FIAraw[(option=="V0"&Age>max(Age)*0.9)&(Gamma==GammaP&Neta==NetaP),Type_choice==1])/
  sum(FIAraw[(option=="V0"&Age>max(Age)*0.9)&(Gamma==GammaP&Neta==NetaP),Type_choice<3])


FIAraw[(Training==0&option=="R0")&Type_choice==2,.(choice,Age)]

FIAraw[Type_choice<2,
       length(Age),by=.(Type_choice,choice)]

FIAraw[choice==0,length(Age),by=.(choice,Type_choice)]



# one trait

traits<-grep("choice",names(FIAraw),value=TRUE)[4:14]

trait<-strsplit(traits[2],split = "_")[[1]][1]

extpar<-listPar[1]

parH<-3

par(plt=posPlot())
xlimtemp<-c(min(FIAraw[get(extpar)==parH,.(get(paste(trait,"_choice",sep="")),
                          get((paste(trait,"_discard",sep=""))))]),
            max(FIAraw[get(extpar)==parH,.(get(paste(trait,"_choice",sep="")),
                          get(paste(trait,"_discard",sep="")))]))
densityplot(~ blueSec_choice | Type_choice,
            data=FIAraw[Type_choice<2&get(extpar)==listVal[idextPar]],
            groups = Species_choice,plot.points=FALSE)
with(FIAraw[get(extpar)==listVal[idextPar]&Type_choice==0],{
  sm.density.compare(x = as.data.frame(FIAraw[Type_choice<2])$Height_choice,
                     group = as.data.frame(FIAraw[Type_choice<2])$Species_choice)  
})

typeof(FIAraw[,Height_choice])

  c(FIAraw[Type_choice==0&get(extpar)==parH,
              get(paste(trait,"_choice",sep=""))],
       FIAraw[Type_discard==0&get(extpar)==parH,
              get(paste(trait,"_discard",sep=""))]),
     col = colours[2],xlim=xlimtemp,freq = FALSE)
hist(c(FIAraw[Type_choice==1&get(extpar)==parH,
              get(paste(trait,"_choice",sep=""))],
       FIAraw[Type_discard==1&get(extpar)==parH,
              get(paste(trait,"_discard",sep=""))]),
     col = colours[1], xlim=xlimtemp,freq = FALSE,add=TRUE)
legend("topleft",legend = c("visitor","resident"),col = colours,pch = 15,
       title = trait)

FIAraw[redMain_choice>100,]
cbind(traits[1:8],param[[1]]$residents$Sp1$means,param[[1]]$visitors$Sp1$means)

diff(fromJSON("s:/quinonesa/Simulations/functionAprox/ActCrit/mHeight30_/parameters.json"),
     fromJSON("s:/quinonesa/Simulations/functionAprox/ActCrit/rdMeanSD1_/parameters.json"))





# parH<-3
# 
# par(plt=posPlot(numplotx = 2,idplotx = 2)+c(-0.05,-0.05,0,0),yaxt='s',
#     las=1,xaxt='s',new=TRUE)
# with(FIAIntValstats[get(extpar)==parH&Neta==0],{
#   plotCI(x=Interv,y=meanVal,
#          ui = upIQRVal,li=lowIQRVal,
#          pch=16,xlab='',ylab='',
#          col=coloptions[match(option,unique(option))],
#          sfrac=0.002,cex.axis=1.3)
#   lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
#   legend('topleft',col =coloptions,legend = unique(option),pch =20,
#          ncol = 3,cex = 0.8)
# })







FIAIntValstats[rdNumSP==5]

par(plt=posPlot(numplotx = 2,idplotx = 2),yaxt='s',las=1,
    xaxt='s',new=TRUE)
with(FIAIntValstats[option=="RV"&Neta==1],{
  plotCI(x=Interv+posit,y=meanChoice,
         ui = upIQRChoice,li=lowIQRChoice,
         pch=16,xlab='',ylab='',yaxt='n',ylim=c(0.5,1),
         col=colboxes[match(get(extpar),unique(get(extpar)))],
         sfrac=0.002,cex.axis=1.3)
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  legend('topleft',col =colboxes,legend = unique(get(extpar)),pch =20,
         ncol = 3,cex = 0.8)
})


# Try to understand the failures --------------------------------------------------------

tmp<-FIAraw[option=='RV'& Age>0.5*max(Age)]

tmp[,':='(length.diff=Length_choice-Length_discard,
          height.diff=Height_choice-Height_discard,
          redMain.diff=redMain_choice-redMain_discard)]

names(tmp)

npx<-3

par(plt=posPlot(numplotx = npx,idplotx = 1)-c(0.05,0.05,0,0))
plot(Type_choice~length.diff,data=tmp)
lines(x = rep(0,2),y=c(0,1),col='grey')

par(plt=posPlot(numplotx = npx,idplotx = 2)-c(0.05,0.05,0,0),new=TRUE)
plot(Type_choice~height.diff,data=tmp,yaxt='n',ylab='')
lines(x = rep(0,2),y=c(0,1),col='grey')

par(plt=posPlot(numplotx = npx,idplotx = 3)-c(0.05,0.05,0,0),new=TRUE)
plot(Type_choice~redMain.diff,data=tmp,yaxt='n',ylab='')
lines(x = rep(0,2),y=c(0,1),col='grey')

Heig.mod<-glm(Type_choice~length.diff+height.diff+redMain.diff,
              family = binomial,data = tmp)
summary(Heig.mod)

Heig.mod.int<-glm(Type_choice~length.diff*height.diff*redMain.diff,
              family = binomial,data = tmp)
summary(Heig.mod.int)

Heig.mod.length<-glm(Type_choice~Length_1_0+Length_1_1+Length_2_0+Length_2_1,
              family = binomial,data = tmp)
summary(Heig.mod.length)

Heig.mod.height<-glm(Type_choice~Height_1_0+Height_1_1+Height_2_0+Height_2_1,
                     family = binomial,data = tmp)
summary(Heig.mod.height)






