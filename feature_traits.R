##### Feature weights ### ------------------------------------------------------

# Directories --------------------------------------------------------------
genDir<-"S:/quinonesa/Simulations/functionAprox/ActCrit/"
scriptDir<-"d:/quinonesa/learning_models_c++/functAprox_actCrit/"


# libraries ----------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(scriptDir,"aesth_par.R",sep=""))
source(paste(scriptDir,"loadData.R",sep = ""))

# Load data ------------------------------------------------------------

setwd(genDir)

(listPar<-c(rep("rdNumSP",7),"gamma","neta"))
(listVal<-c(1,2,3,4,5,6,7,0.8,0))


FIAraw<-rbindlist(lapply(getFilelist(genDir,listPar,listVal)$FIA,
                         loadRawData,folder=genDir))

param<-getParam(genDir,listparam = listPar,values = listVal)


# Compute stats ----------------------------------------------------------------

FIAagg<-FIAraw[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x),
                                                             sd = sd(x))))),
               by=.(Age,AlphaCri,AlphaAct,Gamma,Neta,rdNumSP), 
               .SDcols=grep("[[:digit:]]",names(FIAraw))]

# PDF of paramer combination  --------------------------------------------------


idextPar<-8


pdf(paste(projDir,"\\",extpar,listVal[idextPar],"_.pdf",sep = ""))



# Client traits ----------------------------------------------------------------

# all trait

traits<-grep("choice",names(FIAraw),value=TRUE)[4:6]

tmp<-lapply(traits,function(x){strsplit(x,split="_")[[1]][1]})

nrows<-2
ncols<-2


ylimtemp<-c(0,0.5)
xlimtemp<-c(0,100)
countR<-1
countC<-0
i<-0
par(xaxt='s',yaxt='s')
numSP<-4
nbreaks<-8
png(paste(plotsdir,listPar[1],numSP,"dist.png",sep=""),
    width = 1000,height = 1000)
plot.new()

for(trait in tmp){
  i<-i+1
  countC<-countC+1
  par(plt=posPlot(numplotx = ncols,numploty = nrows,idplotx = countC,
                  idploty = countR),new=TRUE,las=1,cex.main=0.5)
  # xlimtemp<-c(min(FIAraw[get(extpar)==parH,.(get(paste(trait,"_choice",sep="")),
  #                                            get((paste(trait,"_discard",sep=""))))]),
  #             max(FIAraw[get(extpar)==parH,.(get(paste(trait,"_choice",sep="")),
  #                                            get(paste(trait,"_discard",sep="")))]))
  hist(c(0,100),ylim = ylimtemp,xlab="",ylab="",border = "white",main="")
  for(sp in sort(unique(FIAraw[LenRewNumSp==numSP,Species_choice]))){
    hist(c(FIAraw[(Type_choice==0&LenRewNumSp==numSP)&Species_choice==sp,
                  get(paste(trait,"_choice",sep=""))],
           FIAraw[(Type_discard==0&LenRewNumSp==numSP)&Species_discard==sp,
                  get(paste(trait,"_discard",sep=""))]),ylim=ylimtemp,main = "",
         col = colResidents[match(sp,sort(unique(FIAraw[LenRewNumSp==numSP,
                                                          Species_choice])))],
         freq = FALSE,xlab="",xlim=xlimtemp,ylab="",
         add=TRUE,breaks = nbreaks)
    hist(c(FIAraw[(Type_choice==1&LenRewNumSp==numSP)&Species_choice==sp,
                  get(paste(trait,"_choice",sep=""))],
           FIAraw[(Type_discard==1&LenRewNumSp==numSP)&Species_discard==sp,
                  get(paste(trait,"_discard",sep=""))]),main = "",xlab="",ylab="",
         col = colVisitors[match(sp,sort(unique(FIAraw[LenRewNumSp==numSP,
                                                          Species_choice])))],
         freq = FALSE,add=TRUE,ylim=ylimtemp,xlim=xlimtemp,breaks = nbreaks)
  }
  text(x=20,y=0.49,labels = trait)
  par(yaxt='n');
  if((i)%%ncols==0){
    countR<-countR+1
    countC<-0
    par(yaxt='s',xaxt='n')
  }
}
par(plt=posPlot(numplotx = ncols,numploty = nrows,idplotx = countC+1,
                idploty = countR),new=TRUE)
hist(c(0,100),ylim = ylimtemp,xlab="",ylab="",border = "white",main="")
legend("topright",legend = rep(sort(unique(FIAraw[LenRewNumSp==numSP,Species_choice])),2),
       col=c(colVisitors[1:numSP],colResidents[1:numSP]),#c("visitor","resident"),col = colours
       pch = 15,ncol = 2,title="Visitors    Residents")
dev.off()


#  Critic feature weights dyanamics--------------------------------------------------

nrows<-4
ncols<-3

extpar<-listPar[1]

countR<-1
countC<-0
i<-0
par(xaxt='s',yaxt='s')
plot.new()

ylimtemp<-c(min(FIAraw[get(extpar)==listVal[idextPar],
                       .SD,.SDcols=grep("_Crit",names(FIAraw),value = TRUE)]),
            max(FIAraw[get(extpar)==listVal[idextPar],
                       .SD,.SDcols=grep("_Crit",names(FIAraw),value = TRUE)]))



with(FIAagg[get(extpar)==listVal[idextPar]],{
  for(feat in grep("0_Crit.mean",names(FIAagg),value = TRUE)){
    i<-i+1
    countC<-countC+1
    par(plt=posPlot(numplotx = ncols,numploty = nrows,idplotx = countC,
                    idploty = countR),new=TRUE,las=1,cex.main=0.5)
    plot(c(min(Age),max(Age)),rep(0,2),type = "l",
         xlab = '',ylab='',ylim=ylimtemp,col="grey")
    polygon(c(Age,Age[length(Age):1]),
            c(get(feat)+get(grep("0_Crit.sd",names(FIAagg),
                                 value = TRUE)[i]),
              get(feat)[length(Age):1]-
                get(grep("0_Crit.sd",names(FIAagg),
                         value = TRUE)[i])[length(Age):1]),
            col = colours[1],border = FALSE)
    polygon(c(Age,Age[length(Age):1]),
            c(get(grep("1_Crit.mean",names(FIAagg),value = TRUE)[i])+
                get(grep("1_Crit.sd",names(FIAagg),
                         value = TRUE)[i]),
              get(grep("1_Crit.mean",names(FIAagg),
                       value = TRUE)[i])[length(Age):1]-
                get(grep("1_Crit.sd",names(FIAagg),
                         value = TRUE)[i])[length(Age):1]),
            col = colours[2],border = FALSE);
    lines(Age,get(feat),type = "l")
    lines(Age,get(grep("1_Crit.mean",names(FIAagg),
                       value = TRUE)[i]),type = "l")
    # title(main = feat,line = -4)
    legend('bottomleft',
           legend = c(feat,grep("1_Crit.mean",names(FIAagg),value=TRUE)[i])
           ,col = colours,pch = 15,cex = 0.5)
    par(yaxt='n');
    if((i)%%ncols==0)
    {
      countR<-countR+1
      countC<-0
      par(yaxt='s',xaxt='n')
    }
  }
})


# Actor feature weights dyanamics ----------------------------------------------


nrows<-4
ncols<-3

countR<-1
countC<-0
i<-0
par(xaxt='s',yaxt='s')
plot.new()

ylimtemp<-c(min(FIAagg[get(extpar)==listVal[idextPar],
                       .SD,.SDcols=grep("_Act",names(FIAagg),value = TRUE)]),
            max(FIAagg[get(extpar)==listVal[idextPar],
                       .SD,.SDcols=grep("_Act",names(FIAagg),value = TRUE)]))

with(FIAagg[get(extpar)==listVal[idextPar]&Neta==0],{
  for(feat in grep("0_Act.mean",names(FIAagg),value = TRUE)){
    i<-i+1
    countC<-countC+1
    par(plt=posPlot(numplotx = ncols,numploty = nrows,idplotx = countC,
                    idploty = countR),new=TRUE,las=1,cex.main=0.5)
    plot(c(min(Age),max(Age)),rep(0,2),type = "l",
         xlab = '',ylab='',ylim=ylimtemp,col="grey")
    polygon(c(Age,Age[length(Age):1]),
            c(get(feat)+get(grep("0_Act.sd",names(FIAagg),
                                 value = TRUE)[i]),
              get(feat)[length(Age):1]-
                get(grep("0_Act.sd",names(FIAagg),
                         value = TRUE)[i])[length(Age):1]),
            col = colours[1],border = FALSE)
    polygon(c(Age,Age[length(Age):1]),
            c(get(grep("1_Act.mean",names(FIAagg),value = TRUE)[i])+
                get(grep("1_Act.sd",names(FIAagg),
                         value = TRUE)[i]),
              get(grep("1_Act.mean",names(FIAagg),
                       value = TRUE)[i])[length(Age):1]-
                get(grep("1_Act.sd",names(FIAagg),
                         value = TRUE)[i])[length(Age):1]),
            col = colours[2],border = FALSE);
    lines(Age,get(feat),type = "l")
    lines(Age,get(grep("1_Act.mean",names(FIAagg),
                       value = TRUE)[i]),type = "l")
    # title(main = feat,line = -4)
    legend('bottomleft',
           legend = c(feat,grep("1_Act.mean",names(FIAagg),value=TRUE)[i])
           ,col = colours,pch = 15,cex = 0.5)
    par(yaxt='n');
    if((i)%%ncols==0)
    {
      countR<-countR+1
      countC<-0
      par(yaxt='s',xaxt='n')
    }
  }
})





