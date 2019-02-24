#*******************************************************************************************************
# STEP 4: BIOMASS PER ESDU AND LENGTH CLASS: COMPUTE BIOMASS-AT-LENGTHS PER ESDU AND SPECIES
#*******************************************************************************************************
# Author: Mathieu Doray mathieu.doray@ifremer.fr

library(EchoR)

#21. Compute biomass per ESDU and size class --------
#*************************************
# 21.1. Create esdu~haul length distribution link, based on esdu~haul link ----- 
#       Eventually, if minN!=NULL:
#           - select only valid hauls with no. of length measurements larger than minN
#           - in the case of ESDUs associated with invalid hauls, associate the ESDUs to valid hauls based on nearest distance criterion

# paths definition
#*************************************
path.results="C:/Users/SRC01/Documents/SILVIA_security copy/01. PELTIC/PELTIC2018/EchoR/results/peltic18/"

dir.create(file.path(path.results, 'AtLength'), showWarnings = FALSE)
path.results.length=paste(path.results,'AtLength/',sep='')
path.results2=paste(path.results,'AtLength/hauls/',sep='')
dir.create(file.path(path.results, 'AtLength/hauls/'), showWarnings = FALSE)

# 21.2. Compute abundance and biomass (in no. of fish) per size class, ESDU, and species in "spsel" ------- 
#*************************************
spsel=lsp.assess

spsel=c("ENGR-ENC","TRAC-TRA", "SARD-PIL","SPRA-SPR" )# #not working

# All assessed species biomass-at-lengths per esdu,  
#based on hauls with no. of length measurements larger than 10 
  #i just changed list.assoc=list.Assoc.year.ref
graphics.off()
biomEsduSize1=biomass.esdu.length(ESDUDEVs=ESDUDEVs,B.dev.sp.esdu.df=B.dev.sp.esdu.df,
                                  Pechef=Pechef,EspDev=EspDev,mens=mens,LW=LW,
                                  biomres.sp.esdu=biomres.sp.esdu,list.assoc=list.Assoc.year.ref,
                                  spsel=spsel,showDist=TRUE,minN=4,Pechels=Pechels,
                                  legpos='topleft',xlim=NULL,ylim=NULL,ux11=FALSE,
                                  logit=TRUE,nid='NOCHAL',verbose=FALSE,export.plot=path.results2,eps=1,
                                  compute.chamens=TRUE,forceFind=TRUE)

names(biomEsduSize1)
chamens.wide=biomEsduSize1$chamens.wide
chamens.long=biomEsduSize1$chamens.long
head(chamens.long)
Biomres.sp.size=biomEsduSize1$Biomres.sp.size
head(Biomres.sp.size)
list.sp.NW.size2=biomEsduSize1$list.sp.NW.size2
ESDUDEVs2=biomEsduSize1$ESDUDEVs2

# abundance and biomass per ESDU, echotype, species and size category
Biomres.echotype.codesp.size=biomEsduSize1$Biomres.echotype.codesp.size

#Check size distributions
#-> check that species plots are unimodal
plot.biomres.DistSize(BLsp=Biomres.sp.size,path.export=path.results,ux11=FALSE)
mtext('Abundance per ESDUs size distribution')
#compare with biological measurement size distributions
mens.range.check(mens,ux11=FALSE,verbose=FALSE)
mtext('biological measurement size distributions',cex=0.7)
par(mfrow=c(1,1))
#Check anchovy and sardine numbers (no. of fish) and biomass (in kg)
cbs=check.biomass.size(NW.esdu.sp.size.long=Biomres.sp.size,B.esdu.db=B.dev.sp.esdu.df,Pechef=Pechef)
head(Biomres.sp.size)
bss=Biomres.sp.size[Biomres.sp.size$sp=='ENGR-ENC',]
mw=bss$W/bss$N
summary(mw)

# Species length distributions
mLesdusp=aggregate(Biomres.sp.size$L*(Biomres.sp.size$N/Biomres.sp.size$Ntot),
          list(esdu=Biomres.sp.size$esdu.id,sp=Biomres.sp.size$sp),sum)
names(mLesdusp)[3]='mL'
aggregate(mLesdusp$mL,list(mLesdusp$sp),mean)
aggregate(mLesdusp$mL,list(mLesdusp$sp),summary)

# Species mean weight distributions
mWesdusp=aggregate(Biomres.sp.size$PM*(Biomres.sp.size$N/Biomres.sp.size$Ntot),
                   list(esdu=Biomres.sp.size$esdu.id,sp=Biomres.sp.size$sp),sum,na.rm=TRUE)
names(mWesdusp)[3]='mPM'
aggregate(mWesdusp$mPM,list(mWesdusp$sp),mean,na.rm=TRUE)
aggregate(mWesdusp$mPM,list(mWesdusp$sp),summary,na.rm=TRUE)

# 21.9. Maps of biomass per size class, species and esdu (X=species indices in lspcodes.mens2) --------
#*************************************
lspcodes.mens2=names(list.sp.NW.size2)
graphics.off()

lapply(X=seq(length(names(list.sp.NW.size2))),FUN=B.esdu.size.sp.plot2,
       list.sp.NW.size=list.sp.NW.size2,lspcodes.mens=lspcodes.mens2,
       EsduDev=ESDUDEVs,save.plot=TRUE,path.res.charef=path.results.length,
       logit=FALSE,v2plot='W',barplotit=TRUE,smax=1,
       plotit=list(Lclass1=T,LclassALL=F),
       #clims=list(c(0,10,20),c(0,11,20),c(0,50,100),c(0,23,40),c(0,17,30),c(0,26,50),c(0,27,50),c(0,20),c(0,18,60),c(0,16,50)))

#change the limits according to your species L range
clims=list(c(0,14),#CAPR-APE
           c(0,11,18),#ENGR-ENC
           c(0,5,15,20),#SARD-PIL
           c(0,5,10,13),#SPRA-SPR
           c(0,12)#TRAC-TRA
            ))
#Check your data to split it
# capros<- subset(mLesdusp,sp=="TRAC-TRA")
# summary(capros$mL)


# 21.10. Data export ---------
#*************************************
head(Biomres.sp.size)
dim(Biomres.sp.size)

write.table(Biomres.sp.size,paste(path.results.length,cruise,'_Biomres.sp.size.refhaul.csv',sep=''),
            row.names=FALSE,sep=';')

write.table(ESDUDEVs2,paste(path.results.length,cruise,'_chamens.csv',sep=''),
            row.names=FALSE,sep=';') #seems to be blank in the demo set

