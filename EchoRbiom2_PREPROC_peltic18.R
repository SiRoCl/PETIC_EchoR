#*******************************************************************************************************
# STEP 2: PRE-PROCESSING: COMPUTE AND CHECK XE SCALING FACTORS, DEFINE AND CHECK ESDU~HAULS ASSOCIATIONS
#*******************************************************************************************************
# Author: Mathieu Doray mathieu.doray@ifremer.fr

library(EchoR)

#8. Computes average fish backscatter per echotype around each haul ------ 
# (used only in the biomass per region method)
#*****************************************************************
par(mfrow=c(1,1),bg='white') #i change the mfrowc, originally it was mfrowc=c(2,3)
#ux11 was set to TRUE
list.mSa.radius.db=multi.mSa.radius.dev(ESDUDEVs,
                                         Pechef,EspDev,hradius=6,plotit=1,mfrowc=c(2,3),
                                        nid='NOCHAL',ux11=FALSE)
length(list.mSa.radius.db)
dev.print(device=png,
           filename=paste(path.results,cruise,"_sAweightHaulsMap.png",sep=''),
           width=800,height=800)

# 9. Compute ECHO-INTEGRATION FACTOR PER SPECIES: -----------
# X[ie] : ie=haul.species
 #*****************************************************************
# 9.1. check if species are only found in echotype list ------
 #*****************************************************************
unique(EspDev$CodEsp)[!unique(EspDev$CodEsp)%in%unique(Pechef$CodEsp)]

# 9.2. Compute Xe's -------
#*****************************************************************
list.Xe.BN=Xe.compute(captures.comps2=Pechef,
                      compute.it=list(XeB=TRUE,XeN=TRUE),
                      EspDev=EspDev,DEVit=TRUE,TS.fL=FALSE,Unit.dev='sA')
names(list.Xe.BN)

# Extract Xe's in kg
#*****************************************************************
list.XeB=list.Xe.BN$XeB

# Extract Xe's in no. of fish
#*****************************************************************
list.XeN=list.Xe.BN$XeN

# 9.3. Aggregate all Xe's in dataframe format -------
#*****************************************************************
XeB.df=Xe.dframe.it(list.Xe=list.XeB,lNdev=lNdev)
XeN.df=Xe.dframe.it(list.Xe=list.XeN,lNdev=lNdev)
head(XeB.df)
unique(XeB.df$CodEsp)

#check sigma spi and TS
xe.check=Xe.NOCHAL(Peche=Pechef,Mens=NULL,unite='sA',TSkg=FALSE,TS.fL=FALSE,method='P')  
sigma.spi=4*pi*10^((Pechef$CAC*log10(Pechef$LM)-Pechef$BAC)/10)
TSi=Pechef$CAC*log10(Pechef$LM)-Pechef$BAC
summary(TSi)
par(oma=c(3,3,3,3),mfrow=c(1,1)) #increase plot margins, default is 0
hist(TSi,main='TS distribution')
aggregate(TSi,list(Pechef$GENR_ESP),summary)

# 10. Xe's check --------
#***************************************************************** 
# 10.1. Check for erroneous Xe's (NA or infinite) --------
#*****************************************************************
Xe.pb=XeB.df[is.na(XeB.df$Xe)|is.infinite(XeB.df$Xe),]
if (dim(Xe.pb)[1]>0) stop ('Erroneous Xes found')

# 10.2. Xe visual check: Xe "profiles" --------
#*****************************************************************
#All Xe's boxplots
#it is important XE 0-0.2 are the normal values, except for scomber scombus that have very high XE values
#it is not giving me very high values...
#*****************************************************************
XeB.df$CodEsp=as.character(XeB.df$CodEsp)
head(XeB.df)

#Xe's per CodEsp, all cruises
dev.off()
par(mar=c(3,10,3,1))
boxplot(Xe~CodEsp,XeB.df,las=2,horizontal=TRUE,main=paste(cruise,'Xes'))
dev.print(device=png,
          filename=paste(path.results,cruise,"_XeCheck.png",sep=''),
          width=800,height=800)

#Xe's per echotype boxplots
#*****************************************************************  
par(mfrow=c(1,1))
list.Xe=list.XeB
if (plotit){
  for (k in 1:length(lyears)){
    list.Xei=list.Xe[[k]]
    Ndevi=paste('D',unlist(lNdev[[k]]),sep='')
    EspDevi=EspDev[EspDev$CAMPAGNE==as.character(lyears[k]),]
    
    for (i in 1:length(Ndevi)){
      xei=list.Xei[[i]]
      xei$CodEsp=as.character(xei$CodEsp)
      if (i==1){
        par(mfrow=c(2,3),mar=c(3,9,3,1))
      }
      boxplot(Xe~CodEsp,xei,main=paste(lyears[k],' ',
                                       Ndevi[i],sep=''),las=2,horizontal=TRUE)
      #dev.off()
    }
  }
}
dev.print(device=png,
          filename=paste(path.results,cruise,"_XeDEVCheck.png",sep=''),
          width=1000,height=800)


# 11. Hauls / ESDUs associations definition ---------
#*****************************************************************
# 11.1 Echotypes in ESDUs associated to nearest haul comprising species-in-echotype --------
#(automatic)
#*****************************************************************
for (k in 1:length(lyears)){
  EsduDevi=ESDUDEVs[ESDUDEVs$CAMPAGNE==as.character(lyears[k]),]
  Ndevi=paste('D',c(unlist(lNdev[[k]])),sep='')
  EspDevi=EspDev[EspDev$CAMPAGNE==as.character(lyears[k]),]
  Pechei=Pechef[Pechef$CAMPAGNE==as.character(lyears[k]),]
  #if (!TRUE%in%is.na(Pechei$LONF)){
  near.Associ=nearest.haul(Pechei=Pechef,EsduDevi=ESDUDEVs,
                           EspDevi=EspDev,Ndevi,nid=nid)
  list.Associ=near.Associ$list.Assoc
  #}else list.Associ='NA in haul positions'
  names(list.Associ)=Ndevi
  if (k==1){
    list.Assoc.year=list(list.Associ)
  }else{
    list.Assoc.year=c(list.Assoc.year,list(list.Associ))
  }
}

list.Assoc.year.near=list.Assoc.year

# 11.2 AND/OR echotypes in ESDUs manually associated to a trawl haul ---------
#(will keep the reference haul indicated in the reference haul)
#*****************************************************************

# Error in `[.data.frame`(EsduDevi, , Dir) : undefined columns selected  for this year, i run then 11 instead

for (k in 1:length(lyears)){
  EsduDevi=ESDUDEVs[ESDUDEVs$CAMPAGNE==as.character(lyears[k]),]
  Ndevi=paste('D',c(unlist(lNdev[[k]])),sep='')
  EspDevi=EspDev[EspDev$CAMPAGNE==as.character(lyears[k]),]
  Pechei=Pechef[Pechef$CAMPAGNE==as.character(lyears[k]),]

  cat(as.character(lyears[k]),"\n")
  list.Associ=lapply(X=seq(length(Ndevi)),FUN=assoc.k.gen2,
                     Ndevi,EsduDevi)
  names(list.Associ)=Ndevi
  if (k==1){
    list.Assoc.year=list(list.Associ)
  }else{
    list.Assoc.year=c(list.Assoc.year,list(list.Associ))
  }
}

list.Assoc.year.ref=list.Assoc.year   #creates o values in the trawls, don't know for which reason!!!-----




# 12. Check esdu/haul association ----------
# NB: if you are using the reference haul method, use:
#    list.Associ=list.Assoc.year.ref[[1]] (step 11)
# for the nearest haul method, use:
#    list.Associ=list.Assoc.year.near[[1]] (step 12)
#*****************************************************************
# 12.1. Show ESDU-hauls associations, nearest haul --------
#*****************************************************************
path.near=paste(path.results,'nearHaul/',sep='')
dir.create(file.path(path.results, 'nearHaul'), showWarnings = FALSE)

if (plotit){
   show.esdu.refhaul2(Ndevi=Ndevi,list.Associ=list.Assoc.year.near[[1]],
                     Pechelsi=Pechels,Pechei=Pechef,EsduDevi=ESDUDEVs,
                     legpos='bottomleft',export.plot=path.near,radSA=0.05,
                     pradius=0.5,scale.SA=2,xlim=c(-7.2,-2.1),ylim=NULL,logit=TRUE,
                     labelit=TRUE,nid='NOCHAL',ux11=FALSE)
 }



# 12.2. Show ESDU-hauls associations, reference haul ----------
#*****************************************************************
# path.ref=paste(path.results,'refHaul/',sep='')
# dir.create(file.path(path.results, 'refHaul'), showWarnings = FALSE)
# 
# if (plotit){
#   show.esdu.refhaul2(Ndevi=Ndevi,list.Associ=list.Assoc.year.ref[[1]],
#                     Pechelsi=Pechels,Pechei=Pechef,EsduDevi=ESDUDEVs,
#                     legpos='topleft',export.plot=path.ref,radSA=0.05,
#                     pradius=0.5,scale.SA=2,xlim=c(-7.2,-2.1),ylim=NULL,logit=TRUE,
#                     labelit=TRUE,nid='NOCHAL',ux11=FALSE)
# }

# 13. Export pre-processing results --------------
#*****************************************************************
boo <- mapply(function(x,y) within(x, GROUP <- y), list.Assoc.year.near[[1]], names(list.Assoc.year.near[[1]]),SIMPLIFY=FALSE)
write.table(do.call(rbind, boo),paste(path.results,'list.Assoc.year.near.csv',sep=''),row.names=FALSE,sep=';')
#boo <- mapply(function(x,y) within(x, GROUP <- y), list.Assoc.year.ref[[1]], names(list.Assoc.year.ref[[1]]),SIMPLIFY=FALSE)
#write.table(do.call(rbind, boo),paste(path.results,'list.Assoc.year.ref.csv',sep=''),row.names=FALSE,sep=';')
boo <- mapply(function(x,y) within(x, GROUP <- y), list.XeB[[1]], names(list.XeB[[1]]),SIMPLIFY=FALSE)
write.table(do.call(rbind, boo),paste(path.results,'list.XeB.csv',sep=''),row.names=FALSE,sep=';')
boo <- mapply(function(x,y) within(x, GROUP <- y), list.XeN[[1]], names(list.XeN[[1]]),SIMPLIFY=FALSE)
write.table(do.call(rbind, boo),paste(path.results,'list.XeN.csv',sep=''),row.names=FALSE,sep=';')
