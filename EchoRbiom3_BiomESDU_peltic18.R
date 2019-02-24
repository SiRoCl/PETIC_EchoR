#*******************************************************************************************************
# STEP 3: BIOMASS PER ESDU: COMPUTE BIOMASS PER ESDU AND SPECIES AND TOTAL BIOMASS ESTIMATES IN ESTIMATION POLYGON
#**************************** ***************************************************************************
# Author: Mathieu Doray mathieu.doray@ifremer.fr

#Set up
#*************************************
library(EchoR)

path.results.esdu=path.near

# 14. Compute abundance per ESDU, echotype and species, in kg and no. of fish ---------
# NB: if you are using the reference haul method, use:
#    list.Assoc.year=list.Assoc.year.ref
# for the nearest haul method, use:
#    list.Assoc.year=list.Assoc.year.near
#*************************************

B.esdu.sp.res=multi.acoubiom.esdu(ESDUDEV=ESDUDEVs,EspDev,Pechef,
                                  list.Assoc.year=list.Assoc.year.near,
                                  list.XeB,list.XeN)
#head(B.esdu.sp.res[[3]][[1]])
####code continues
names(B.esdu.sp.res)

#Results in long format  
B.dev.sp.esdu.df=B.esdu.sp.res$B.dev.sp.esdu.df.long
head(B.dev.sp.esdu.df)  
dim(B.dev.sp.esdu.df)

#Results in wide format for plotting and length class computations
BB.dev.sp.esdu.df.wide=B.esdu.sp.res$BB.dev.sp.esdu.df[[1]]
BN.dev.sp.esdu.df.wide=B.esdu.sp.res$BN.dev.sp.esdu.df[[1]]

# 15. Check abundance and biomass per esdu results -------
#*************************************
Bsum.sp.esdu=biomass.esdu.check2(B.dev.sp.esdu.df,saveIt=TRUE,plotIt=TRUE,Pechef=Pechef) #function modified to save the plots as they are computed

# 16. Compute abundance per ESDU and species, in kg and no. of fish --------
#*************************************
# 16.1. Abundance per ESDU and species, in kg and no. of fish -------
#*************************************
biomass.esdu.sp.res=biomass.esdu.sp(B.dev.sp.esdu.df)
names(biomass.esdu.sp.res)
B.sp.esdu.df=biomass.esdu.sp.res$B.sp.esdu.df
mB.sp.esdu.df=biomass.esdu.sp.res$mB.sp.esdu.df
mB.spsize.df=biomass.esdu.sp.res$mB.spsize.df
#mB.spsize.df[,3:4] <-round(mB.spsize.df[,3:4],digits=2)

# 17. Maps of biomass per echotype, species and esdu --------
#and total biomass per species, echotype and cruise
#*************************************
ldevs=unlist(lNdev)
for (i in 1:length(lyears)){
  #cruise selection
  #i=1
  biomtot.dev.spi=lapply(X=seq(length(ldevs)),
                         FUN=Bdev.sp.plot2,biom.esdu.dev.sp=BB.dev.sp.esdu.df.wide,
                         Ndev=unlist(lNdev),save.plot=TRUE,sp.sel=FALSE,plotit=TRUE,
                         path.res.charef=path.ref,smax=1,pradius=.5,
                         legpos='topright',ux11=FALSE) #if ux11=FALSE is directly saved, not shown
  for (j in 1:length(biomtot.dev.spi)){
    if (j==1){
      biomtot.dev.sp=biomtot.dev.spi[[j]]
    }else{
      biomtot.dev.sp=rbind(biomtot.dev.sp,
                           biomtot.dev.spi[[j]])
    }
  }
  if (i==1){Btot.dev.sp.df=biomtot.dev.sp
  }else{Btot.dev.sp.df=rbind(Btot.dev.sp.df,biomtot.dev.sp)}
}

#all biomass per EDSU and species DONE

# 18. Longitudes projection: multiply longitudes by cos(mean(latitudes)) -------
# to ensure exact distance computation
#*************************************
ylat=mean(B.sp.esdu.df$LAT)
mcos=cos(ylat*pi/180)
B.sp.esdu.df$xcor=B.sp.esdu.df$LONG*mcos

# 19. Plot species biomass maps -------
#*************************************
# 19.1. Maps of anchovy biomass per echotype and esdu -------
#*************************************
graphics.off()
sp.plot.res=sp.plot(B.sp.esdu.df1=B.dev.sp.esdu.df,sp='ENGR-ENC',
                    logit=TRUE,polygoni=NULL,projectit=TRUE,bathy=FALSE,
                    ylat=NULL,cruise=cruise,zmult=.5,spcol=3)

# 19.2. Maps of anchovy biomass per esdu --------
#*************************************
#x11()
sp.plot.res.AN=sp.plot(B.sp.esdu.df1=B.sp.esdu.df,sp='ENGR-ENC',logit=TRUE,
                       polygoni=NULL,projectit=TRUE,bathy=FALSE,ylat=NULL,cruise=cruise,
                       zmult=0.5)

dev.print(device=png,filename=paste(path.results,cruise,"_AnchovyMap.png",sep=''),width=800,height=800)

names(B.sp.esdu.df)

# dev.print(device=png,
#           filename=paste(path.results,'/',cruise,"_AnchovyMap.svg",sep=''),
#           width=2400,height=2400)


#Extract biomass per species and esdu
names(sp.plot.res.AN)
B.sp.esdu.df.AN=sp.plot.res.AN$B.1sp.esdu.df
B.sp.esdu.df.AN.in=sp.plot.res.AN$B.1sp.esdu.df.in
head(B.sp.esdu.df.AN)
dim(B.sp.esdu.df.AN)
length(unique(B.sp.esdu.df.AN$TC))
summary(B.sp.esdu.df.AN$BB)
summary(B.sp.esdu.df.AN.in$BB)

# 19.3. Maps of sardine biomass per echotype and esdu ---------
#*************************************
#x11()
spdev.plot.res.SA=sp.plot(B.sp.esdu.df1=B.dev.sp.esdu.df,sp='SARD-PIL',
                          logit=TRUE,polygoni=NULL,projectit=TRUE,spcol=4,ylat=NULL,
                          bathy=FALSE,cruise=cruise,zmult=0.1)

dev.print(device=png,filename=paste(path.results,cruise,"_Sardinemap1.png",sep=''),width=800,height=800)

# 19.4. Maps of sardine biomass per esdu -------
#*************************************
sp.plot.res.SA=sp.plot(B.sp.esdu.df1=B.sp.esdu.df,sp='SARD-PIL',
                       logit=TRUE,polygoni=NULL,projectit=TRUE,spcol=4,
                       bathy=FALSE,ylat=NULL,cruise=cruise,zmult=0.1)

dev.print(device=png,filename=paste(path.results,cruise,"_Sardinemap2.png",sep=''),width=800,height=800)

#Extract sardine results per esdus
B.sp.esdu.df.SA=sp.plot.res.SA$B.1sp.esdu.df
B.sp.esdu.df.SA.in=sp.plot.res.SA$B.1sp.esdu.df.in
head(B.sp.esdu.df.SA)

# 19.5. Maps of sprat biomass per esdu -------
#*************************************
sp.plot.res.SA=sp.plot(B.sp.esdu.df1=B.sp.esdu.df,sp='SPRA-SPR',
                       logit=TRUE,polygoni=NULL,projectit=TRUE,spcol=1,
                       bathy=FALSE,ylat=NULL,cruise=cruise,zmult=0.1)

dev.print(device=png,filename=paste(path.results,cruise,"_Spratmap1.png",sep=''),width=800,height=800)


# 20. Compute anchovy, sardine and sprat total biomass, ----------
#     based on mean biomass per EDSU
# total biomass = mean biomass per EDSU (per NM) * convex polygon surface  (NM²)
# the total biomass will be slightly under-estimated, as the convex polygon boundaries might be the outermost ESDUs
#*************************************
# 20.1. Automatic definition of a convex estimation polygon around esdus ---------
#several ways
#*************************************
graphics.off()

#PBS mapping style #(automathic style)
library(PBSmapping)
xyRegion=as.EventData(data.frame(EID=ESDUDEVs$esdu.id,X=ESDUDEVs$LONG*mcos,
                                 Y=ESDUDEVs$LAT))
cxpoly1=calcConvexHull(xyRegion)
plotMap(cxpoly1)
addPoints(xyRegion)
(area1=calcArea(cxpoly1)*3600)  #29540.88

#OR alphahull style #(more accurate polygon)
xx=ESDUDEVs$LONG*mcos
yy=ESDUDEVs$LAT

library(alphahull)
apoly=ahull(xx,yy,alpha=.4) #Error in tri.mesh(X) : duplicate data points
plot(apoly)
(areaf=areaahull2(apoly)*3600)  #21398.31

# 18.2.1. OR manual definition: draw polygon around data ESDUs by hand
# #
# x11()
# plot(B.sp.esdu.df$xcor,B.sp.esdu.df$LAT,type='n')
# points(B.sp.esdu.df$xcor, B.sp.esdu.df$LAT,pch=1,
#        cex=0.1,col='grey20')
# points(B.sp.esdu.df$xcor, B.sp.esdu.df$LAT,pch=16,
#        cex=log(B.sp.esdu.df$BB+1)/5,col=2)
# p=locator(type='l')
# 
# #Define projected polygon, add coordinates in nautical miles  
# polyman=data.frame(xcor=p$x,LATF=p$y)
# polyman$xcornm=60*polyman$xcor
# polyman$ynm=60*polyman$LATF
# polyman$LONF=polyman$xcor/mcos
# polyman.Pelgas2014=polyman
# # 
#  #Export polygon
# write.table(polyman.Pelgas2014,paste(path.results,cruise,'_polygon.csv',sep=''),
#             sep=';',row.names=FALSE)
# 
# #polygon area
# psa=data.frame(PID=1,POS=as.numeric(row.names(polyman)),
#            X=polyman$xcor,Y=polyman$LATF)
# poly.pbs=as.PolySet(psa)
#plot(poly.pbs)
#(area1=calcArea(poly.pbs,rollup=2))

(area.man=area1$area*3600)
(area.pbs=calcArea(cxpoly1)$area*3600) #29540.88
#(area.aa=areaahull2(apoly)*3600) #error

# 20.2. Compute mean anchovy, sardine and sprat biomass per esdu --------
#*************************************
# in the estimation polygon
      #mean biomass in the area by avering the mean biomass per esdu
mBB.AN=mean(B.sp.esdu.df.AN$BB)  
mBB.SA=mean(B.sp.esdu.df.SA$BB)

# 20.3. Compute anchovy, sardine and sprat total biomass ---------
#*************************************
# in the estimation polygon
areaf=area.pbs

(BB.AN=mBB.AN*areaf) #and then you just multiple for the area and you have the estimation of the global biomass
(BB.SA=mBB.SA*areaf)

mB.spsize.df$biomW.esdu=mB.spsize.df$BB*areaf

# 20.4. Compute all species total biomass --------
#*************************************
biomres.sp.esdu=mB.sp.esdu.df
biomres.sp.esdu$area.esdu=areaf
biomres.sp.esdu$biomW.esdu=biomres.sp.esdu$mBB*biomres.sp.esdu$area
biomres.sp.esdu$biomN.esdu=biomres.sp.esdu$mBN*biomres.sp.esdu$area
biomres.sp.esdu=merge(biomres.sp.esdu,Bsum.sp.esdu[,
                                                   !names(Bsum.sp.esdu)%in%c('mBB','mBN')],by.x='sp',by.y='sp')
names(biomres.sp.esdu)
biomres.sp.esdu[,c('sp','area.esdu','biomW.esdu','biomN.esdu')]
# biomass in tons, abundance in no. of fish
biomres.sp.esdu$BB*1e6/biomres.sp.esdu$BN
biomres.sp.esdu$biomW.esdu*1e6/biomres.sp.esdu$biomN.esdu

# 20.5. Export biomass per ESDU results ----------
#*************************************
#(it is paste in the folder ref.haul or ref.near, depending on which you choose (step 11 or 12))
write.table(biomres.sp.esdu,paste(path.results.esdu,paste(cruise,'_biomtot.sp.MethEsdu.csv',sep=''),sep=''),sep=';',row.names=FALSE)
write.table(B.dev.sp.esdu.df,paste(path.results.esdu,paste(cruise,'_biom.dev.codesp.esdu.df.csv',sep=''),sep=''),sep=';',row.names=FALSE)

