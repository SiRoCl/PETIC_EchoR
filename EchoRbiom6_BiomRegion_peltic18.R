#*******************************************************************************************************
# STEP 6: BIOMASS AND ESTIMATION ERROR PER REGION: COMPUTE BIOMASS AND ESTIMATION ERROR PER REGION AND SPECIES
#*******************************************************************************************************
# Author: Mathieu Doray mathieu.doray@ifremer.fr

library(EchoR)

# paths definition
#*********************************************
#path.regions='C:/Users/SRC01/Documents/SILVIA_security copy/01. PELTIC/PELTIC2018/EchoR/data/regions'#regions from PELTIC2017
path.regions="C:/Users/SRC01/Documents/SILVIA_security copy/01. PELTIC/PELTIC2018/EchoR/data/regions_2" #region 7 extended to include 2 trawls from region10

#ICES rectangles
#path.regions='C:/Users/SRC01/Documents/SILVIA/T3_EchoR/peltic_adapt/ICES_ractangles'
#like this works, so you should eliminate the last slash
      #myShapeInR<-readOGR("C:/Program Files/R/R-3.3.2/library/EchoR/data","EchoRtest_region6_CLAS")

path.results.regions=paste(path.results,'regions2/',sep='')
dir.create(path.results.regions,showWarnings = TRUE,recursive=TRUE)

      #read the EDUDEVs file again
      # setwd("C:/Users/SRC01/Documents/SILVIA/T3_EchoR/peltic_adapt/results 2017-11-30")
      ESDUDEVs <- read.table(paste(path.results,"ESDUDEVs.csv",sep=""), sep=";",header=T) #if you reexport regions need to read it again

#*************************************************************************
# 23. Compute biomass per region
#*************************************************************************
#+++bit added to unify region 2 and 5 into a unique region----
# library("rgdal")
# library("rgeos")
# library("dplyr")
# 
# setwd('C:/Users/SRC01/Documents/SILVIA/T3_EchoR/peltic_adapt/Strata')
# strata <- readOGR(".","peltic_strata")
# summary(strata@data)
# plot(strata); strata@data
# #write.table(as.data.frame(strata),paste("C:/Users/SRC01/Desktop/PELTIC17_sprat_checking/","area_strata.csv"),row.names=FALSE)
# 
# lu<- data.frame()
# lu <- rbind(lu, strata@data)
# lu$OBJECTID <- as.character(strata$OBJECTID)
# lu$label <- as.character(strata$label)
# lu$AREA_GEO <-as.numeric(strata$AREA_GEO)
# 
# lu$idtom <- substr(lu$label,14,15)
# lu$idtom[which(lu$idtom %in% c("2A","2B"))] <- "M2"
# lu$idtom[which(lu$idtom %in% c("5A","5B"))] <- "M5"
# 
# strata@data <- merge(strata@data, lu, by = "label")
# 
# # Now the dissolve
# strata.n <- gUnaryUnion(strata, id = strata@data$idtom)
# plot(strata.n) #M2 and M5 merged
# class(strata.n)
# 
# row.names(strata.n)
# lu <- lu[!duplicated(lu$idtom),]
# row.names(lu) <- lu$idtom
# 
# strata.n <- SpatialPolygonsDataFrame(strata.n, lu)
# writePolyShape(strata.n, "stratan.shp")
# ss <- readOGR(".", "stratan")
# 
# library(raster)
# ss$Area_sqm <- area(ss)
# ss@data
# ss@data$AREA_GEO <- NULL
# ss@data$Area_sqkm <- ss@data$Area_sqm / (1000*1000)
# ss@data$Area_sqnmi<- ss@data$Area_sqkm / (1.852*1.852)
# #putting the correct names
# ss@data$AREA_GEO <- ss@data$Area_sqnmi
# ss@data$Area_sqm <- NULL
# ss@data$Area_sqkm <- NULL
# ss@data$Area_sqnmi <- NULL
# class(ss)
# 
# ss@data$label <- as.character(ss@data$label)
# ss@data$label[10] <- "PELTIC_region2_CLAS"
# ss@data$label[11] <- "PELTIC_region5_CLAS"
# 
# ss@data$OBJECTID <- (1:dim(ss@data))
# ss@data$SP_ID <- NULL
# ss@data$idtom<- NULL
# 
# #save the file
# #write.table(as.data.frame(ss@data),paste("C:/Users/SRC01/Desktop/PELTIC17_sprat_checking/","strata_area.csv"),row.names=FALSE)
# 
# #spliting multpart polygon file to singlepart polygons and saving as shapefiles
# dir.create(path.results.regions,showWarnings = FALSE,recursive=TRUE)
# setwd(path.results.regions)
# 
# #ids <- as.character(unique(ss@data$idtom))
# # for (i in ids){
# # x <- ss@data$idtom==i
# # pol <- ss[x,]
# # proj4string(pol) <- CRS("+proj=longlat +datum=WGS84")
# # plot(pol)
# # writePolyShape(pol, paste("p",i,".shp",sep=''))
# # 
# # }
# 
# ids <- (ss@data$OBJECTID)
# for (i in ids){
#   x <- ss@data$OBJECTID==i
#   pol <- ss[x,]
#   plot(pol)
#   writePolyShape(pol, paste(ss@data$label[i],".shp",sep=''))
#   cat(showWKT(proj4string(pol),paste(ss@data$label[i],".prj",sep=''))) #to have the projections
#   #cat(showWKT(proj4string(pol)),file="prova.prj") 
# }
# 
# #showWKT(proj4string(pol))
# 
# # i=1
# # writePolyShape(pol,"prova.shp")
# # cat(showWKT(proj4string(pol),paste(ss@data$label[i],".prj",sep='')))
# 
# #proj4string(pol) <- CRS("+proj=longlat +datum=WGS84")
# #proj4string(ss) <- CRS("+proj=longlat +datum=WGS84")
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# #+++end of bit added----
# ##code continues###  
  
# 23.0 manual definition: draw polygon around data ESDUs by hand ------
#*********************************************
#Plot catches
# catches.piePlot2(EsduDevi=ESDUDEVs,Pechelsi=Pechels,Pechei=Pechef,legpos='topleft',
#                 export.plot=NULL,radSA=0.05,pradius=0.5,scale.SA=2,xlim=NULL,
#                 ylim=NULL,logit=TRUE,labelit=TRUE,nid='NOCHAL',ux11=FALSE)
#         
# Draw polygons (manually)
# Npolygons=2
# for (i in 1:Npolygons){
#   p=locator(type='l')
#   # #Define projected polygon, add coordinates in nautical miles  
#   polyman=data.frame(ID=i,xcor=p$x,LATF=p$y)
#   polyman$xcornm=60*polyman$xcor
#   polyman$ynm=60*polyman$LATF
#   polyman$LONF=polyman$xcor/mcos
#   # #polygon area
#   psa=data.frame(PID=1,POS=as.numeric(row.names(polyman)),
#                  X=polyman$xcor,Y=polyman$LATF)
#   poly.pbs=as.PolySet(psa)
#   (area1=calcArea(poly.pbs,rollup=2))
#   polyman$area=area1$area
#   if (i==1){polyman.db=polyman
#   }else{
#     polyman.db=rbind(polyman.db,polyman)
#   }
# }



# 23.1. Import region boundaries, defined as distinct .shp files --------
#*********************************************
          #checking just one shapefile
          # a <- readOGR(dsn=path.regions, "PELTIC_region1A_CLAS")
          # b <- readOGR('C:/Users/SRC01/Documents/SILVIA/T3_EchoR/peltic_adapt/results 2017-11-22/Regions3', "p1A")
          # a@data
          # a@data$AREA_GEO

# OR import polygon shapes and set their projection to "longlat"  
regions.import=regionshp.import(path.regions,ESDUDEV=ESDUDEVs,Pechef,keep.projection=F)
list.files(path.regions)

#acoustic and fishing data in shape projection
esdu.sp.shp=regions.import$esdu.sp.shp
peche.sp.shp=regions.import$peche.sp.shp
#acoustic and fishing data in longlat projection
esdu.sp=regions.import$esdu.sp
peche.sp=regions.import$peche.sp
#polygons in longlat projection if keep.projection==FALSE, 
#in orignal shape projection otherwise
polys=regions.import$polys
#esdus in polygons
esdu.inpoly=regions.import$esdu.inpoly
esdu.inpoly$CAMPAGNE=cruise
#trawl hauls in polygons
peche.inpoly=regions.import$peche.inpoly
peche.inpoly$CAMPAGNE=cruise
#Region metadata
lfs.shp=regions.import$lfs.shp 
#select only the regions we want
#lfs.shp <- lfs.shp[lfs.shp$X2%in%c('region1A','region1B','region2','region3A','region3B',
#                                    'region4','region5','region6','region7','region8','region9'),]

    #checking results
    #write.table(lfs.shp,paste("C:/Users/SRC01/Documents/SILVIA/T3_EchoR/peltic_adapt/results 2017-11-22/check/",cruise,"Area_strata_beforemerging.csv",sep=''))
    #write.table(lfs.shp,paste("C:/Users/SRC01/Documents/SILVIA/T3_EchoR/peltic_adapt/results 2017-11-22/check/",cruise,"Area_strata.csv",sep=''))


#CLAS regions polygons and ID
lrb=lfs.shp[lfs.shp$STRATE=='CLAS',]
#we are merging region 2 and 5, so select regions accordingly:
#lrb=lfs.shp[lfs.shp$STRAT=='CLAS'&lfs.shp$X2%in%c('region1A','region1B','region2','region3A','region3B',
#                                                   'region4','region5','region6','region7','region8','region9'),]

lrbs=paste(lrb$STRATE,lrb$ID,sep='.')
lrbn=match(lrbs,lfs.shp$ID2)
polys.CLAS=polys[lrbn]

# #SURF regions polygons and ID (silenced for now, we don't have surface regions)
# lrs=lfs.shp[lfs.shp$STRATE=='SURF',]
# lrss=paste(lrs$STRATE,lrs$ID,sep='.')
# lrsn=match(lrss,lfs.shp$ID2)
# polys.SURF=polys[lrsn]

graphics.off()
#plot esdus, hauls and regions
par(mfrow=c(1,1))
# plot(esdu.sp,cex=.1,main='SURF');plot(peche.sp,add=TRUE,col=2,cex=1)
# for (i in lrsn){
#   plot(polys[[i]],add=TRUE,main='Surface regions')
# }
#coast()
plot(esdu.sp,cex=.1,main='CLAS');plot(peche.sp,add=TRUE,col=2,cex=1)
for (i in lrbn){
  plot(polys[[i]],add=TRUE,main='Bottom regions')
}
coast()

#all regions are ok
plot(polys[[1]],add=T,col="red")#10 ok
plot(polys[[2]],add=T,col="red") # 1a ok
plot(polys[[3]],add=T,col="red") # 1b ok
plot(polys[[4]],add=T,col="red") # 2 ok
plot(polys[[5]],add=T,col="red") # 3a ok
plot(polys[[6]],add=T,col="red") # 3b ok
plot(polys[[7]],add=T,col="red") # 4 ok
plot(polys[[8]],add=T,col="blue") # 5 ok
plot(polys[[9]],add=T,col="red") # 6 ok
plot(polys[[10]],add=T,col="red") # 7a ok
plot(polys[[11]],add=T,col="blue") # 9 ok




# 23.2. Check ESDUs belongings to regions -------
#*********************************************
#ESDUDEVs=ESDUDEV[ESDUDEV$FLAG==1,]
# 23.2.1. Check that all ESDUs are allocated to one region
esdu.naregions=esdu.regions.check(ESDUDEV=ESDUDEVs)
head(ESDUDEVs)

# 23.2.2. Eventually, adds region belongings from shape file to ESDUDEV
esdu.inpoly=esdu.inpoly[order(esdu.inpoly$NESU),]
dim(esdu.inpoly)
dim(ESDUDEVs)
ESDUDEVs=merge(ESDUDEVs,esdu.inpoly[,c('TC','Region.CLAS1','Region.SURF1')], 
              by.x='TC',by.y='TC')# i remove the 1 at the end

summary(ESDUDEVs[1,])#1NA
dim(ESDUDEVs)
head(ESDUDEVs)
ESDUDEVs$zonesCLAS=ESDUDEVs$Region.CLAS1 
ESDUDEVs$zonesSURF=ESDUDEVs$Region.SURF1 

    #remove zonesCLAS and zonesSURF columns (added)
    # ESDUDEVs <- ESDUDEVs[ , -which(names(ESDUDEVs) %in% c("Region.CLAS1"))]
    # ESDUDEVs <- ESDUDEVs[ , -which(names(ESDUDEVs) %in% c("Region.SURF1"))]

#regions need to be only numbers (if you want a plot this, but then you need the original names)
    table(sort(ESDUDEVs$zonesCLAS))
    
# 23.2.3. Eventually, remove areas
#ESDUDEVs$FLAG
# ESDUDEVs[!is.na(ESDUDEVs$zonesSURF)&(ESDUDEVs$zonesSURF=='SURF.3'),'FLAG']=0
# ESDUDEVs=ESDUDEVs[ESDUDEVs$FLAG==1,]
# #Eventually, set coastal surface regions belongings to a dummy region
# ESDUDEVs[ESDUDEVs$depth<50,'zonesSURF']='SURF.99'
# #set offshore regions belongings to a dummy bottom region
# ESDUDEVs[is.na(ESDUDEVs$zonesCLAS)&ESDUDEVs$depth>=145,'zonesCLAS']='CLAS.99'

# Eventually, set coastal surface regions belongings to a dummy region
#ESDUDEVs[ESDUDEVs$depth<45,'zonesSURF']='SURF.99'
# #set offshore regions belongings to a dummy bottom region
#ESDUDEVs[is.na(ESDUDEVs$zonesCLAS)&ESDUDEVs$depth>=145,'zonesCLAS']='CLAS.99'

# 23.2.4.Check that all ESDUs are allocated to one region
esdu.naregions=esdu.regions.check(ESDUDEVs)

# 23.2.5.Eventually, fill missing region belongings with neighbouring belongings
names(ESDUDEVs)
 for (i in 1:dim(ESDUDEVs)[1]){
   if ((i>1)&is.na(ESDUDEVs[i,'zonesCLAS'])){
     ESDUDEVs[i,'zonesCLAS']=ESDUDEVs[i-1,'zonesCLAS']
   }
   if ((i>1)&is.na(ESDUDEVs[i,'zonesSURF'])){
     ESDUDEVs[i,'zonesSURF']=ESDUDEVs[i-1,'zonesSURF']
   }
 }


# 23.2.1 Remove NA from ESDUDEVs (added step)----------- 
# ESDUDEVs <- ESDUDEVs[ , -which(names(ESDUDEVs) %in% c("zonesSURF"))]
# ESDUDEVs <- na.omit(ESDUDEVs) #1 cases deleted now
subset(ESDUDEVs,is.na(zonesCLAS)) #only 1 NA now
#ESDUDEVs <- ESDUDEVs[-1,] #1937 now
ESDUDEVs<-subset(ESDUDEVs,!is.na(zonesCLAS))
###code continues
 
# 23.2.6. Visual check of esdus belongings to regions (seems to be only visual> change the names of the classes to plot it)
# if (unique(nchar(as.character(ESDUDEVs$zonesCLAS)))[1]==1){    
#   rlab.CLAS=as.numeric(as.character(ESDUDEVs$zonesCLAS))  #does not like the letters, check if this is problematic later
#   rlab.SURF=as.numeric(as.character(ESDUDEVs$zonesSURF))
# }else{
#   rlab.CLAS=as.numeric(substr(ESDUDEVs$zonesCLAS,6,7))
#   rlab.SURF=as.numeric(substr(ESDUDEVs$zonesSURF,6,7))
# }
# 
# par(mfrow=c(1,1),mar=c(3,3,3,1))
# plot(ESDUDEVs$LONG,ESDUDEVs$LAT,col=rlab.CLAS,pch=rlab.CLAS,
#      main=paste(cruise,'CLAS'),xlab='',ylab='')
# legend('bottomleft',legend=unique(ESDUDEVs$zonesCLAS),
#        col=unique(rlab.CLAS),pch=unique(rlab.CLAS),bg='white')
# coast()
# points(ESDUDEVs[is.na(ESDUDEVs$zonesCLAS),'LONG'],
#        ESDUDEVs[is.na(ESDUDEVs$zonesCLAS),'LAT'],pch=17,col=2)
# 
# #save plot
# dev.print(device=png,filename=paste(path.results.regions,cruise,"_Strata_col.png",sep=''),width=800,height=800)
# dev.off()

#points(naregions.CLAS$LONG,naregions.CLAS$LAT,pch=12,col=2,cex=2)
 # plot(ESDUDEVs$LONG,ESDUDEVs$LAT,col=rlab.SURF,pch=rlab.SURF,
 #      main=paste(cruise,'SURF'),xlab='',ylab='')
 # legend('bottomleft',legend=unique(ESDUDEVs$zonesSURF),
 #        col=unique(rlab.SURF),pch=unique(rlab.SURF),bg='white')
 # points(ESDUDEVs[is.na(ESDUDEVs$zonesSURF),'LONG'],
 #        ESDUDEVs[is.na(ESDUDEVs$zonesSURF),'LAT'],
 #        pch=substr(ESDUDEVs$zonesSURF,6,6),col=2)
 # points(naregions.SURF$LONG,naregions.SURF$LAT,pch=12,col=2,cex=2)
 # coast()

ESDUDEVs=ESDUDEVs[order(ESDUDEVs$TC),]

# Correct check regions names if needed
if (!'CLAS'%in%ESDUDEVs$zonesCLAS){
  ESDUDEVs$zonesCLAS=paste('CLAS',ESDUDEVs$zonesCLAS,sep='.')
}
# if (!'SURF'%in%ESDUDEVs$zonesSURF){
#   ESDUDEVs$zonesSURF=paste('SURF',ESDUDEVs$zonesSURF,sep='.')
# }

# 23.3. Check  hauls belongings to regions -----------
#********************************************* (need to do step 23.1)
peche.inpoly[peche.inpoly$STRATE=='CLAS'&is.na(peche.inpoly$Region.CLAS),]
peche.inpoly[peche.inpoly$STRATE=='SURF'&is.na(peche.inpoly$Region.SURF),]

length(unique(peche.inpoly$Region.CLAS)) #10 regions

# Eventually, fill gaps in hauls to region belongings
#peche.inpoly[peche.inpoly$STRATE=='CLAS'&is.na(peche.inpoly$Region.CLAS),'Region.CLAS']='CLAS.5'
#peche.inpoly[peche.inpoly$NOCHAL==948,'Region.CLAS']='CLAS.4'

#plot hauls belongings
par(mfrow=c(1,1),bg='white')
peche.inpoly.CLAS=peche.inpoly[peche.inpoly$STRATE=='CLAS',]
#peche.inpoly.SURF=peche.inpoly[peche.inpoly$STRATE=='SURF',]       
plot(peche.inpoly.CLAS$LONF,peche.inpoly.CLAS$LATF,
     col=substr(peche.inpoly.CLAS$Region.CLAS,6,6),pch=16,
     main=paste(cruise,'CLAS hauls'),xlab='',ylab='')
points(peche.inpoly.CLAS[is.na(peche.inpoly.CLAS$Region.CLAS),'LONF'],
       peche.inpoly.CLAS[is.na(peche.inpoly.CLAS$Region.CLAS),'LATF'],pch=17,
       col=2,cex=1)
legend('bottomleft',legend=c(unique(peche.inpoly.CLAS$Region.CLAS)),
       col=c(as.numeric(unique(substr(peche.inpoly.CLAS$Region.CLAS,6,6)))),
       pch=c(rep(16,length(unique(peche.inpoly.CLAS$Region.CLAS)))),bg='white',
       ncol=2)
coast()
# plot(peche.inpoly.SURF$LONF,peche.inpoly.SURF$LATF,
#     col=substr(peche.inpoly.SURF$Region.SURF,6,6),pch=16,
#     main=paste(cruise,'SURF hauls'),xlab='',ylab='')
# points(peche.inpoly.SURF[is.na(peche.inpoly.SURF$Region.SURF),'LONF'],
#        peche.inpoly.SURF[is.na(peche.inpoly.SURF$Region.SURF),'LATF'],pch=17,
#        col=2,cex=1)
# legend('bottomleft',legend=c(unique(peche.inpoly.SURF$Region.SURF)),
#        col=c(as.numeric(unique(substr(peche.inpoly.SURF$Region.SURF,6,6)))),
#        pch=c(rep(16,length(unique(peche.inpoly.SURF$Region.SURF)))),bg='white')
# coast()

# 23.4. Bottom/surface regions definition and area computation -----------
#*********************************************
#Bottom/surface regions definition

deszones=data.frame(CAMPAGNE=cruise,ID=lfs.shp$ID2,ZONE=NA,
                    TYPE=lfs.shp$STRATE,AREA.sp=lfs.shp$AREA.sp,
                    AREA.pbs=lfs.shp$AREA.pbs)
deszones$STRATE=substr(deszones$ID,1,4)
#Total surface of surface regions
colSums(deszones[deszones$STRATE=='SURF',c('AREA.sp','AREA.pbs')])
#Total surface of bottom regions
colSums(deszones[deszones$STRATE=='CLAS',c('AREA.sp','AREA.pbs')])

#*****************************
#valid surfaces are AREA.pbs
#*****************************
deszones$AREA=deszones$AREA.pbs

# Define estimation zone total surface
Asup=mean(aggregate(deszones$AREA,list(deszones$TYPE),sum)[,2])  
Asup=sum(deszones[deszones$STRATE=='CLAS',c('AREA.pbs')])

# 23.5. Compute mean acoustic density per deviation and region bottom/surface ----------
#*********************************************
head(ESDUDEVs)
#remove the duplicated CLAS (don't know why he is dong this...)
ESDUDEVs$zonesCLAS <- sub('CLAS.', '',ESDUDEVs$zonesCLAS)

for (i in 1:length(lyears)){
  #print(lyears[i])
  #print(colMeans(ESDUDEVs[ESDUDEVs$CAMPAGNE==lyears[i],paste('D',lNdev[[i]],sep='')]))
  
  deszonesi=deszones[deszones$CAMPAGNE==lyears[i],]
  mEsduDevi=meanSa.dev.region(EspDevi=EspDev[EspDev$CAMPAGNE==lyears[i],],
                              EsduDevi=ESDUDEVs[ESDUDEVs$CAMPAGNE==lyears[i],],  #Error in `[.data.frame`(deszonesi, , c("ID", "AREA")) : undefined columns selected
                              deszonesi=deszonesi)
  
  mEsduDevi$CAMPAGNE=lyears[i]
  mEsduDevi=list(mEsduDevi)
  names(mEsduDevi)=lyears[i]
  if (i==1) {mEsduDev.db=mEsduDevi
  }else {mEsduDev.db=c(mEsduDev.db,mEsduDevi)}
}  

#write.table(mEsduDevi,paste(path.results.regions,"meanAcousticdensity.csv",sep=''),row.names=F)

#Check
#Mean sA
head(ESDUDEVs)
mEsduDevi
     
aggregate(ESDUDEVs[,paste('D',lNdev[[1]],sep='')],list(ESDUDEVs$zonesCLAS),mean)
aggregate(ESDUDEVs[,paste('D',lNdev[[1]],sep='')],list(ESDUDEVs$zonesSURF),mean)

#Check region surface
aggregate(mEsduDev.db[[1]]$AREA,list(mEsduDev.db[[1]]$D4>0),sum)

# 23.5. Mean sA per echotype/region in dataframe format ------------
#*********************************************
mEsduDev.dbi=mEsduDev.db[[1]]
head(mEsduDev.dbi)
mEsduDev.df=reshape(data=mEsduDev.dbi,
                    varying=list(2:(length(lNdev[[1]])+1)),idvar='Region',
                    direction='long',v.names='sA',
                    times=names(mEsduDev.dbi)[2:(length(lNdev[[1]])+1)],timevar='DEV')

mEsduDevsp.df=merge(mEsduDev.df,EspDevi[,c('CAMPAGNE','DEV','CodEsp',
                                           'GENR_ESP')],by.x=c('CAMPAGNE','DEV'),by.y=c('CAMPAGNE','DEV'))


# 23.6. Plot mean sA per region maps, clupeids ------------
#*********************************************
mEsduDev.df$logsA=log(mEsduDev.df$sA+1)

graphics.off()
scalef=1
#par(mfrow=c(1,2))
par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=mEsduDev.df[mEsduDev.df$DEV=='D2',],cexi=0,
                          scalef=scalef,
                          zlab='logsA',rlab='Region',
                          tmain='Simple average of
                          log D2 NASC',
                          polylab=lrb$ID2,col=3)

#polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=mEsduDev.df[mEsduDev.df$DEV=='D4',],cexi=0,
#                           scalef=scalef,
#                           zlab='logsA',rlab='Region',
#                           tmain='Simple average of
#                           log D4 NASC',
#                           polylab=lrs$ID2,col=3)

# 23.7. Creates CHvalid -----------
#*********************************************
CHvalid=CHvalidit(B.dev.sp.esdu.df,Pechef,ESDUDEVs)

# 23.8. Compute mean Xe per deviation, species:length_cat and region ---------
#*********************************************
# #Re-run association procedure to add region names in list.assoc
# for (k in 1:length(lyears)){
#   EsduDevi=ESDUDEVs[ESDUDEVs$CAMPAGNE==as.character(lyears[k]),]
#   Ndevi=paste('D',c(unlist(lNdev[[k]])),sep='')
#   EspDevi=EspDev[EspDev$CAMPAGNE==as.character(lyears[k]),]
#   Pechei=Pechef[Pechef$CAMPAGNE==as.character(lyears[k]),]
#   
#   cat(as.character(lyears[k]),"\n")
#   list.Associ=lapply(X=seq(length(Ndevi)),FUN=assoc.k.gen,
#                      Ndevi,EsduDevi)
#   names(list.Associ)=paste('D',Ndevi,sep='')
#   if (k==1){
#     list.Assoc.year=list(list.Associ)
#   }else{
#     list.Assoc.year=c(list.Assoc.year,list(list.Associ))
#   }
# }
# 
# list.Assoc.year.near=list.Assoc.year

for (k in 1:length(lyears)){
  EsduDevi=ESDUDEVs[ESDUDEVs$CAMPAGNE==as.character(lyears[k]),]
  Ndevi=paste('D',c(unlist(lNdev[[k]])),sep='')
  EspDevi=EspDev[EspDev$CAMPAGNE==as.character(lyears[k]),]
  Pechei=Pechef[Pechef$CAMPAGNE==as.character(lyears[k]),]
  
  cat(as.character(lyears[k]),"\n")
  list.Associ=lapply(X=seq(length(Ndevi)),FUN=assoc.k.gen,
                     Ndevi,EsduDevi)
  names(list.Associ)=paste('D',Ndevi,sep='')
  if (k==1){
    list.Assoc.year=list(list.Associ)
  }else{
    list.Assoc.year=c(list.Assoc.year,list(list.Associ))
  }
}

list.Assoc.year.ref=list.Assoc.year

#Compute mean Xe
for (i in 1:length(lyears)){
  
  cat(as.character(lyears[i]),'\n')
  list.Xei=list.XeB[[i]]
  list.Associ=list.Assoc.year.ref[[i]]
  EspDevi=EspDev[EspDev$CAMPAGNE==lyears[i],]
  mSa.radiusi=list.mSa.radius.db[[i]]
  Ndevi=lNdev[[i]]
  CH.validi=CHvalid[CHvalid$CAMPAGNE==lyears[i],]
  peche.inpolyi=peche.inpoly[peche.inpoly$CAMPAGNE==lyears[i],]
  
  list.Xe.regionsi=lapply(X=seq(length(lNdev[[i]])),
                          FUN=Xe.k.region,list.Xei=list.Xei,list.Associ=list.Associ,
                          EspDevi=EspDevi,mSa.radiusi=mSa.radiusi,Ndevi=Ndevi,
                          CH.validi=CH.validi,peche.inpoly=peche.inpolyi)
  
  names(list.Xe.regionsi)=paste('D',lNdev[[i]],sep='')
  list.Xe.regionsi=list(list.Xe.regionsi)
  names(list.Xe.regionsi)=lyears[i]  
  if (i==1){list.Xe.regions.years=list.Xe.regionsi
  }else{list.Xe.regions.years=c(list.Xe.regions.years,list.Xe.regionsi)}
}	

#Mean Xe in df format
N0=length(list.Xe.regions.years)
for (j in 1:N0){
  list.Xe.regions.yearsj=list.Xe.regions.years[[j]]
  N=length(list.Xe.regions.yearsj)
  for (i in 1:N){
    Xe.regions.yearsi=data.frame(DEV=paste('D',lNdev[[1]][i],sep=''),
                                 list.Xe.regions.yearsj[[i]]$Xe.res)
    names(Xe.regions.yearsi)[names(Xe.regions.yearsi)%in%c('Region.SURF',
                                                           'Region.CLAS')]='region'
    Xek.regions.yearsi=data.frame(DEV=paste('D',lNdev[[1]][i],sep=''),
                                  list.Xe.regions.yearsj[[i]]$Xek)
    cat(dim(Xek.regions.yearsi),'\n')
    names(Xek.regions.yearsi)[names(Xek.regions.yearsi)%in%c('Region.SURF',
                                                             'Region.CLAS')]='region'
    if (i==1){
      mXe.df=Xe.regions.yearsi
      Xek.df=Xek.regions.yearsi
    }else{
      mXe.df=rbind(mXe.df,Xe.regions.yearsi)
      Xek.df=rbind(Xek.df,Xek.regions.yearsi)
    }  
  }  
}
mXe.df.AN=mXe.df[mXe.df$CodEsp%in%c('ENGR-ENC-CLAS-0','ENGR-ENC-SURF-0','ENGR-ENC-CLAS-G'),]
mXe.df.SA=mXe.df[mXe.df$CodEsp%in%c('SARD-PIL-CLAS-0','SARD-PIL-SURF-0','SARD-PIL-CLAS-G'),]
names(mXe.df.AN)

head(mXe.df)

# 23.9. Plot mean Xes ------------
#********************************************* 
#No. of hauls used to compute mean Xe, anchovy
#*********************************************
names(mXe.df.AN)
par(mfrow=c(1,2),bg='white')
summary(mXe.df.AN$Nhauls)
boxplot(Nhauls~region,mXe.df.AN,ylab='No. of hauls used to compute mean Xe',
        xlab='',main='Anchovy',las=2)
#plot(mXe.df.AN$mXe,mXe.df.AN$Nhauls)
boxplot(mXe~Nhauls,mXe.df.AN,xlab='No. of hauls used to compute mean Xe',
        ylab='Xe simple average',main='Anchovy')

#mean Xe distributions per region, anchovy
#*********************************************
#modified to be only for clas. originally the class bit was:
# boxplot(wmXe~region,mXe.df.AN[!mXe.df.AN$region%in%lrss,],
#         main='Anchovy Xe Weighted average, 
#         CLAS regions' )

par(mfrow=c(1,2))
boxplot(mXe~region,mXe.df.AN[mXe.df.AN$region%in%lrbs,],   
        main='Anchovy Xe simple average, 
        CLAS regions' )
# boxplot(mXe~region,mXe.df.AN[mXe.df.AN$region%in%lrss,],
#         main='Anchovy Xe simple average, 
#         SURF regions' )
boxplot(wmXe~region,mXe.df.AN[mXe.df.AN$region%in%lrbs,],
        main='Anchovy Xe Weighted average, 
        CLAS regions' )
# boxplot(wmXe~region,mXe.df.AN[mXe.df.AN$region%in%lrss,],
#         main='Anchovy Xe Weighted average, 
#         SURF regions' )

summary(mXe.df.AN$mXe)
summary(mXe.df.AN$wmXe)

#mean Xe per region maps, anchovy
#*********************************************
#par(mfrow=c(1,2))
#again modified to be possible for class only
par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=mXe.df.AN[mXe.df.AN$region%in%c(lrb$ID2),],cexi=0,scalef=0.01,
                          zlab='mXe',rlab='region',tmain='Simple average of
                          anchovy CLAS Xe-CLAS only',
                          polylab=lrb$ID2,col=3)
# polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=mXe.df.AN[mXe.df.AN$region%in%c(lrs$ID2),],cexi=0,scalef=0.01,
#                           zlab='mXe',rlab='region',tmain='Simple average of
#                           anchovy SURF Xe',
#                           polylab=lrs$ID2,col=3)

par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=mXe.df.AN[mXe.df.AN$region%in%c(lrb$ID2),],cexi=0,scalef=.01,
                          zlab='wmXe',rlab='region',tmain='Weighed average of
                          anchovy CLAS Xe-CLAS only',
                          polylab=lrb$ID2,col=3)
# polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=mXe.df.AN[mXe.df.AN$region%in%c(lrs$ID2),],cexi=0,scalef=.01,
#                           zlab='wmXe',rlab='region',tmain='Weighed average of
#                           anchovy SURF Xe',
#                           polylab=lrs$ID2,col=3)  

#No. of hauls used to compute mean Xe, sardine
#*********************************************  
par(mfrow=c(1,2))
summary(mXe.df.SA$Nhauls)
boxplot(Nhauls~region,mXe.df.SA,ylab='No. of hauls used to compute mean Xe',
        xlab='region',main='Sardine',las=2)
#plot(mXe.df.AN$mXe,mXe.df.AN$Nhauls)
boxplot(mXe~Nhauls,mXe.df.SA,xlab='No. of hauls used to compute mean Xe',
        ylab='Xe simple average',main='Sardine')

#mean Xe distributions per region, sardine
#*********************************************
#also modified to work with clas only
par(mfrow=c(1,2))
boxplot(mXe~region,mXe.df.SA[mXe.df.SA$region%in%lrbs,],
        main='Sardine Xe simple average, 
        CLAS regions' )
# boxplot(mXe~region,mXe.df.SA[mXe.df.SA$region%in%lrss,],
#         main='Sardine Xe simple average, 
#         SURF regions' )
boxplot(wmXe~region,mXe.df.SA[mXe.df.SA$region%in%lrbs,],
        main='Sardine Xe Weighted average, 
        CLAS regions' )
# boxplot(wmXe~region,mXe.df.SA[mXe.df.SA$region%in%lrss,],
#         main='Sardine Xe Weighted average, 
#         SURF regions' )


summary(mXe.df.SA$mXe)
summary(mXe.df.SA$wmXe)

#mean Xe per region maps, sardine
#*********************************************
#also modified to be for clas only
par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=mXe.df.SA[mXe.df.SA$region%in%c(lrb$ID2),],cexi=0,scalef=0.01,
                          zlab='mXe',rlab='region',tmain='Simple average of
                          sardine CLAS Xe-CLAS only',
                          polylab=lrb$ID2,col=4)
# polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=mXe.df.SA[mXe.df.SA$region%in%c(lrs$ID2),],cexi=0,scalef=.01,
#                           zlab='mXe',rlab='region',tmain='Simple average of
#                           sardine SURF Xe',
#                           polylab=lrs$ID2,col=4)

par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=mXe.df.SA[mXe.df.SA$region%in%c(lrb$ID2),],cexi=0,scalef=.01,
                          zlab='wmXe',rlab='region',tmain='Weighed average of
                          sardine CLAS Xe-CLAS only',
                          polylab=lrb$ID2,col=4)
# polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=mXe.df.SA[mXe.df.SA$region%in%c(lrs$ID2),],cexi=0,scalef=.01,
#                           zlab='wmXe',rlab='region',tmain='Weighed average of
#                           sardine SURF Xe',
#                           polylab=lrs$ID2,col=4)  
par(mfrow=c(1,1))

# 23.10. Biomass per species:length_cat, deviation and region -------
#*********************************************
#Dataframe style
#*********************************************
#Check mean sA and Xe dataframes
mEsduDevsp.df$ID=paste(mEsduDevsp.df$DEV,mEsduDevsp.df$Region,
                       mEsduDevsp.df$CodEsp)
length(mEsduDevsp.df$ID);length(unique(mEsduDevsp.df$ID))  
mXe.df$ID=paste(mXe.df$DEV,mXe.df$region,mXe.df$CodEsp)  
length(mXe.df$ID);length(unique(mXe.df$ID))
difID=mEsduDevsp.df$ID[!mEsduDevsp.df$ID%in%mXe.df$ID]
names(mXe.df)
#head(mEsduDevsp.df)
dim(mEsduDevsp.df)
dim(mXe.df)
head(mXe.df)
#Merge mean sA and Xe dataframes 
mEsduDevsp.mXe.df=merge(mEsduDevsp.df,mXe.df[,c('DEV','region','CodEsp',
                                                'wmXe','wmXes','mXe','mXes','Nhauls','mweight','wmweight')],by.x=c('DEV','Region','CodEsp'),
                        by.y=c('DEV','region','CodEsp'))
dim(mEsduDevsp.mXe.df)
head(mEsduDevsp.mXe.df) 
#Compute biomass based on Xe simple average 
mEsduDevsp.mXe.df$biom=mEsduDevsp.mXe.df$sA*mEsduDevsp.mXe.df$mXe*
  mEsduDevsp.mXe.df$AREA
aggregate(mEsduDevsp.mXe.df$biom,list(mEsduDevsp.mXe.df$GENR_ESP),sum)  
mEsduDevsp.mXe.df$bioms=mEsduDevsp.mXe.df$sA*mEsduDevsp.mXe.df$mXes*
  mEsduDevsp.mXe.df$AREA  
aggregate(mEsduDevsp.mXe.df$bioms,list(mEsduDevsp.mXe.df$GENR_ESP),sum)
#Compute biomass based on Xe weighted average
mEsduDevsp.mXe.df$wbiom=mEsduDevsp.mXe.df$sA*mEsduDevsp.mXe.df$wmXe*
  mEsduDevsp.mXe.df$AREA
aggregate(mEsduDevsp.mXe.df$wbiom,list(mEsduDevsp.mXe.df$GENR_ESP),sum)  
mEsduDevsp.mXe.df$wbioms=mEsduDevsp.mXe.df$sA*mEsduDevsp.mXe.df$wmXes*
  mEsduDevsp.mXe.df$AREA  
aggregate(mEsduDevsp.mXe.df$wbioms,list(mEsduDevsp.mXe.df$GENR_ESP),sum)
head(mEsduDevsp.mXe.df)
#Compute abundances
head(mEsduDevsp.mXe.df)
mEsduDevsp.mXe.df$abun=mEsduDevsp.mXe.df$biom/mEsduDevsp.mXe.df$mweight
mEsduDevsp.mXe.df$wabun=mEsduDevsp.mXe.df$wbiom/mEsduDevsp.mXe.df$wmweight
plot(mEsduDevsp.mXe.df$abun,mEsduDevsp.mXe.df$wabun)

#Compute biomass based on list data
#*********************************************
for (i in 1:length(lyears)){
  Ndevi=lNdev[[i]]
  list.Xe.regionsi=list.Xe.regions.years[[i]]
  mEsduDevi=mEsduDev.db[[i]]
  EspDevi=EspDev[EspDev$CAMPAGNE==lyears[i],]
  cat(as.character(lyears[i]),'\n')
  BiomDev.regionsi=lapply(X=seq(length(Ndevi)),FUN=Biom.Dev.region,
                          list.Xe.regionsi=list.Xe.regionsi,mEsduDevi=mEsduDevi,
                          Ndevi=Ndevi,EspDevi=EspDevi)
  names(BiomDev.regionsi)=paste('D',lNdev[[i]],sep='')
  BiomDev.regionsi=list(BiomDev.regionsi)
  names(BiomDev.regionsi)=lyears[i]    
  if (i==1){BiomDev.regions.years=BiomDev.regionsi
  }else{BiomDev.regions.years=c(BiomDev.regions.years,BiomDev.regionsi)}
}    

#If DX=0 and wmXe=0 or wmXes=0: no echotype X SA in region,
#then weighted Xe is null
#Else if DX!=0 and wmXes=0: there was echotype X SA in the region,
#but the 'expert' decided to discard it 

#  23.11. Biomass per species and region -----
#*********************************************
# In dataframe format
#*********************************************
#Region areas
Biom.region.areas=aggregate(mEsduDevsp.mXe.df[,c('AREA')],
                            list(mEsduDevsp.mXe.df[,'CAMPAGNE'],
                                 mEsduDevsp.mXe.df[,'Region']),unique)
names(Biom.region.areas)=c('CAMPAGNE','Region','area')
Biom.region.areas$STRATE=substr(Biom.region.areas$Region,1,4)
#Biom.region.areas$area=Biom.region.areas$area*(60^2)
Biom.region.areas.tot=sum(Biom.region.areas$area)/2
#check surface regions area
sum(Biom.region.areas[Biom.region.areas$STRATE=='SURF','area'])
#check bottom regions area
sum(Biom.region.areas[Biom.region.areas$STRATE=='CLAS','area'])

#Biomass per codesp and region
head(mEsduDevsp.mXe.df)

Biom.region.codesp.df=aggregate(mEsduDevsp.mXe.df[,c('biom','bioms',
                                                     'wbiom','wbioms','abun','wabun')],
                                list(mEsduDevsp.mXe.df$CAMPAGNE,mEsduDevsp.mXe.df$Region,
                                     mEsduDevsp.mXe.df$GENR_ESP,mEsduDevsp.mXe.df$CodEsp),
                                sum)  
names(Biom.region.codesp.df)=c('CAMPAGNE','Region','GENR_ESP','CodEsp',
                               'biom','bioms','wbiom','wbioms','abun','wabun')
#Biomass per sp and region    
Biom.region.sp.df=aggregate(mEsduDevsp.mXe.df[,c('biom','bioms',
                                                 'wbiom','wbioms','abun','wabun')],
                            list(mEsduDevsp.mXe.df$CAMPAGNE,mEsduDevsp.mXe.df$Region,
                                 mEsduDevsp.mXe.df$GENR_ESP),sum)  
names(Biom.region.sp.df)=c('CAMPAGNE','Region','GENR_ESP','biom','bioms',
                           'wbiom','wbioms','abun','wabun')
#Biomass per sp    
Biom.sp.df=aggregate(mEsduDevsp.mXe.df[,c('biom','bioms',
                                          'wbiom','wbioms','abun','wabun')],
                     list(mEsduDevsp.mXe.df$CAMPAGNE,mEsduDevsp.mXe.df$GENR_ESP),sum)  
names(Biom.sp.df)=c('CAMPAGNE','GENR_ESP','biom','bioms',
                    'wbiom','wbioms','abun','wabun')  

#Biomass per csp    
Biom.csp.df=aggregate(mEsduDevsp.mXe.df[,c('biom','bioms',
                                           'wbiom','wbioms','abun','wabun')],
                      list(mEsduDevsp.mXe.df$CAMPAGNE,mEsduDevsp.mXe.df$CodEsp),sum)  
names(Biom.csp.df)=c('CAMPAGNE','CodEsp','biom','bioms',
                     'wbiom','wbioms','abun','wabun')

# In list format
#****************************
for (k in 1:length(lyears)){
  Ndevi=lNdev[[k]]
  BiomDev.regionsi=BiomDev.regions.years[[k]]
  Deszonesi=deszones[deszones$CAMPAGNE==lyears[k],]
  EspDevi=EspDev[EspDev$CAMPAGNE==lyears[k],]
  cat(as.character(lyears[k]),'\n')
  
  Biom.regionsi=Biom.region(BiomDev.regionsi=BiomDev.regionsi,Ndevi=Ndevi,
                            EspDevi=EspDevi,Deszonesi=Deszonesi)
  names(Biom.regionsi)
  Biom.spi=data.frame(cruise=lyears[k],Biom.regionsi$Biom.sp.ALL)
  Biom.region.codespi=data.frame(cruise=lyears[k],
                                 Biom.regionsi$Biom.region.db.ALL)
  Biom.region.spi=data.frame(cruise=lyears[k],
                             Biom.regionsi$Biom.sp.region.ALL)
  
  if (k==1){
    BiomDev.regions.db=BiomDev.regionsi
    Biom.sp.db=Biom.spi
    Biom.region.codesp=Biom.region.codespi
    Biom.region.sp=Biom.region.spi
  }else{
    BiomDev.regions.db=rbind(BiomDev.regions.db,BiomDev.regionsi)
    Biom.sp.db=rbind(Biom.sp.db,Biom.spi)
    Biom.region.codesp=rbind(Biom.region.codesp,Biom.region.codespi)
    Biom.region.sp=rbind(Biom.region.sp,Biom.region.spi)
  }  
}
head(Biom.sp.db)

#  23.12. Merge with results per ESDU --------
#*********************************************
head(Biom.sp.db)
head(biomres.sp.esdu)
Biom.sp.ALL=merge(Biom.sp.db,biomres.sp.esdu,by.x='sp',by.y='sp')
Biom.sp.ALL$averageB=rowMeans(Biom.sp.ALL[,c('biom','wbiom','biomW.esdu')])
Biom.sp.ALL$sdB=apply(Biom.sp.ALL[,c('biom','wbiom','biomW.esdu')],1,sd)
Biom.sp.ALL[,c('cruise','sp','biom','wbiom','biomW.esdu')]

# 23.13. Export results per species ---------
#*********************************************

write.table(Biom.sp.ALL,
            paste(path.results.regions,cruise,'_Biom.sp.ALL.csv',
                  sep=''),sep=';',row.names=FALSE)
write.table(Biom.region.sp.df,
            paste(path.results.regions,cruise,'_Biom.region.sp.df.csv',
                  sep=''),sep=';',row.names=FALSE)

# 25. Biomass per region estimates and plots ----------
#*********************************************
#25.a) Anchovy-----
#*********************************************                              
Biom.region.dev.sp.AN=mEsduDevsp.mXe.df[mEsduDevsp.mXe.df$CodEsp=='ENGR-ENC-CLAS-0',]
#Biom.region.dev.sp.ANs=mEsduDevsp.mXe.df[mEsduDevsp.mXe.df$sA!=0,] # i did silence that
Biom.region.sp.AN=Biom.region.sp[Biom.region.sp$sp=='ENGR-ENC',]

Biom.region.sp.AN=merge(Biom.region.sp.AN,mEsduDev.db[[1]][,c('Region','AREA')],
                        by.x='Region',by.y='Region')
Biom.region.sp.AN$mB=Biom.region.sp.AN$wbioms/Biom.region.sp.AN$AREA

#Biomass per region bubbleplots
par(mfrow=c(1,1)) ##need to change scalef accordingly
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=Biom.region.sp.AN[Biom.region.sp.AN$Region%in%lrb$ID2,],
                          cexi=0.1,scalef=2000,zlab='wbiom',rlab='Region',
                          tmain='Weighted average of
                          anchovy CLAS biomass',polylab=lrb$ID2,col=3)
# polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=Biom.region.sp.AN[Biom.region.sp.AN$Region%in%lrs$ID2,],cexi=0.1,
#                           scalef=2e4,zlab='wbiom',rlab='Region',tmain='Weighted average of
#                           anchovy SURF biomass',
#                           polylab=lrs$ID,col=3)


#save plot
dev.print(device=png,filename=paste(path.results.regions,cruise,"_ANE_Wbiomsaclef2000.png",sep=''),width=800,height=800)
dev.off()

#25.b) Sardine----
#*********************************************                            
#Biom.region.dev.sp.SA=mEsduDevsp.mXe.df[mEsduDevsp.mXe.df$CodEsp=='SARD-PIL-CLAS-0',]

Biom.region.dev.sp.SA=mEsduDevsp.mXe.df[mEsduDevsp.mXe.df$CodEsp %in%c('SARD-PIL-CLAS-0','SARD-PIL-CLAS-G'),]

Biom.region.sp.SA=Biom.region.sp[Biom.region.sp$sp=='SARD-PIL',]
Biom.region.sp.SA=merge(Biom.region.sp.SA,mEsduDev.db[[1]][,c('Region','AREA')],by.x='Region',by.y='Region')
Biom.region.sp.SA$mB=Biom.region.sp.SA$wbioms/Biom.region.sp.SA$AREA

#Biomass per region bubbleplots
par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=Biom.region.sp.SA[Biom.region.sp.SA$Region%in%lrb$ID2,],
                          cexi=0.1,scalef=8000,zlab='wbiom',rlab='Region',
                          tmain='Weighted average of
                          sardine CLAS biomass',polylab=lrb$ID2,col=4)
# polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=Biom.region.sp.SA[Biom.region.sp.SA$Region%in%lrs$ID,],cexi=0.1,
#                           scalef=20000,zlab='wbiom',rlab='Region',tmain='Weighted average of
#                           sardine SURF biomass',
#                           polylab=lrs$ID2,col=4)

#save plot
dev.print(device=png,filename=paste(path.results.regions,cruise,"_PIL_Wbiom-scalef8000.png",sep=''),width=800,height=800)
dev.off()

#25.c) Sprat----
#*********************************************                            
#Biom.region.dev.sp.SPR=mEsduDevsp.mXe.df[mEsduDevsp.mXe.df$CodEsp=='SPRA-SPR-CLAS-0',]
Biom.region.dev.sp.SPR=mEsduDevsp.mXe.df[mEsduDevsp.mXe.df$CodEsp %in%c('SPRA-SPR-CLAS-0','SPRA-SPR-CLAS-G'),]

Biom.region.sp.SPR=Biom.region.sp[Biom.region.sp$sp=='SPRA-SPR',]

Biom.region.sp.SPR=merge(Biom.region.sp.SPR,mEsduDev.db[[1]][,c('Region','AREA')],
                        by.x='Region',by.y='Region')
Biom.region.sp.SPR$mB=Biom.region.sp.SPR$wbioms/Biom.region.sp.SPR$AREA


#Biomass per region bubbleplots
par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=Biom.region.sp.SPR[Biom.region.sp.SPR$Region%in%lrb$ID2,],
                          cexi=0.1,scalef=2000,zlab='wbiom',rlab='Region',
                          tmain='Weighted average of
                          sprat CLAS biomass',polylab=lrb$ID2,col=1)

#save plot
dev.print(device=png,filename=paste(path.results.regions,cruise,"_SPR_Wbiom-scalef2000.png",sep=''),width=800,height=800)
dev.off()


#25.d) Horse mackerel----
#*********************************************                            
#Biom.region.dev.sp.HOM=mEsduDevsp.mXe.df[mEsduDevsp.mXe.df$CodEsp=='TRAC-TRA-CLAS-0',]
Biom.region.dev.sp.HOM=mEsduDevsp.mXe.df[mEsduDevsp.mXe.df$CodEsp %in%c('TRAC-TRA-CLAS-0','TRAC-TRA-CLAS-G'),]
Biom.region.sp.HOM=Biom.region.sp[Biom.region.sp$sp=='TRAC-TRA',]

Biom.region.sp.HOM=merge(Biom.region.sp.HOM,mEsduDev.db[[1]][,c('Region','AREA')],
                         by.x='Region',by.y='Region')
Biom.region.sp.HOM$mB=Biom.region.sp.HOM$wbioms/Biom.region.sp.HOM$AREA


#Biomass per region bubbleplots
par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=Biom.region.sp.HOM[Biom.region.sp.SPR$Region%in%lrb$ID2,],
                          cexi=0.1,scalef=1000,zlab='wbiom',rlab='Region',
                          tmain='Weighted average of
                          horse mackerel CLAS biomass',polylab=lrb$ID2,col=7)


#save plot
dev.print(device=png,filename=paste(path.results.regions,cruise,"_HOM_Wbiom-scalef1000.png",sep=''),width=800,height=800)
dev.off()


#25.e) Boarfish----
#*********************************************                            
#Biom.region.dev.sp.BOF=mEsduDevsp.mXe.df[mEsduDevsp.mXe.df$CodEsp=='CAPR-APE-CLAS-0',]
Biom.region.dev.sp.BOF=mEsduDevsp.mXe.df[mEsduDevsp.mXe.df$CodEsp %in%c('CAPR-APE-CLAS-0','CAPR-APE-CLAS-G'),]

Biom.region.sp.BOF=Biom.region.sp[Biom.region.sp$sp=='CAPR-APE',]

Biom.region.sp.BOF=merge(Biom.region.sp.BOF,mEsduDev.db[[1]][,c('Region','AREA')],
                          by.x='Region',by.y='Region')
Biom.region.sp.BOF$mB=Biom.region.sp.BOF$wbioms/Biom.region.sp.BOF$AREA


#Biomass per region bubbleplots
par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=Biom.region.sp.BOF[Biom.region.sp.BOF$Region%in%lrb$ID2,],
                          cexi=0.1,scalef=300,zlab='wbiom',rlab='Region',
                          tmain='Weighted average of
                          boarfish CLAS biomass',polylab=lrb$ID2,col=6)

#save plot
dev.print(device=png,filename=paste(path.results.regions,cruise,"_BOF_Wbiom-scalef300.png",sep=''),width=800,height=800)
dev.off()



# 26. Anchovy and sardine biomass estimates comparisons (extended to sprat and horse mackerel) -----
#*********************************************  
#Biomass estimates comparison
#*********************************************
par(mfrow=c(2,2))
#ANE
par(mar=c(5,6,3,1))  
bp1=barplot(as.matrix(Biom.sp.ALL[Biom.sp.ALL$sp=='ENGR-ENC',
                                  c('biom','wbiom','biomW.esdu','averageB')]),
            names.arg=names(Biom.sp.ALL$sp),horiz=TRUE,beside=TRUE,las=2,
            main='Anchovy biomass estimates',
            xlim=c(0,Biom.sp.ALL[Biom.sp.ALL$sp=='ENGR-ENC','averageB']+2*
                     Biom.sp.ALL[Biom.sp.ALL$sp=='ENGR-ENC','sdB']),
            col=c('darkgreen','palegreen','seagreen','green'))
arrows(Biom.sp.ALL[Biom.sp.ALL$sp=='ENGR-ENC','averageB']-1.96*
         Biom.sp.ALL[Biom.sp.ALL$sp=='ENGR-ENC','sdB'],bp1[4],
       Biom.sp.ALL[Biom.sp.ALL$sp=='ENGR-ENC','averageB']+1.96*
         Biom.sp.ALL[Biom.sp.ALL$sp=='ENGR-ENC','sdB'],bp1[4],code=3,angle=90)

#PIL
par(mar=c(5,6,3,1))  
bp1=barplot(as.matrix(Biom.sp.ALL[Biom.sp.ALL$sp=='SARD-PIL',
                                  c('biom','wbiom','biomW.esdu','averageB')]),
            names.arg=names(Biom.sp.ALL$sp),horiz=TRUE,beside=TRUE,las=2,
            main='Sardine biomass estimates',
            xlim=c(0,Biom.sp.ALL[Biom.sp.ALL$sp=='SARD-PIL','averageB']+2*
                     Biom.sp.ALL[Biom.sp.ALL$sp=='SARD-PIL','sdB']),
            col=c('darkblue','lightblue','steelblue','blue'))
arrows(Biom.sp.ALL[Biom.sp.ALL$sp=='SARD-PIL','averageB']-1.96*
         Biom.sp.ALL[Biom.sp.ALL$sp=='SARD-PIL','sdB'],bp1[4],
       Biom.sp.ALL[Biom.sp.ALL$sp=='SARD-PIL','averageB']+1.96*
         Biom.sp.ALL[Biom.sp.ALL$sp=='SARD-PIL','sdB'],bp1[4],code=3,angle=90)


#SPR
bp1=barplot(as.matrix(Biom.sp.ALL[Biom.sp.ALL$sp=='SPRA-SPR',
                                  c('biom','wbiom','biomW.esdu','averageB')]),
            names.arg=names(Biom.sp.ALL$sp),horiz=TRUE,beside=TRUE,las=2,
            main='Sprat biomass estimates',
            xlim=c(0,Biom.sp.ALL[Biom.sp.ALL$sp=='SPRA-SPR','averageB']+2*
                     Biom.sp.ALL[Biom.sp.ALL$sp=='SPRA-SPR','sdB']),
            col=c('grey50','grey70','grey90','grey'))
arrows(Biom.sp.ALL[Biom.sp.ALL$sp=='SPRA-SPR','averageB']-1.96*
         Biom.sp.ALL[Biom.sp.ALL$sp=='SPRA-SPR','sdB'],bp1[4],
       Biom.sp.ALL[Biom.sp.ALL$sp=='SPRA-SPR','averageB']+1.96*
         Biom.sp.ALL[Biom.sp.ALL$sp=='SPRA-SPR','sdB'],bp1[4],code=3,angle=90)

#HOM
bp1=barplot(as.matrix(Biom.sp.ALL[Biom.sp.ALL$sp=='TRAC-TRA',
                                  c('biom','wbiom','biomW.esdu','averageB')]),
            names.arg=names(Biom.sp.ALL$sp),horiz=TRUE,beside=TRUE,las=2,
            main='Horse mackerel biomass estimates',
            xlim=c(0,Biom.sp.ALL[Biom.sp.ALL$sp=='TRAC-TRA','averageB']+2*
                     Biom.sp.ALL[Biom.sp.ALL$sp=='TRAC-TRA','sdB']),
            col=c('darkgoldenrod1','darkgoldenrod2','darkgoldenrod3','darkgoldenrod4'))
arrows(Biom.sp.ALL[Biom.sp.ALL$sp=='TRAC-TRA','averageB']-1.96*
         Biom.sp.ALL[Biom.sp.ALL$sp=='TRAC-TRA','sdB'],bp1[4],
       Biom.sp.ALL[Biom.sp.ALL$sp=='TRAC-TRA','averageB']+1.96*
         Biom.sp.ALL[Biom.sp.ALL$sp=='TRAC-TRA','sdB'],bp1[4],code=3,angle=90)

title(main="2018",outer=T)

#save plot
dev.print(device=png,filename=paste(path.results.regions,cruise,"_Bestimates-comparison.png",sep=''),width=800,height=800)
dev.off()
#*******************************************************************************************************