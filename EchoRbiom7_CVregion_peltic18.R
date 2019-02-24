#*******************************************************************************************************
# STEP 7: ESTIMATION ERROR PER REGION: COMPUTE ESTIMATION ERROR PER REGION AND SPECIES
#*******************************************************************************************************
# Author: Mathieu Doray mathieu.doray@ifremer.fr

#Load package
#*************************************
library(EchoR)

path.results.cv=paste(path.results,'cv/',sep='')
dir.create(path.results.cv,showWarnings = FALSE,recursive=TRUE)


# 24. Compute estimation error ---------------
#*************************************
# 24.1. Compute estimation variances per regions, echotype ---------- 
#       and species categories
#*************************************
CV.df=EVA(Xek.df=Xek.df,ESDUDEV=ESDUDEVs,deszones=deszones)

#giving an error in the formats, remove factors
str(Xek.df)
Xek.df$DEV <- as.character(Xek.df$DEV)
str(ESDUDEVs)
ESDUDEVs$TC <- as.character(ESDUDEVs$TC)
ESDUDEVs$CAMPAGNE <- as.character(ESDUDEVs$CAMPAGNE)
str(deszones)
deszones$CAMPAGNE <- as.character(deszones$CAMPAGNE)
deszones$ID <- as.character(deszones$ID)
deszones$TYPE <- as.character(deszones$TYPE)
######
head(CV.df)

#24.1 a) Barplots: proportion of total estimation variance per species, echotypes and regions:-----
#graphics.off()
#x11()
dfs=EVA.barplot2(df=CV.df,anames='DEVregion',
                args.legend = list(x = "bottomright",text.width=.12),
                xlab='Proportion of total estimation variance',mar=c(5,6,1,1))
#savePlot(paste(path.results.regions,cruise,'propTotVariance_EchotypeRegion.png'))

dev.print(device=png,filename=paste(path.results.cv,cruise,"_propTotVariance_EchotypeRegion(2).png",sep=''),width=1000,height=800)
dev.off()


#24.1 b) Barplots: identification variance/total estimation variance per species, echotypes and regions:----  
#graphics.off()
#x11()
dfs=EVA.barplot2(df=CV.df,anames='DEVregion',
                vnames=c('prop.varai','prop.varwi'),
                args.legend = list(x = "topright",text.width=.12),mar=c(5,6,1,1),
                xlab='Identification variance/total estimation variance')

#savePlot(paste(path.results.regions,cruise,'IdVarTotVarRatio_EchotypeRegion.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_IdVarTotVarRatio_EchotypeRegion(2).png",sep=''),width=800,height=800)
dev.off()


# 24.2. Estimation variance and CV per species category and region -----------
# REM: within a given region, fish densities must be added 
# to compute total density
#************************************* 
sigmaE2tots=aggregate(CV.df[,c('sigmaE2tot','wsigmaE2tot')],
                      list(CV.df$sp),sum,na.rm=TRUE)
names(sigmaE2tots)=c('sp','sigmaE2tot','wsigmaE2tot')

sigmaE2tots2=aggregate(CV.df[,c('sigmaE2tot','wsigmaE2tot')],
                      list(CV.df$CodEsp),sum,na.rm=TRUE)
names(sigmaE2tots2)=c('CodEsp','sigmaE2tot','wsigmaE2tot')

# CV per species and regions
CV.region.sp=aggregate(CV.df[,c('mB','mwB','mB1','mwB1','ew','ewi','ews',
                                'ea','eai','eas','wea','weai','weas',
                                'wew','wewi','wews')],
                       list(CV.df$sp,CV.df$region),sum,na.rm=TRUE)
names(CV.region.sp)=c('sp','region','mB','mwB','mB1','mwB1','wsigmaE2',
                      'wsigmaE2i','wsigmaE2s','sigmaE2','sigmaE2i',
                      'sigmaE2s','wsigmaE21','wsigmaE21i','wsigmaE21s',
                      'sigmaE21','sigmaE21i','sigmaE21s')
CV.region.sp$CV=sqrt(CV.region.sp$sigmaE2)/CV.region.sp$mB
CV.region.sp$CVi=sqrt(CV.region.sp$sigmaE2i)/CV.region.sp$mB
CV.region.sp$CVs=sqrt(CV.region.sp$sigmaE2s)/CV.region.sp$mB
CV.region.sp$wCV=sqrt(CV.region.sp$wsigmaE2)/CV.region.sp$mwB
CV.region.sp$wCVi=sqrt(CV.region.sp$wsigmaE2i)/CV.region.sp$mwB
CV.region.sp$wCVs=sqrt(CV.region.sp$wsigmaE2s)/CV.region.sp$mwB
CV.region.sp=merge(CV.region.sp,sigmaE2tots,by.x='sp',by.y='sp')
CV.region.sp$prop.vara=CV.region.sp$sigmaE21/CV.region.sp$sigmaE2tot
CV.region.sp$prop.varai=CV.region.sp$sigmaE21i/CV.region.sp$sigmaE2tot
CV.region.sp$prop.varas=CV.region.sp$sigmaE21s/CV.region.sp$sigmaE2tot
CV.region.sp$prop.varw=CV.region.sp$wsigmaE21/CV.region.sp$wsigmaE2tot
CV.region.sp$prop.varwi=CV.region.sp$wsigmaE21i/CV.region.sp$wsigmaE2tot
CV.region.sp$prop.varws=CV.region.sp$wsigmaE21s/CV.region.sp$wsigmaE2tot
head(CV.region.sp)

#save file
dim(CV.region.sp)
write.table(CV.region.sp,paste(path.results.cv,cruise,'_CVspeciesRegion.csv',sep=""),sep=";",row.names=F)


# CV per species code and regions
CV.region.CodEsp=aggregate(CV.df[,c('mB','mwB','mB1','mwB1','ew','ewi','ews',
                                'ea','eai','eas','wea','weai','weas',
                                'wew','wewi','wews')],
                       list(CV.df$CodEsp,CV.df$region),sum,na.rm=TRUE)
names(CV.region.CodEsp)=c('CodEsp','region','mB','mwB','mB1','mwB1','wsigmaE2',
                      'wsigmaE2i','wsigmaE2s','sigmaE2','sigmaE2i',
                      'sigmaE2s','wsigmaE21','wsigmaE21i','wsigmaE21s',
                      'sigmaE21','sigmaE21i','sigmaE21s')
CV.region.CodEsp$CV=sqrt(CV.region.CodEsp$sigmaE2)/CV.region.CodEsp$mB
CV.region.CodEsp$CVi=sqrt(CV.region.CodEsp$sigmaE2i)/CV.region.CodEsp$mB
CV.region.CodEsp$CVs=sqrt(CV.region.CodEsp$sigmaE2s)/CV.region.CodEsp$mB
CV.region.CodEsp$wCV=sqrt(CV.region.CodEsp$wsigmaE2)/CV.region.CodEsp$mwB
CV.region.CodEsp$wCVi=sqrt(CV.region.CodEsp$wsigmaE2i)/CV.region.CodEsp$mwB
CV.region.CodEsp$wCVs=sqrt(CV.region.CodEsp$wsigmaE2s)/CV.region.CodEsp$mwB
CV.region.CodEsp=merge(CV.region.CodEsp,sigmaE2tots2,by.x='CodEsp',by.y='CodEsp')
CV.region.CodEsp$prop.vara=CV.region.CodEsp$sigmaE21/CV.region.CodEsp$sigmaE2tot
CV.region.CodEsp$prop.varai=CV.region.CodEsp$sigmaE21i/CV.region.CodEsp$sigmaE2tot
CV.region.CodEsp$prop.varas=CV.region.CodEsp$sigmaE21s/CV.region.CodEsp$sigmaE2tot
CV.region.CodEsp$prop.varw=CV.region.CodEsp$wsigmaE21/CV.region.CodEsp$wsigmaE2tot
CV.region.CodEsp$prop.varwi=CV.region.CodEsp$wsigmaE21i/CV.region.CodEsp$wsigmaE2tot
CV.region.CodEsp$prop.varws=CV.region.CodEsp$wsigmaE21s/CV.region.CodEsp$wsigmaE2tot
head(CV.region.CodEsp)

#save file
dim(CV.region.CodEsp)
write.table(CV.region.CodEsp,paste(path.results.cv,cruise,'_CVspeciesCodeRegion.csv',sep=""),sep=";",row.names=F)


#24.2 a) Barplots: proportion of total EVA per species and regions:-----
#graphics.off()
#x11()
dfs=EVA.barplot2(df=CV.region.sp,anames='region',
                args.legend = list(x = "bottomright",text.width=.02),mar=c(5,5,1,1),
                xlab=c('Proportion of total estimation variance'),ylab='')
#savePlot(paste(path.results.regions,cruise,'propTotVariance_Region.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_propTotVariance_Region(2).png",sep=''),width=800,height=800)
dev.off()


#24.2 b) Barplots: proportion of identification EVA per species and regions:----
#x11()
dfs=EVA.barplot2(df=CV.region.sp,anames='region',
                vnames=c('prop.varai','prop.varwi'),
                args.legend = list(x = "bottomright",text.width=.01),mar=c(5,5,1,1),
                xlab=c('Identification variance/total estimation variance ratio'),ylab='Region')
#savePlot(paste(path.results.regions,cruise,'IdVarTotVarRatio_Region.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_IdVarTotVarRatio_Region(2).png",sep=''),width=800,height=800)
dev.off()


# 24.3. Plot polygon.bubbleplot ----------- 
# showing the proportion of total EVA per species and regions: 
#*************************************
#24.3 a) anchovy----
#*************************************
#modified to work only with CLAS
dfs.AN=dfs[dfs$sp=='ENGR-ENC',]                            
#graphics.off()                              
par(mfrow=c(1,2))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=dfs.AN[dfs.AN$region%in%lrb$ID2,],cexi=0.1,scalef=0.05,
                          zlab='prop.vara',rlab='region',tmain='Proportion of total estimation variance
                          Simple average of anchovy CLAS Xe',
                          polylab=lrb$ID2,col=3)
# polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=dfs.AN[dfs.AN$region%in%lrs$ID2,],cexi=0.1,scalef=0.05,
#                           zlab='prop.vara',rlab='region',tmain='Proportion of total estimation variance
#                           Simple average of anchovy SURF Xe',
#                           polylab=lrs$ID2,col=3)

#dfs.AN=dfs[dfs$sp=='ENGR-ENC',]                            
# graphics.off()                              
# par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=dfs.AN[dfs.AN$region%in%lrb$ID2,],cexi=0.1,scalef=.01,
                          zlab='prop.varw',rlab='region',tmain='Proportion of total estimation variance
                          Weighted average of anchovy CLAS Xe',
                          polylab=lrb$ID2,col=3)
# polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=dfs.AN[dfs.AN$region%in%lrs$ID2,],cexi=0.1,scalef=.01,
#                           zlab='prop.varw',rlab='region',tmain='Proportion of total estimation variance
#                           Weighted average of anchovy SURF Xe',
#                           polylab=lrs$ID2,col=3)                            


#save plot
dev.print(device=png,filename=paste(path.results.cv,cruise,"_ANE_averages.png",sep=''),width=800,height=800)
dev.off()


#24.3 b) sardine----
#*************************************
#modified to work only with CLAS
dfs.AN=dfs[dfs$sp=='SARD-PIL',]                            
#graphics.off()                              
par(mfrow=c(1,2))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=dfs.AN[dfs.AN$region%in%lrb$ID2,],cexi=0,scalef=.01,
                          zlab='prop.vara',rlab='region',tmain='Proportion of total estimation variance
                          Simple average of sardine CLAS Xe',
                          polylab=lrb$ID2,col=4)
# polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=dfs.AN[dfs.AN$region%in%lrs$ID2,],cexi=0,scalef=.05,
#                           zlab='prop.vara',rlab='region',tmain='Proportion of total estimation variance
#                           Simple average of sardine SURF Xe',
#                           polylab=lrs$ID2)  

#dfs.AN=dfs[dfs$sp=='SARD-PIL',]                            
#graphics.off()                              
#par(mfrow=c(1,1))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=dfs.AN[dfs.AN$region%in%lrb$ID2,],cexi=0,scalef=.01,
                          zlab='prop.varw',rlab='region',tmain='Proportion of total estimation variance
                          Weighted average of sardine CLAS Xe',
                          polylab=lrb$ID2,col=4)
# polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.SURF,
#                           z=dfs.AN[dfs.AN$region%in%lrs$ID2,],cexi=0,scalef=.01,
#                           zlab='prop.varw',rlab='region',tmain='Proportion of total estimation variance
#                           Weighted average of sardine SURF Xe',
#                           polylab=lrs$ID2,col=4)                            

#save plot
dev.print(device=png,filename=paste(path.results.cv,cruise,"_PIL_averages.png",sep=''),width=800,height=800)
dev.off()

#24.3 c) sprat----
#*************************************
#modified to work only with CLAS
dfs.AN=dfs[dfs$sp=='SPRA-SPR',]                            
#graphics.off()                              
par(mfrow=c(1,2))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=dfs.AN[dfs.AN$region%in%lrb$ID2,],cexi=0,scalef=.01,
                          zlab='prop.vara',rlab='region',tmain='Proportion of total estimation variance
                          Simple average of sprat CLAS Xe',
                          polylab=lrb$ID2,col=1)

polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=dfs.AN[dfs.AN$region%in%lrb$ID2,],cexi=0,scalef=.01,
                          zlab='prop.varw',rlab='region',tmain='Proportion of total estimation variance
                          Weighted average of sprat CLAS Xe',
                          polylab=lrb$ID2,col=1)
#save plot
dev.print(device=png,filename=paste(path.results.cv,cruise,"_SPR_averages.png",sep=''),width=800,height=800)
dev.off()


#24.3 d) horse mackerel----
#*************************************
#modified to work only with CLAS
dfs.AN=dfs[dfs$sp=='TRAC-TRA',]                            
#graphics.off()                              
par(mfrow=c(1,2))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=dfs.AN[dfs.AN$region%in%lrb$ID2,],cexi=0,scalef=.01,
                          zlab='prop.vara',rlab='region',tmain='Proportion of total estimation variance
                          Simple average of horse mackerel CLAS Xe',
                          polylab=lrb$ID2,col=7)

polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=dfs.AN[dfs.AN$region%in%lrb$ID2,],cexi=0,scalef=.01,
                          zlab='prop.varw',rlab='region',tmain='Proportion of total estimation variance
                          Weighted average of horse mackerel CLAS Xe',
                          polylab=lrb$ID2,col=7)
#save plot
dev.print(device=png,filename=paste(path.results.cv,cruise,"_HOM_averages.png",sep=''),width=800,height=800)
dev.off()


#24.3 e) boarfish----
#*************************************
#modified to work only with CLAS
dfs.AN=dfs[dfs$sp=='CAPR-APE',]                            
#graphics.off()                              
par(mfrow=c(1,2))
polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=dfs.AN[dfs.AN$region%in%lrb$ID2,],cexi=0,scalef=.01,
                          zlab='prop.vara',rlab='region',tmain='Proportion of total estimation variance
                          Simple average of boarfish CLAS Xe',
                          polylab=lrb$ID2,col=6)

polygon.simple.bubbleplot(esdu.sp.UTM=esdu.sp,lpolys=polys.CLAS,
                          z=dfs.AN[dfs.AN$region%in%lrb$ID2,],cexi=0,scalef=.01,
                          zlab='prop.varw',rlab='region',tmain='Proportion of total estimation variance
                          Weighted average of boarfish CLAS Xe',
                          polylab=lrb$ID2,col=6)
#save plot
dev.print(device=png,filename=paste(path.results.cv,cruise,"_BOF_averages.png",sep=''),width=800,height=800)
dev.off()


# 24.4. Estimation variance and CV per species category and echotype -----------
# REM: across regions, mean fish densities are weighted by the region 
# surface
#*************************************  
CV.DEV.csp=aggregate(CV.df[,c('mB1','mwB1','wew','wewi','wews',
                              'wea','weai','weas')],
                     list(CV.df$CodEsp,CV.df$DEV),sum,na.rm=TRUE)
names(CV.DEV.csp)=c('CodEsp','DEV','mB','mwB','wsigmaE2','wsigmaE2i',
                    'wsigmaE2s','sigmaE2','sigmaE2i','sigmaE2s')
CV.DEV.csp$sp=substr(CV.DEV.csp$CodEsp,1,8)
CV.DEV.csp$CV=sqrt(CV.DEV.csp$sigmaE2)/CV.DEV.csp$mB
CV.DEV.csp$CVi=sqrt(CV.DEV.csp$sigmaE2i)/CV.DEV.csp$mB
CV.DEV.csp$CVs=sqrt(CV.DEV.csp$sigmaE2s)/CV.DEV.csp$mB
CV.DEV.csp$wCV=sqrt(CV.DEV.csp$wsigmaE2)/CV.DEV.csp$mwB
CV.DEV.csp$wCVi=sqrt(CV.DEV.csp$wsigmaE2i)/CV.DEV.csp$mwB
CV.DEV.csp$wCVs=sqrt(CV.DEV.csp$wsigmaE2s)/CV.DEV.csp$mwB
CV.DEV.csp=merge(CV.DEV.csp,sigmaE2tots,by.x='sp',by.y='sp')
CV.DEV.csp$prop.vara=CV.DEV.csp$sigmaE2/CV.DEV.csp$sigmaE2tot
CV.DEV.csp$prop.varai=CV.DEV.csp$sigmaE2i/CV.DEV.csp$sigmaE2tot
CV.DEV.csp$prop.varas=CV.DEV.csp$sigmaE2s/CV.DEV.csp$sigmaE2tot
CV.DEV.csp$prop.varw=CV.DEV.csp$wsigmaE2/CV.DEV.csp$wsigmaE2tot
CV.DEV.csp$prop.varwi=CV.DEV.csp$wsigmaE2i/CV.DEV.csp$wsigmaE2tot
CV.DEV.csp$prop.varws=CV.DEV.csp$wsigmaE2s/CV.DEV.csp$wsigmaE2tot


#save file
write.table(CV.DEV.csp,paste(path.results.cv,cruise,"_CVspeciesCatEchotype.csv",sep=""),sep=";",row.names=F)


#24.4 Barplots: EVA per species and echotypes:----
#graphics.off() 
#x11()
dfs=EVA.barplot2(df=CV.DEV.csp,anames='DEV',
                args.legend = list(x = "topright",text.width=.035),
                xlabs=c('Proportion of total estimation variance'),
                ylab='Echotype')
#savePlot(paste(path.results.regions,cruise,'propTotVariance_Echotype.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_propTotVariance_Echotype(2).png",sep=''),width=800,height=800)
dev.off()


#24.4.1 Barplots: EVA and mean density for 1 species and echotypes:------                        
#graphics.off()   
#x11()
#24.4.1 a) anchovy-----
dfs=EVA.barplot(df=CV.DEV.csp,anames='DEV',lsps=c('ENGR-ENC'),
                v2names=c('mB','mwB'),vnames=c('prop.vara','prop.varw'),
                args.legend = list(x = "topright",text.width=.045),
                xlabs=c('Proportion of total estimation variance','Mean biomass'),
                ylab='Echotype')                                    
#savePlot(paste(path.results.regions,cruise,'ANpropTotVariance&MeanBiomass_Echotype.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_ANpropTotVariance&MeanBiomass_Echotype.png",sep=''),width=800,height=800)
dev.off()


#24.4.1 b) sardine----
#graphics.off()
#x11()
dfs=EVA.barplot(df=CV.DEV.csp,anames='DEV',lsps=c('SARD-PIL'),
                v2names=c('mB','mwB'),vnames=c('prop.vara','prop.varw'),
                args.legend = list(x = "bottomright",text.width=.045),
                xlabs=c('Proportion of total estimation variance','Mean biomass'),
                ylab='Echotype')
#savePlot(paste(path.results.regions,cruise,'SApropTotVariance&MeanBiomass_Echotype.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_PILpropTotVariance&MeanBiomass_Echotype.png",sep=''),width=800,height=800)
dev.off()

#24.4.1 c) sprat----
#graphics.off()
#x11()
dfs=EVA.barplot(df=CV.DEV.csp,anames='DEV',lsps=c('SPRA-SPR'),
                v2names=c('mB','mwB'),vnames=c('prop.vara','prop.varw'),
                args.legend = list(x = "bottomright",text.width=.045),
                xlabs=c('Proportion of total estimation variance','Mean biomass'),
                ylab='Echotype')
#savePlot(paste(path.results.regions,cruise,'SApropTotVariance&MeanBiomass_Echotype.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_SPRpropTotVariance&MeanBiomass_Echotype.png",sep=''),width=800,height=800)
dev.off()


#24.4.1 d) horse mackerel----
#graphics.off()
#x11()
dfs=EVA.barplot(df=CV.DEV.csp,anames='DEV',lsps=c('TRAC-TRA'),
                v2names=c('mB','mwB'),vnames=c('prop.vara','prop.varw'),
                args.legend = list(x = "bottomright",text.width=.045),
                xlabs=c('Proportion of total estimation variance','Mean biomass'),
                ylab='Echotype')
#savePlot(paste(path.results.regions,cruise,'SApropTotVariance&MeanBiomass_Echotype.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_HOMpropTotVariance&MeanBiomass_Echotype.png",sep=''),width=800,height=800)
dev.off()


#24.4.1 e) boarfish----
#graphics.off()
#x11()
dfs=EVA.barplot(df=CV.DEV.csp,anames='DEV',lsps=c('CAPR-APE'),
                v2names=c('mB','mwB'),vnames=c('prop.vara','prop.varw'),
                args.legend = list(x = "bottomright",text.width=.045),
                xlabs=c('Proportion of total estimation variance','Mean biomass'),
                ylab='Echotype')
#savePlot(paste(path.results.regions,cruise,'SApropTotVariance&MeanBiomass_Echotype.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_BOFpropTotVariance&MeanBiomass_Echotype.png",sep=''),width=800,height=800)
dev.off()


# 24.5. Compute estimation variance and CV per species category ------------
# REM: once fish density means and variances are averaged over regions,
# they must be added to compute total density
#*************************************
CV.csp=aggregate(CV.DEV.csp[,c('mB','mwB','wsigmaE2','wsigmaE2i',
                               'wsigmaE2s','sigmaE2','sigmaE2i',
                               'sigmaE2s')],
                 list(CV.DEV.csp$CodEsp),sum,na.rm=TRUE)
names(CV.csp)=c('CodEsp','mB1','mwB1','wsigmaE2','wsigmaE2i','wsigmaE2s',
                'sigmaE2','sigmaE2i','sigmaE2s')
CV.csp=merge(CV.csp,Biom.csp.df)
CV.csp$sp=substr(CV.csp$CodEsp,1,8)
#Surface of the estimation zone
Asup=sum(unique(deszones$AREA))/2
Asups=aggregate(deszones$AREA,list(deszones$TYPE),sum)
names(Asups)=c('TYPE','AREAtot')
Asup=Asups[Asups$TYPE=='CLAS','AREAtot']
#Mean fish density
CV.csp$mwB=CV.csp$wbiom/Asup
CV.csp$mB=CV.csp$biom/Asup
CV.csp=merge(CV.csp,sigmaE2tots,by.x='sp',by.y='sp')
#Proportions of total EVA per species 
CV.csp$prop.vara=CV.csp$sigmaE2/CV.csp$sigmaE2tot
CV.csp$prop.varai=CV.csp$sigmaE2i/CV.csp$sigmaE2tot
CV.csp$prop.varas=CV.csp$sigmaE2s/CV.csp$sigmaE2tot
CV.csp$prop.varw=CV.csp$wsigmaE2/CV.csp$wsigmaE2tot
CV.csp$prop.varwi=CV.csp$wsigmaE2i/CV.csp$wsigmaE2tot
CV.csp$prop.varws=CV.csp$wsigmaE2s/CV.csp$wsigmaE2tot

#check means 
graphics.off()
plot(CV.csp$mB,CV.csp$mB1,xlab='Reference haul method',
     ylab='Geographic method',main='Overall mean biomass per species')
points(CV.csp$mwB,CV.csp$mwB1,pch=16)
#text(CV.csp$mwB,CV.csp$mwB1,CV.csp$mwB,labels=CV.csp$CodEsp)
legend('topleft',c('Arithmetic Xe average','Weighted Xe average'),
       pch=c(1,16))


# 24.6. Compute estimation error per species categories -----
#*************************************
CV.csp$CV=sqrt(CV.csp$sigmaE2)/CV.csp$mB
CV.csp$CVi=sqrt(CV.csp$sigmaE2i)/CV.csp$mB
CV.csp$CVs=sqrt(CV.csp$sigmaE2s)/CV.csp$mB
CV.csp$wCV=sqrt(CV.csp$wsigmaE2)/CV.csp$mwB
CV.csp$wCVi=sqrt(CV.csp$wsigmaE2i)/CV.csp$mwB
CV.csp$wCVs=sqrt(CV.csp$wsigmaE2s)/CV.csp$mwB
CV.csp$sp=substr(CV.csp$CodEsp,1,8)
CV.csp$strate=substr(CV.csp$CodEsp,10,13)

CV.csp.comp=CV.csp
head(CV.csp.comp)
CV.csp.comp$wref.over.geo=CV.csp.comp$mwB/CV.csp.comp$mwB1
CV.csp.comp$ref.over.geo=CV.csp.comp$mB/CV.csp.comp$mB1
CV.csp.comps=CV.csp.comp[CV.csp.comp$sp%in%c('ENGR-ENC','SARD-PIL','SPRA-SPR',
                                             'TRAC-TRA','CAPR-APE'),]

par(mar=c(5,12,1,1))
bpc=barplot(t(as.matrix(CV.csp.comps[,c('wref.over.geo','ref.over.geo')])),
            beside=T,names.arg=CV.csp.comps$CodEsp,horiz=T,las=2,
            xlab='charef regions/geo regions biomass')  
text(rep(1.5,4),bpc[1,],round(CV.csp.comps$wref.over.geo,1))
text(rep(1.5,4),bpc[2,],round(CV.csp.comps$ref.over.geo,1))

#save files
write.csv(CV.csp,paste(path.results.cv,paste(cruise,'_CV.csv',sep=''),sep=''),
          row.names=FALSE)                
write.csv(CV.df,paste(path.results.cv,paste(cruise,'_CV_data.csv',sep=''),
                      sep=''),row.names=FALSE)

#24.6.1 Barplots: EVA and mean density for 1 species and depth stratum:----
#24.6.1 a) anchovy----
#graphics.off()  
#x11()
dfs=EVA.barplot(df=CV.csp,anames='strate',lsps=c('ENGR-ENC'),
                v2names=c('mB','mwB'),vnames=c('prop.vara','prop.varw'),
                args.legend = list(x = "topright",text.width=.1),
                xlabs=c('Proportion of total estimation variance','Mean biomass'))                            
#savePlot(paste(path.results.regions,cruise,'ANpropTotVariance&MeanBiomass_Stratum.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_ANpropTotVariance&MeanBiomass_Stratum.png",sep=''),width=800,height=800)
dev.off()

#24.6.1 b) sardine----
#graphics.off()  
#x11()
dfs=EVA.barplot(df=CV.csp,anames='strate',lsps=c('SARD-PIL'),
                v2names=c('mB','mwB'),vnames=c('prop.vara','prop.varw'),
                args.legend = list(x = "topright",text.width=.05),
                xlabs=c('Proportion of total estimation variance','Mean biomass'))                            
#savePlot(paste(path.results.regions,cruise,'SApropTotVariance&MeanBiomass_Stratum.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_PILpropTotVariance&MeanBiomass_Stratum.png",sep=''),width=800,height=800)
dev.off()


#24.6.1 c) sprat----
#graphics.off()  
#x11()
dfs=EVA.barplot(df=CV.csp,anames='strate',lsps=c('SPRA-SPR'),
                v2names=c('mB','mwB'),vnames=c('prop.vara','prop.varw'),
                args.legend = list(x = "topright",text.width=.05),
                xlabs=c('Proportion of total estimation variance','Mean biomass'))                            
#savePlot(paste(path.results.regions,cruise,'SApropTotVariance&MeanBiomass_Stratum.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_SPRpropTotVariance&MeanBiomass_Stratum.png",sep=''),width=800,height=800)
dev.off()


#24.6.1 d) horse mackerel----
#graphics.off()  
#x11()
dfs=EVA.barplot(df=CV.csp,anames='strate',lsps=c('TRAC-TRA'),
                v2names=c('mB','mwB'),vnames=c('prop.vara','prop.varw'),
                args.legend = list(x = "topright",text.width=.05),
                xlabs=c('Proportion of total estimation variance','Mean biomass'))                            
#savePlot(paste(path.results.regions,cruise,'SApropTotVariance&MeanBiomass_Stratum.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_HOMpropTotVariance&MeanBiomass_Stratum.png",sep=''),width=800,height=800)
dev.off()

#24.6.1 e) boarfish----
#graphics.off()  
#x11()
dfs=EVA.barplot(df=CV.csp,anames='strate',lsps=c('CAPR-APE'),
                v2names=c('mB','mwB'),vnames=c('prop.vara','prop.varw'),
                args.legend = list(x = "topright",text.width=.05),
                xlabs=c('Proportion of total estimation variance','Mean biomass'))                            
#savePlot(paste(path.results.regions,cruise,'SApropTotVariance&MeanBiomass_Stratum.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_BOFpropTotVariance&MeanBiomass_Stratum.png",sep=''),width=800,height=800)
dev.off()

# 24.7. Compute estimation variance and CV per species --------
# REM: once fish density means and variances are averaged over regions,
# they must be added to compute total density                         
#*************************************
names(CV.csp)                         
CV.sp=aggregate(CV.csp[,c('mB','mwB','wsigmaE2','wsigmaE2i','wsigmaE2s',
                          'sigmaE2','sigmaE2i','sigmaE2s')],
                list(CV.csp$sp),sum,na.rm=TRUE)
names(CV.sp)=c('sp','mB','mwB','wsigmaE2','wsigmaE2i','wsigmaE2s',
               'sigmaE2','sigmaE2i','sigmaE2s')
CV.sp$CV=sqrt(CV.sp$sigmaE2)/CV.sp$mB
CV.sp$CVi=sqrt(CV.sp$sigmaE2i)/CV.sp$mB
CV.sp$CVs=sqrt(CV.sp$sigmaE2s)/CV.sp$mB
CV.sp$wCV=sqrt(CV.sp$wsigmaE2)/CV.sp$mwB
CV.sp$wCVi=sqrt(CV.sp$wsigmaE2i)/CV.sp$mwB
CV.sp$wCVs=sqrt(CV.sp$wsigmaE2s)/CV.sp$mwB

#save file
write.table(CV.sp, paste(path.results.cv,cruise,"_CVspecies.csv",sep=""),sep=";",row.names=F)


#24.7.1 a) Barplots: total CV per species:-----
#x11()
par(mar=c(3,8,1,1))                       
barplot(t(as.matrix(CV.sp[,c('wCV','CV')])),names.arg=CV.sp[,'sp'],
        horiz=TRUE,las=2,main='',beside=TRUE,xlab='',
        legend=c('CV, mean Xe weighted by sA','CV, arithmetic mean Xe'),
        args.legend = list(x = "topright",text.width=0.05))

#savePlot(paste(path.results.regions,cruise,'CVspecies.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_CVspecies.png",sep=''),width=800,height=800)
dev.off()

#24.7.b) Barplots: identification and spatial error per species:----
#x11()
par(mar=c(3,8,1,1))                       
barplot(t(as.matrix(CV.sp[,c('wCVi','wCVs')])),names.arg=CV.sp[,'sp'],
        horiz=TRUE,las=2,main='',beside=FALSE,xlab='',
        legend=c('XE identification error, mean Xe weighted by sA',
                 'NASC spatial error, arithmetic mean Xe'),
        args.legend = list(x = "bottomright",text.width=0.05))
#savePlot(paste(path.results.regions,cruise,'IDvarVsSPvarSpecies.png'))
dev.print(device=png,filename=paste(path.results.cv,cruise,"_IDvarVsSPvarSpecies.png",sep=''),width=800,height=800)
dev.off()


CV.df[CV.df$sp%in%c('ENGR-ENC'),c('sp','TYPE','region','wCV')]
CV.df[CV.df$sp%in%c('SARD-PIL'),c('sp','TYPE','region','wCV')]
CV.df[CV.df$sp%in%c('SPRA-SPR'),c('sp','TYPE','region','wCV')]


CV.csp[CV.csp$sp%in%c('ENGR-ENC','SARD-PIL'),c('CodEsp','strate','wbiom','wCV')]
CV.csp[,c('CodEsp','strate','wbiom','wCV')]

#save files
write.csv(CV.csp,paste(path.results.cv,paste(cruise,'_CV.csv',sep=''),sep=''),
          row.names=FALSE)                

write.csv(CV.df,paste(path.results.cv,paste(cruise,'_CV_data.csv',sep=''),
                      sep=''),row.names=FALSE)

# 24.8. Merge with biomass per species results and export ---------
#*************************************
head(Biom.sp.ALL)
biom.CV.res=merge(Biom.sp.ALL,CV.sp)

biom.CV.res[biom.CV.res$sp%in%c('ENGR-ENC','SARD-PIL','SPRA-SPR'),c('sp','wbiom','wCV')]
biom.CV.res[,c('sp','wbiom','wCV','wCVi','wCVs')]

#save file
write.csv(biom.CV.res,paste(path.results.cv,paste(cruise,'_biom.CV.sp.csv',sep=''),
                         sep=''),row.names=FALSE)
#*******************************************************************************************************