#*****************************************************************
# EchoRbiom: acoustics+catches=biomass
#*****************************************************************
# Original code by Pierre Petitgas, 02/ 06/ 2003
# Adapted by Mathieu Doray mathieu.doray@ifremer.fr, 11/02/2010, 2011, 2012 ....
#*****************************************************************
#  Acoustic fish biomass assessment, Ifremer methodology 
#*****************************************************************
  
#*******************************************************************************************************
# STEP 1: IMPORT AND CHECK DATA
#*******************************************************************************************************
library(EchoR) #old EchoR library (vs.1.3.7) with new EchoR scripts (vs.1.3.7.1)

# 0.1. Set cruise name -------
#*****************************************************************
cruise='PELTIC2018'

# 0.2. Define paths ------
#*****************************************************************
# Path to data
path.data='C:/Users/SRC01/Documents/SILVIA_security copy/01. PELTIC/PELTIC2018/EchoR/data/' 
# Path where to save results
path.results='C:/Users/SRC01/Documents/SILVIA_security copy/01. PELTIC/PELTIC2018/EchoR/results/'
# create path results with the date of the analysis
#dir.create(file.path(path.results,paste('results',Sys.Date())),showWarnings = TRUE)
dir.create(file.path(path.results,'peltic18'),showWarnings=TRUE)

path.results='C:/Users/SRC01/Documents/SILVIA_security copy/01. PELTIC/PELTIC2018/EchoR/results/peltic18/'

# 0.3. Display control plots?-------
#*****************************************************************
plotit=TRUE

# 0.4. Change TS~length equation? --------
#------------------------
TSzCor=FALSE

#*****************************************************************
#1. Import data ---------
#*****************************************************************
# 1.1. Import fishing data and define species to be assessed ---------
#*****************************************************************
# 1.1.1. Import fish total samples ------------
#*****************************************************************
Pechei=read.table(paste(path.data,'FishingTotalSamples_PELTIC2018.csv',sep=''),sep=';',header=TRUE)
head(Pechei);dim(Pechei)
#Pechei$TotalNumber=round(Pechei$TotalNumber)
Pechei$NoPerKg=round(Pechei$NoPerKg, digits=2) 

#convert headers from input to EchoR formats
Pechei=header.converter(x=Pechei,direction='base2R')
head(Pechei);dim(Pechei)
 
# 1.1.2. Import fish sub-samples ----------
#*****************************************************************
mens0=read.table(paste(path.data,'FishingSubSamples_PELTIC2018.csv',sep=''),sep=';',header=TRUE)
head(mens0);dim(mens0)

#numbers of fish have to be integers
mens0$WeightOfFish <- round(mens0$WeightOfFish,digits=2)
          #mens0$NumberOfFish=round(mens0$NumberOfFish, digits=0
          #remove the columns SubSampleWeight (POISDSECHANT) and SubSampleNumberofFish (NECHANT)that are optional and
          #are not the "real" subsample values
          #mens0 <- mens0[,-which(names(mens0)%in%c("SubSampleWeight","SubSampleNumberOfFish"))]

#convert headers from input to EchoR formats
mens0=header.converter(x=mens0,direction='base2R')
head(mens0);dim(mens0)

# Species frequency in total catches
sort(table(Pechei$GENR_ESP))
# Species frequency in subsamples
sort(table(mens0$GENR_ESP))

# Species to be assessed
#peltic data
lsp.assess=c('ENGR-ENC','CAPR-APE','TRAC-TRA','SARD-PIL','SPRA-SPR')
#you can not assess all you species, you need to have at least one ouside the lsp.asses
#otherwise sizecat funtion gives an error. We leave the  'CLUP-HAR' out for the moment

# 1.2. check subsamples:  -----------
# 1.2.1. size categories and species codes ---------
#   species code = species+size category
#   each species code will be assessed separately 
#*****************************************************************
# Automatic size category break definition: #change the minLag to create more size categories (original=5)
rsc=sizecat(menst=mens0,spsel=lsp.assess,nsize='Lcm',plotit=TRUE,minLag=3,
            method='breaks',breaks='means',lbks=NULL)

# OR manual size category break definition: just fill lbks (only one break per species)
# rsc=sizecat(menst=mens0,spsel=lsp.assess,nsize='Lcm',plotit=TRUE,minLag=5,
#              method='breaks',breaks='means',lbks=c(NA,50,NA,32,30,15,25,NA))

#Extract size category breaks 
dflm=rsc$dflm
#Use corrected file
mens=rsc$mens2 #created in sizecat fx
dim(mens0);dim(mens)
head(mens)
# number of hauls in corrected file
length(unique(mens$NOSTA)) 
# check length categories in corrected file
unique(mens$CATEG)  
table(mens$CATEG)
table(mens$GENR_ESP,mens$CATEG)
table(as.character(mens0[mens0$GENR_ESP%in%lsp.assess,'GENR_ESP']),
      as.character(mens0[mens0$GENR_ESP%in%lsp.assess,'CATEG']))
table(as.character(mens[mens$GENR_ESP%in%lsp.assess,'GENR_ESP']),
      as.character(mens[mens$GENR_ESP%in%lsp.assess,'CATEG']))
unique(mens$CodEsp2)

# 1.2.2. check length distributions -----------
#*****************************************************************
#Select main species
lspcodes.mens=c(paste(lsp.assess,'0',sep='-'),paste(lsp.assess,'G',sep='-'))

# Check that size distributions per size category are unimodal (from all the sp)
par(oma=c(1,1,1,1)) #increase plot margins, default is 0
mens.range.check(mens,nsp='CodEsp2',lsp=lspcodes.mens)

# 1.2.3. Compute and check length-weight relationships ------------
#*****************************************************************
#Compute and display length-weight relationships for all species
LW.mens.csp2=LW.compute(catch=mens,codespname='CodEsp2',
              Lname='Lcm',Wname='PM',wname='NBIND',plotit=TRUE,Ncol=4)
 
  #to check if it is similar to Joana's values
  # LW.mens.all=as.data.frame(LW.mens.csp2)
  # write.table(LW.mens.all,paste(path.results,'LWRvalues.csv',sep=''),row.names=FALSE,sep=';')
             
#weight in POISTAILLE and MEAN WEIGHT have to be in Kg
#put Wscaling=1 if you PM is in grams!!!

dev.print(device=png,
         filename=paste(path.results,cruise,"_FishingSubSamplesLW(1).png",sep=''),
         width=800,height=800)


#Compute and display length-weight relationships for main species
LW.mens=LW.compute(catch=mens,lspcodes.mens=lsp.assess,codespname='GENR_ESP',
              Lname='Lcm',Wname='PM',wname='NBIND',plotit=TRUE,Nmin=8)
LW.mens$GENR_ESP=LW.mens$CodEsp2
par(mfrow=c(1,1))

# 1.2.4. Remove invalid biological measurements ------------
#***************************************************************** 
mens.cor=mens[mens$NBIND>0|!is.na(mens$NBIND),]

# 1.2.5. Eventually, change TS as function of depth for clupeids
#*****************************************************************
  if (TSzCor){
    # Change b20 only
    Pechei=TSdephCorrection(Pechei,b20=65.4,equation='changeb20only')
    # Apply Ona (2003) equation, based on seabed depth at trawling stations
    # Ona (2003). An expanded target-strength relationship for herring. IJMS, 60: 493–499.
    Pechei=TSdephCorrection(Pechei,equation='Ona03zcor')
    # Apply Zhao (2008) equation, based on seabed depth at trawling stations 
    # Zhao et al 2008. Depth-dependent target strength of anchovy (Engraulis japonicus) 
    # measured in situ. IJMS, 65: 882 –888.
    Pechei=TSdephCorrection(Pechei,equation='Zhao2008')
    # Check b20 distribution
    Pecheis1=Pechei[Pechei$GENR_ESP%in%c('ENGR-ENC'),]
    dev.off()
    boxplot(Pecheis1$BAC,ylab='Anchovy b20 (dB)',main=cruise)
  } 

# 1.2.6. Check total catch data -----------
#*****************************************************************
# 1.2.6.1. Eventually, add haul numbers to subsamples -------
#*****************************************************************
dim(mens)
mens=merge(mens,unique(Pechei[,c('NOSTA','NOCHAL')]),by.x='NOSTA',
             by.y='NOSTA')
dim(mens)
  
# 1.2.6.2. Check for duplicated species in catches -----------
  #*****************************************************************
  duplicated.speciesIncatches.check(Pechei)

# 1.2.6.3. Check depth strata ---------
  #*****************************************************************  
  unique(Pechei$STRATE)
  #correct depth strata
  Pechei$STRATE=substr(Pechei$STRATE,1,4)

# 1.2.6.4. Check (and eventually --------- 
# define thoughtfull) size categories
  #*****************************************************************
  Pechei=size.category.check(Pechei,dflm=dflm,
                             spsel=lsp.assess,method='breaks',breaks='manual')

  #update species code
  Pechei$CodEsp=paste(Pechei$GENR_ESP,Pechei$STRATE,Pechei$SIGNEP,sep='-')
  #duplicated catches after size category correction?
  duplicated.speciesIncatches.check(Pechei)
  table(Pechei$GENR_ESP)
  # Check species:size category combinations
  table(as.character(Pechei[Pechei$GENR_ESP%in%lsp.assess,'GENR_ESP']),
        as.character(Pechei[Pechei$GENR_ESP%in%lsp.assess,'SIGNEP']))
  
#1.2.6.5. Aggregate species with same species code
  #*****************************************************************
  rac=aggregate.catches(Pechei,spname='CodEsp',stname='NOSTA')
  Pechei=rac$Pechei
  # update species codes
  Pechei$CodEsp=paste(Pechei$GENR_ESP,Pechei$STRATE,Pechei$SIGNEP,sep='-')
  #duplicated catches after size category correction?
  duplicated.speciesIncatches.check(Pechei)
  # diplay species codes
  unique(Pechei$CodEsp)

#1.2.6.6. Check length-weight relationships from catch data (each dot=trawl station)
  #*****************************************************************
#Compute and display length-weight relationships for all species
  x11();dev.off()
  LW.catch=LW.compute(catch=Pechei,lspcodes.mens=lsp.assess,
              codespname='GENR_ESP',Lname='LM',Wname='PM',gname=NA,
              plotit=TRUE,compute.LW=TRUE,Wscaling=1,ux11=FALSE)
  par(mfrow=c(1,1))
#Eventually, export plots
  dev.print(device=png,
          filename=paste(path.results,cruise,"_TotalCatchLW1.png",sep=''),
          width=800,height=800)
# dev.set(which = dev.next())
# dev.print(device=png,
#           filename=paste(path.results,cruise,"_TotalCatchLW2.png",sep=''),
#           width=800,height=800)

#1.2.6.7. Remove invalid hauls before biomass assessment
  #*****************************************************************  
  Pecheis=Pechei[Pechei$STRATE!='NULL',]
  unique(Pecheis$NOSTA)
  unique(Pechei[Pechei$STRATE=='NULL','NOSTA'])

# 1.3. Import echotypes definitions
  #*****************************************************************
  EspDev=read.table(paste(path.data,'Echotypes_PELTIC2018.csv',sep=''),
                    sep=';',header=TRUE,stringsAsFactors=F)
  head(EspDev);dim(EspDev)
  #convert headers from Echobase to EchoR formats
  EspDev=header.converter(x=EspDev,direction='base2R')
  head(EspDev);dim(EspDev)

  # 1.3.1. Check and correct assessed species in catches and not in echotype/species table -------
  #*****************************************************************
  
  #EspDev[nrow(EspDev) + 1,] =c("PELTIC2017","CLAS","<NA>","D4","0","TRAC-TRA","TRAC-TRA-CLAS-G") #add the row that is missing
  res=EspDevCor(EspDev,Pecheis,lsp.assess) #only the assessed species
  EspDev=res$EspDev
  
  # 1.3.2. Extract metadata
  #***************************************************************** 
  EspDev=EspDev[order(EspDev$DEV),]  
  meta=get.metadata(EspDev)
  lyears=cruise
  lNdev=meta$lNdev
  EspDev$CAMPAGNE=cruise
  Ndevi=lNdev[[1]]


# 1.4. Import NASC per echotype data ---------
  #*****************************************************************
  # Import from local file
  path.chamanRef=paste(path.data,'AcousticData_PELTIC2018.csv',sep='')

  ESDUDEVs=read.table(path.chamanRef,sep=';',header=TRUE)
  head(ESDUDEVs);dim(ESDUDEVs) # 2207   18
  
  #convert headers from Echobase to EchoR formats
  head(ESDUDEVs)
  ESDUDEVs=header.converter(x=ESDUDEVs,direction='base2R')
  head(ESDUDEVs);dim(ESDUDEVs)
  # Check that "zonesCLAS" and "zonesSURF" fields are present
  if (sum(!c("zonesCLAS","zonesSURF")%in%names(ESDUDEVs))>0){
    ESDUDEVs$zonesCLAS=NA
    ESDUDEVs$zonesSURF=NA
  }

# 1.4.1. check NASC per echotypes -----------
  #*****************************************************************
  # look for negative NASC (ESDUS with negative sA in echotypes)
  check.esdudev(ESDUDEVs,Ndev=lNdev[[1]])
#save the output  
#out <- capture.output(summary(check.esdudev(ESDUDEVs,Ndev=lNdev[[1]])))
#write.table(out,paste(path.results,'check.esudev.csv',sep=''),row.names=FALSE,sep=';')

  
  # display NASC quantiles per echotypes, to be compared with previous years
  ds=ESDUDEVs[,paste('D',lNdev[[1]],sep='')]
  if (is.null(dim(ds))){
    summary(ds)
  }else{ 
    apply(ESDUDEVs[,paste('D',lNdev[[1]],sep='')],2,summary)
  }
  # check postratification regions
  unique(ESDUDEVs$zonesSURF)
  unique(ESDUDEVs$zonesCLAS)
  apply(ESDUDEVs[,paste('D',lNdev[[1]],sep='')],2,sum)
  
  
# 1.4.2. Total fish NASC map
  #*****************************************************************
  par(mfrow=c(1,1))
  plot(ESDUDEVs$LONG,ESDUDEVs$LAT,xlab='',ylab='',main=paste(cruise,'total fish NASC'),
     type='n')
  coast()
  points(ESDUDEVs$LONG,ESDUDEVs$LAT,pch=16,cex=0.1+log(ESDUDEVs[,'TOTAL']+1)/5)
  # points(ESDUDEVs[ESDUDEVs$FLAG==1,'LONG'],ESDUDEVs[ESDUDEVs$FLAG==1,
  #                                              'LAT'],cex=0.1+log(ESDUDEVs[ESDUDEVs$FLAG==1,'TOTAL']+1)/5,
  #        pch=16,col=2)
  #legend('bottomright',legend=c('All ESDUS','Flagged ESDUs'),pch=16,col=seq(2))
  legend('bottomright',legend=c('All EDSUS'),pch=16,col=1)
  
#export plots
   dev.print(device=png,
             filename=paste(path.results,cruise,"_TotalfishNASCmap.png",sep=''),
             width=800,height=800)
   
   dev.off()

# 1.4.2. Check NASC per echotype distributions ('profiles')
  #*****************************************************************
  head(ESDUDEVs)
  ds2=ESDUDEVs[,as.character(unique(EspDev$DEV))]
  N=dim(ESDUDEVs)[1]
  if (is.null(dim(ds2))){
    Ds=ds2
    mDs=mean(ds2)
  }else{ 
    Ds=c(unlist(ESDUDEVs[,paste('D',Ndevi,sep='')]))
    mDs=colMeans(ESDUDEVs[,as.character(unique(EspDev$DEV))])
  }
  Dl=rep(Ndevi,each=N)
  Dsum=data.frame(Ds=Ds,Dl=Dl)

  par(mfrow=c(2,2))
  
  boxplot(ESDUDEVs$TOTAL+1,main='Fish NASC distribution',log='y')
  boxplot(Ds+1~Dl,Dsum,main='Fish NASC distribution per echotype',log='y')
  boxplot(Ds+1~Dl,Dsum,main='Fish NASC distribution per echotype',log='') #i remove the logarithmic scale
  
  plot(Ndevi,mDs,xlab='Echotype',ylab='Mean NASC')
  par(mfrow=c(1,1))

#export plots
  dev.print(device=png,
            filename=paste(path.results,cruise,"_FishNASC.png",sep=''),
            width=800,height=800)
  
  dev.off()
  
  
# 1.5. IF "complement" in echotype definition, -----------
# pools non assessed species into a fake "complement" ('COMP-LEM') 
# species with:
#  'SIGNEP'='0'
#  'LM'=20
#  'MOULE'=20
#  'PM'=0.05
#	 'CAC'=20
#  'BAC'=67
  #*****************************************************************

# 1.5.1. assessed species in catches and not in echotype/species table
  #*****************************************************************  
unique(Pecheis$CodEsp)[(substr(unique(Pecheis$CodEsp),1,8)%in%lsp.assess)&
                        (!unique(Pecheis$CodEsp)%in%unique(EspDev$CodEsp))]

# 1.5.2. Compute "complement" ----------
  #*****************************************************************
dim(Pecheis)
Pechei.comp=complement(Pecheis,EspDev,wmens=FALSE)
dim(Pechei.comp)

# 1.6. Fishing data correction and selection ---------
#*****************************************************************
# 1.6.1. remove or correct catches with null 'MOULE'n (No per Kg), mean length or total weight --------
# and returns dataframes with corrected and bad data
#*****************************************************************
Pechei.cor=check.catches(captures.comp=Pechei.comp,
                         path.checkit=path.results,
                         correctit=TRUE,removeit=TRUE,LW=NULL)
dim(Pechei);dim(Pechei.comp);dim(Pechei.cor$captures.OK)

# 1.6.2. Select catches data between several flavors: --------
# - complemented: Peche.comp
# - or not: Peche
# - corrected but not complemented: Peche.cor
# - corrected and complemented: Peche.comp.cor
#*****************************************************************
Pechef=Pechei.cor$captures.OK
Pechei.pb=Pechei.cor$captures.PB
dim(Pechef)  
dim(Pechei.pb)
#Select haul id
nid='NOCHAL'

# 1.6.3. Check for duplicated species ---------
#*****************************************************************
duplicated.speciesIncatches.check(Pechef)

# 1.6.4. Check length-weight relationships of corrected species --------
#*****************************************************************
#x11();dev.off()
  LWcor=LW.compute(catch=Pechef,lspcodes.mens=NULL,
                    codespname='GENR_ESP',Lname='LM',Wname='PM',gname=NA,
                    plotit=TRUE,compute.LW=TRUE,Wscaling=1,ux11=FALSE)
  #export plots
  dev.print(device=png,
          filename=paste(path.results,cruise,"_TotalCorCatchLW1.png",sep=''),
          width=800,height=800)
  
  par(mfrow=c(1,1))


# 1.6.5. Select length-weight equation for further computations ---------
  #*****************************************************************
  LW=LWcor
  names(LW)[names(LW)=='CodEsp2']='GENR_ESP'
  LW$CAMPAGNE=cruise

# 1.6.6. Check catches,length and weight ranges of corrected catches -------
  #*****************************************************************
  captures.range.check(Pechef,EspDev)
  dev.print(device=png,
          filename=paste(path.results,cruise,"_TotalCorCatchLWsum.png",sep=''),
          width=800,height=800)
  par(mfrow=c(1,1))

# 1.6.4. Check haul geographic positions ---------
  #***************************************************************** 
  hpc.res=haul.positions.check(Pechef,correctit=TRUE,
                             path.checkit=path.results)

# 1.7. Put catches in "wide" format for piecharts --------
  #*****************************************************************
# catches per species
  Pechels=widen.catch(Pechef,nid=nid,nsp='GENR_ESP',lsp=lsp.assess)
  #Pechels[order(Pechels$NOCHAL),] # order them

  # catches per species code
  Pechels2=widen.catch(Pechef,nid=nid,nsp='CodEsp',lsp=lsp.assess)

# 1.8. Grand average of catches per hauls --------
  #*****************************************************************
  graphics.off()
  par(mfrow=c(1,1))
  head(Pechels)
  # Total catches per species
  pie(colSums(Pechels[,!names(Pechels)%in%c('CAMPAGNE','NOCHAL','LONF','LATF','STRATE',
                                            'SONDE','GEAR')]),main=paste('Total catches',cruise))

  dev.print(device=png,
            filename=paste(path.results,cruise,"_GrandAverageCatchHaul.png",sep=''),
            width=800,height=800)
  par(mfrow=c(1,1))  
  
# 8. Export checked data files ------------
  #*****************************************************************
write.table(Pechef,paste(path.results,'Pechef.csv',sep=''),row.names=FALSE,sep=';')
write.table(Pechels,paste(path.results,'Pechels.csv',sep=''),row.names=FALSE,sep=';')
write.table(Pechels2,paste(path.results,'Pechels2.csv',sep=''),row.names=FALSE,sep=';')
write.table(mens,paste(path.results,'mens.csv',sep=''),row.names=FALSE,sep=';')
write.table(EspDev,paste(path.results,'EspDev.csv',sep=''),row.names=FALSE,sep=';')
write.table(ESDUDEVs,paste(path.results,'ESDUDEVs.csv',sep=''),row.names=FALSE,sep=';')

