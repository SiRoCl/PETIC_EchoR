#*******************************************************************************************************
# STEP 5: BIOMASS PER ESDU AT AGES: COMPUTE BIOMASS-AT-AGE PER ESDU AND SPECIES
#*******************************************************************************************************
# Author: Mathieu Doray mathieu.doray@ifremer.fr

library(EchoR)

# paths definition
#*************************************
path.LA.AN=paste(path.data,'AgeLengthSp2.csv',sep='') #the real anchovy
path.LA.SA=paste(path.data,'AgeLengthSp3.csv',sep='')
dir.create(file.path(path.ref, 'AtAge'), showWarnings = FALSE)
path.results.age=paste(path.ref,'AtAge/',sep='')

# 22. Compute biomass-at-age per ESDU ----------
#*************************************
# 22.1. Import and check length-age data -----
#*************************************

#head(read.csv(path.LA.AN,sep=""),row.names=F,sep=";")
head(read.csv(path.LA.AN,sep=";"))
head(read.csv(path.LA.SA,sep=";"))

# Import length-age data
# from text files
Af.df=age.at.length.import(path1=path.LA.AN,path2=path.LA.SA,
                           sp=c('ENGR-ENC','SARD-PIL'),cruise) #species changed from 'ENGR-ENC' to sprat, not working (inside the function the name is specified...)
# OR from dataframes
#data(AgeLengthSp1)
#data(AgeLengthSp2)
#Af.df=age.at.length.import(path1=AgeLengthSp1,path2=AgeLengthSp2,
#                           sp=c('ENGR-ENC','SARD-PIL'),cruise)

LA.AN=Af.df$Af.AN.df
LA.SA=Af.df$Af.SA.df
LA.ANSA=Af.df$LA.ANSAs
head(LA.ANSA)
LAa=aggregate(LA.ANSA[,c('Lmm','poids')],list(LA.ANSA$sp,LA.ANSA$age),mean)
names(LAa)=c('sp','age','mLmm','mw')
boxplot(poids~age,data=LA.ANSA[LA.ANSA$sp=='ENGR-ENC',],
        main=paste(cruise,'Weight-at age distributions, anchovy raw biometry data'),
        xlab='Age',ylab='Weight(g)')
#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_LW_plot_ANE.png",sep=''),width=800,height=800)

boxplot(poids~age,data=LA.ANSA[LA.ANSA$sp=='SARD-PIL',],
        main=paste(cruise,'Weight-at age distributions, sardine raw biometry data'),
        xlab='Age',ylab='Weight(g)')
#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_LW_plot_PIL.png",sep=''),width=800,height=800)

Af.AN.df=Af.df$Af.AN.df
Af.SA.df=Af.df$Af.SA.df
LA=Af.df$LA
head(LA)
head(Af.AN.df)
wmLA=aggregate(LA$Ti,list(LA$GENRE_ESP,LA$AGE),sum)                                         
names(wmLA)=c('GENRE_ESP','AGE','wmL')

#convert length to cm if need be
if(wmLA$wmL[3]>100){
  wmLA$wmL=wmLA$wmL/10
}

# 22.2. Compute abundance and biomass-at-age per ESDU --------
#*************************************
# Enter species to assess
lsp=c('ENGR-ENC','SARD-PIL')

#List with one dataframe per species: biomass (in No. of fish) at age
list.N.esdu.ANSA.age=lapply(X=seq(length(lsp)),
                            FUN=age.ventilator.sp,
                            BLsp=Biomres.sp.size,LA=LA,lsp=lsp,mult=10)
#NW.esdu.sp.size.longs

# 22.3. Check abundances-at-age ------
#*************************************

for (i in 1:length(list.N.esdu.ANSA.age)){
  li=list.N.esdu.ANSA.age[[i]]
  print(unique(li$sp))
  names(li)
  Ncheck=(apply(li[,3:(dim(li)[2]-1)],1,sum)-li$Ntot)
  summary(Ncheck)
  #boxplot(Ncheck)
  li.pb=li[abs(Ncheck)>=1,]
  cat('No. of errors','\n') 
  print(dim(li.pb)[1])
}                                         
            #Results 16.01.2018
            # [1] "ENGR-ENC"
            # No. of errors 
            # [1] 16
            # [1] "SARD-PIL"
            # No. of errors 
            # [1] 17



# 22.4. Produce a dataframe with abundances in no. of fish (N) -----------
#per species, age class and ESDUs
#*************************************

NW.esdu.ANSA.age.long=N.esdu.sp.age.long.summary(list.N.esdu.ANSA.age,
                                                 wmLA,LW)

head(NW.esdu.ANSA.age.long)

#Select non-null ages
NW.esdu.ANSA.age.longs=NW.esdu.ANSA.age.long[NW.esdu.ANSA.age.long$N!=0,]
head(NW.esdu.ANSA.age.longs)
dim(NW.esdu.ANSA.age.longs)
unique(NW.esdu.ANSA.age.longs$Age)

# 22.5. Check biomass-at-age ---------
#*************************************
# Total abundance per species
# at-age data
aggregate(NW.esdu.ANSA.age.long[,c('N','BA')],list(NW.esdu.ANSA.age.long$sp),sum)
nws=unique(NW.esdu.ANSA.age.long[,c('sp','Esdu','Ntot')])
aggregate(nws$Ntot,list(nws$sp),sum)
# per esdu data
biomres.sp.esdu[biomres.sp.esdu$sp%in%c('ENGR-ENC','SARD-PIL'),c('BB','BN')]

# check
NW.tot.check=aggregate(NW.esdu.ANSA.age.longs[,c('N','BA')],
                       list(NW.esdu.ANSA.age.longs$sp),sum)
names(NW.tot.check)=c('sp','Ntot.sp','BA.age')
NW.tot.check$mw.age.g=(NW.tot.check$BA.age/NW.tot.check$Ntot.sp)*1e6
NW.tot.check

NW.age.check=aggregate(NW.esdu.ANSA.age.longs[,c('N','BA')],
                        list(NW.esdu.ANSA.age.longs$sp,
                             NW.esdu.ANSA.age.longs$Age),sum)
names(NW.age.check)=c('sp','age','Ntot.age','BAtot.age')
#compute mean weight-at age per species
NW.age.check$mw=(NW.age.check$BAtot.age/NW.age.check$Ntot.age)*1e6
NW.age.check.AN=NW.age.check[NW.age.check$sp=='ENGR-ENC',]
NW.age.check.SA=NW.age.check[NW.age.check$sp=='SARD-PIL',]
mw.check=rbind(data.frame(type='est',NW.age.check[,c('sp','age','mw')]),data.frame(type='raw',LAa[,c('sp','age','mw')]))
library(lattice)
graphics.off()
par(mfrow=c(1,1))
xyplot(mw~age|sp,groups=type,mw.check,auto.key=T)
boxplot(poids~age,data=LA.ANSA[LA.ANSA$sp=='ENGR-ENC',],main=paste(cruise,'Weight-at age distributions of anchovy raw data, 
                                                                   with estimated mean (triangle)'))
points(NW.age.check.AN$age,NW.age.check.AN$mw,pch=17,col=2)
boxplot(poids~age,data=LA.ANSA[LA.ANSA$sp=='SARD-PIL',],main=paste(cruise,'Weight-at age distributions of sardine raw data, 
                                                                   with estimated mean (triangle)'))
points(NW.age.check.SA$age,NW.age.check.SA$mw,pch=17,col=2)
#compute mean biomass per species and age
NW.age.check$mB=NW.age.check$BAtot.age/dim(ESDUDEVs)[1]
#re-compute total biomass per species and age
NW.age.check$Btot=NW.age.check$mB*areaf
#compute mean number per species and age
NW.age.check$mN=NW.age.check$Ntot.age/dim(ESDUDEVs)[1]
#re-compute total biomass per species and age
NW.age.check$Ntot=NW.age.check$mN*areaf
# compute total biomass per species
Btot.sp.check=aggregate(NW.age.check$Btot,list(NW.age.check$sp),sum)
# compute total abundance per species
Ntot.sp.check=aggregate(NW.age.check$Ntot,list(NW.age.check$sp),sum)

head(Biomres.sp.size)
wntot.size=aggregate(Biomres.sp.size[Biomres.sp.size$sp%in%c('ENGR-ENC','SARD-PIL'),c('W','N')],
               list(Biomres.sp.size[Biomres.sp.size$sp%in%c('ENGR-ENC','SARD-PIL'),'sp']),sum,na.rm=TRUE)
wntot.size$mw.g.size=1000*wntot.size$W/wntot.size$N
mw.comp=data.frame(biomres.sp.esdu[biomres.sp.esdu$sp%in%c('ENGR-ENC','SARD-PIL'),c('sp','mw.g','mw.g.catch')],
                   mw.g.age=Btot.sp.check[,2]/Ntot.sp.check[,2]*1e6,mw.biometry=aggregate(LA.ANSA$poids,list(LA.ANSA$sp),mean)[,2],
                   mw.size=wntot.size$mw.g.size)
barplot(as.matrix(mw.comp[,-1]),beside=TRUE,legend.text=c('ENGR-ENC','SARD-PIL'))

# format at-age global data 
#*************************************
NW.age=merge(NW.age.check,LAa,by.x=c('sp','age'),by.y=c('sp','age'))
names(NW.age)[c(5,11)]=c('mw.est','mw.raw')
NW.age=merge(NW.age,NW.tot.check,by.x=c('sp'),by.y=c('sp'))
NW.age$pN.age=NW.age$Ntot.age/NW.age$Ntot.sp
NW.age$pW.age=NW.age$BAtot.age/NW.age$BA.age

aggregate(NW.age[,c('Ntot.age','BAtot.age')],list(NW.age$sp),sum)
NW.age$BAtot.age/NW.age$Ntot.age

library(lattice)
xyplot(mw.est~mw.raw|sp,data=NW.age)

barchart(age~Btot|sp,NW.age.check,
         xlab='Total biomass per esdu (t)')

barchart(age~Ntot|sp,NW.age.check,
         xlab='Total abundance per esdu')

barchart(age~pN.age|sp,NW.age,
         xlab='Proportions of abundance at age per esdu')

barchart(age~mw.est|sp,NW.age,
         xlab='Mean weights at age per esdu (g)')

# 22.6. Format outputs ------------
#*************************************
Biomres.sp.age=merge(unique(ESDUDEVs[,c('esdu.id','TC','LONG','LAT')]),
                     NW.esdu.ANSA.age.longs,by.x='esdu.id',by.y='Esdu')
head(Biomres.sp.age)

Biomres.AN.age=Biomres.sp.age[Biomres.sp.age$sp=='ENGR-ENC',]
head(Biomres.AN.age)
Biomres.SA.age=Biomres.sp.age[Biomres.sp.age$sp=='SARD-PIL',]
head(Biomres.SA.age)

# 22.7. check numbers at age ---------
#*************************************
esduNatot=aggregate(Biomres.sp.age$N,list(Biomres.sp.age$sp,Biomres.sp.age$esdu.id),
                    sum)
names(esduNatot)=c('sp','esdu','Natot')
esduNtot=aggregate(Biomres.sp.age$Ntot,list(Biomres.sp.age$sp,Biomres.sp.age$esdu.id),
                   unique)  
names(esduNtot)=c('sp','esdu','Ntot')
esduNtotcheck=merge(esduNatot,esduNtot)
names(esduNtotcheck)
summary(esduNtotcheck$Ntot-esduNtotcheck$Natot)

# 22.8. check age distributions -------
#*************************************
N.esdu.AN.age=list.N.esdu.ANSA.age[[1]]
head(N.esdu.AN.age)
sna.AN=colSums(N.esdu.AN.age[,!names(N.esdu.AN.age)%in%c('Esdu','sp','Ntot')])
barplot(sna.AN,main=paste(cruise,'Anchovy numbers-at-age'))
#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_NatAge_ANE.png",sep=''),width=800,height=800)

Biomres.AN.age=merge(unique(ESDUDEVs[,c('esdu.id','TC','LONG','LAT')]),
                     N.esdu.AN.age,by.x='esdu.id',by.y='Esdu')


N.esdu.SA.age=list.N.esdu.ANSA.age[[2]]
head(N.esdu.SA.age)
sna.SA=colSums(N.esdu.SA.age[,!names(N.esdu.SA.age)%in%c('Esdu','sp','Ntot')])
barplot(sna.SA,main=paste(cruise,'Sardine numbers-at-age'))
#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_NatAge_PIL.png",sep=''),width=800,height=800)
Biomres.SA.age=merge(unique(ESDUDEVs[,c('esdu.id','TC','LONG','LAT')]),
                     N.esdu.SA.age,by.x='esdu.id',by.y='Esdu')
head(Biomres.SA.age)

# 22.9. Data export -----------
#*************************************
write.table(Biomres.sp.age,paste(path.results.age,'Biomres.sp.age.refhaul.csv',sep=''),
            row.names=FALSE,sep=';')  
write.table(NW.age,paste(path.results.age,'NW.age.csv',sep=''),
            row.names=FALSE,sep=';')  

# 22.10. Maps of biomass per age class, species and esdu -------
#*************************************
#All ages on one plot
#*************************************
graphics.off()
head(Biomres.AN.age)
##ANE
#bars
pie.xy(x=Biomres.AN.age$LONG,y=Biomres.AN.age$LAT,
       z=as.matrix(Biomres.AN.age[,6:10]),bar=FALSE,mtitle='Anchovy',
       pcol=c('darkgreen','seagreen','green'),pradius=.3,smax=2)
legend('bottomleft',legend=paste('Age',seq(3)),
       fill=c('darkgreen','seagreen','green'))

#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_BatAge_ANEbars.png",sep=''),width=800,height=800)
#dev.off()

#pies
pie.xy(x=Biomres.AN.age$LONG,y=Biomres.AN.age$LAT,
       z=as.matrix(Biomres.AN.age[,6:10]),bar=TRUE,mtitle='Anchovy',
       pcol=c('darkgreen','seagreen','green'),pradius=.3,smax=2)
legend('bottomleft',legend=paste('Age',seq(3)),
       fill=c('darkgreen','seagreen','green'))

#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_BatAge_ANE.png",sep=''),width=800,height=800)
#dev.off()

##PIL
head(Biomres.SA.age)
#bars
pie.xy(x=Biomres.SA.age$LONG,y=Biomres.SA.age$LAT,
       z=as.matrix(Biomres.SA.age[,6:10]),bar=TRUE,mtitle='Sardine',
       pcol=rainbow(7))
legend('bottomleft',legend=paste('Age',seq(7)),
       fill=rainbow(7))

#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_BatAge_PILbars.png",sep=''),width=800,height=800)
#dev.off()

#pies
pie.xy(x=Biomres.SA.age$LONG,y=Biomres.SA.age$LAT,
       z=as.matrix(Biomres.SA.age[,6:10]),bar=FALSE,mtitle='Sardine',
       pcol=rainbow(7))
legend('bottomleft',legend=paste('Age',seq(7)),
       fill=rainbow(7))
#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_BatAge_PIL.png",sep=''),width=800,height=800)
#dev.off()

names(Biomres.SA.age)
names(Biomres.AN.age)

#One age per plot
#*************************************
#graphics.off()
B.esdu.age.ANSA.plot(path.res.charef=path.results.age,Biomres.AN.age=Biomres.AN.age,
                     Biomres.SA.age=Biomres.SA.age)

###################################################################################################################################-
###################################################################################################################################-

# 22.b Compute biomass-at-age per ESDU  sprat and horse mackerel----------
#(Added by me)#
#*************************************
# 22.b.1. Import and check length-age data -----
#*************************************
path.LA.SP=paste(path.data,'AgeLengthSp1.csv',sep='') 
path.LA.TR=paste(path.data,'AgeLengthSp4.csv',sep='')
dir.create(file.path(path.ref, 'AtAge'), showWarnings = FALSE)
path.results.age=paste(path.ref,'AtAge/',sep='')

#check
head(read.csv(path.LA.SP,sep=";"))
head(read.csv(path.LA.TR,sep=";"))


Af.df=age.at.length.importSP_TR(path1=path.LA.SP,path2=path.LA.TR,
                          sp=c('SPRA-SPR','TRAC-TRA'),cruise) #species changed to sprat and horse mackerel


LA.SP=Af.df$Af.SP.df
LA.TR=Af.df$Af.TR.df
LA.SPTR=Af.df$LA.SPTRs
head(LA.SPTR)
LAa=aggregate(LA.SPTR[,c('Lmm','poids')],list(LA.SPTR$sp,LA.SPTR$age),mean)
names(LAa)=c('sp','age','mLmm','mw')
boxplot(poids~age,data=LA.SPTR[LA.SPTR$sp=='SPRA-SPR',],
        main=paste(cruise,'Weight-at age distributions, sprat raw biometry data'),
        xlab='Age',ylab='Weight(g)')
#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_LW_plot_SPR.png",sep=''),width=800,height=800)

boxplot(poids~age,data=LA.SPTR[LA.SPTR$sp=='TRAC-TRA',],
        main=paste(cruise,'Weight-at age distributions, horse mackerel raw biometry data'),
        xlab='Age',ylab='Weight(g)')
#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_LW_plot_HOM.png",sep=''),width=800,height=800)

Af.SP.df=Af.df$Af.SP.df
Af.TR.df=Af.df$Af.TR.df
LA=Af.df$LA
head(LA)
head(Af.SP.df)
wmLA=aggregate(LA$Ti,list(LA$GENRE_ESP,LA$AGE),sum)                                         
names(wmLA)=c('GENRE_ESP','AGE','wmL')

#convert length to cm if need be
if(wmLA$wmL[3]>100){
  wmLA$wmL=wmLA$wmL/10
}

# 22.b.2. Compute abundance and biomass-at-age per ESDU --------
#*************************************
# Enter species to assess
lsp=c('SPRA-SPR','TRAC-TRA')

#List with one dataframe per species: biomass (in No. of fish) at age
list.N.esdu.SPTR.age=lapply(X=seq(length(lsp)),
                            FUN=age.ventilator.sp,
                            BLsp=Biomres.sp.size,LA=LA,lsp=lsp,mult=10)
#NW.esdu.sp.size.longs

# 22.b.3. Check abundances-at-age ------
#*************************************

for (i in 1:length(list.N.esdu.SPTR.age)){
  li=list.N.esdu.SPTR.age[[i]]
  print(unique(li$sp))
  names(li)
  Ncheck=(apply(li[,3:(dim(li)[2]-1)],1,sum)-li$Ntot)
  summary(Ncheck)
  #boxplot(Ncheck)
  li.pb=li[abs(Ncheck)>=1,]
  cat('No. of errors','\n') 
  print(dim(li.pb)[1])
}                                  

# 22.b.4. Produce a dataframe with abundances in no. of fish (N) -----------
#per species, age class and ESDUs
#*************************************

NW.esdu.SPTR.age.long=N.esdu.sp.age.long.summary(list.N.esdu.SPTR.age,wmLA,LW)
head(NW.esdu.SPTR.age.long)

#Select non-null ages
NW.esdu.SPTR.age.longs=NW.esdu.SPTR.age.long[NW.esdu.SPTR.age.long$N!=0,]
head(NW.esdu.SPTR.age.longs)
dim(NW.esdu.SPTR.age.longs)
unique(NW.esdu.SPTR.age.longs$Age)

# 22.b.5. Check biomass-at-age ---------
#*************************************
# Total abundance per species
# at-age data
aggregate(NW.esdu.SPTR.age.long[,c('N','BA')],list(NW.esdu.SPTR.age.long$sp),sum)
nws=unique(NW.esdu.SPTR.age.long[,c('sp','Esdu','Ntot')])
aggregate(nws$Ntot,list(nws$sp),sum)
# per esdu data
biomres.sp.esdu[biomres.sp.esdu$sp%in%c('SPRA-SPR','TRAC-TRA'),c('BB','BN')]

# check
NW.tot.check=aggregate(NW.esdu.SPTR.age.longs[,c('N','BA')],
                       list(NW.esdu.SPTR.age.longs$sp),sum)
names(NW.tot.check)=c('sp','Ntot.sp','BA.age')
NW.tot.check$mw.age.g=(NW.tot.check$BA.age/NW.tot.check$Ntot.sp)*1e6
NW.tot.check

NW.age.check=aggregate(NW.esdu.SPTR.age.longs[,c('N','BA')],
                       list(NW.esdu.SPTR.age.longs$sp,
                            NW.esdu.SPTR.age.longs$Age),sum)
names(NW.age.check)=c('sp','age','Ntot.age','BAtot.age')
#compute mean weight-at age per species
NW.age.check$mw=(NW.age.check$BAtot.age/NW.age.check$Ntot.age)*1e6
NW.age.check.SP=NW.age.check[NW.age.check$sp=='SPRA-SPR',]
NW.age.check.TR=NW.age.check[NW.age.check$sp=='TRAC-TRA',]
mw.check=rbind(data.frame(type='est',NW.age.check[,c('sp','age','mw')]),data.frame(type='raw',LAa[,c('sp','age','mw')]))
library(lattice)
graphics.off()
par(mfrow=c(1,1))
xyplot(mw~age|sp,groups=type,mw.check,auto.key=T)
boxplot(poids~age,data=LA.SPTR[LA.SPTR$sp=='SPRA-SPR',],main=paste(cruise,'Weight-at age distributions of sprat raw data, with estimated mean (triangle)'))
points(NW.age.check.SP$age,NW.age.check.SP$mw,pch=17,col=2)
boxplot(poids~age,data=LA.SPTR[LA.SPTR$sp=='TRAC-TRA',],main=paste(cruise,'Weight-at age distributions of horse mackerel raw data, with estimated mean (triangle)'))
points(NW.age.check.TR$age,NW.age.check.TR$mw,pch=17,col=2)
#compute mean biomass per species and age
NW.age.check$mB=NW.age.check$BAtot.age/dim(ESDUDEVs)[1]
#re-compute total biomass per species and age
NW.age.check$Btot=NW.age.check$mB*areaf
#compute mean number per species and age
NW.age.check$mN=NW.age.check$Ntot.age/dim(ESDUDEVs)[1]
#re-compute total biomass per species and age
NW.age.check$Ntot=NW.age.check$mN*areaf
# compute total biomass per species
Btot.sp.check=aggregate(NW.age.check$Btot,list(NW.age.check$sp),sum)
# compute total abundance per species
Ntot.sp.check=aggregate(NW.age.check$Ntot,list(NW.age.check$sp),sum)

head(Biomres.sp.size)
wntot.size=aggregate(Biomres.sp.size[Biomres.sp.size$sp%in%c('SPRA-SPR','TRAC-TRA'),c('W','N')],
                     list(Biomres.sp.size[Biomres.sp.size$sp%in%c('SPRA-SPR','TRAC-TRA'),'sp']),sum,na.rm=TRUE)
wntot.size$mw.g.size=1000*wntot.size$W/wntot.size$N
mw.comp=data.frame(biomres.sp.esdu[biomres.sp.esdu$sp%in%c('SPRA-SPR','TRAC-TRA'),c('sp','mw.g','mw.g.catch')],
                   mw.g.age=Btot.sp.check[,2]/Ntot.sp.check[,2]*1e6,mw.biometry=aggregate(LA.SPTR$poids,list(LA.SPTR$sp),mean)[,2],
                   mw.size=wntot.size$mw.g.size)
barplot(as.matrix(mw.comp[,-1]),beside=TRUE,legend.text=c('SPRA-SPR','TRAC-TRA'))

# format at-age global data 
#*************************************
NW.age=merge(NW.age.check,LAa,by.x=c('sp','age'),by.y=c('sp','age'))
names(NW.age)[c(5,11)]=c('mw.est','mw.raw')
NW.age=merge(NW.age,NW.tot.check,by.x=c('sp'),by.y=c('sp'))
NW.age$pN.age=NW.age$Ntot.age/NW.age$Ntot.sp
NW.age$pW.age=NW.age$BAtot.age/NW.age$BA.age

aggregate(NW.age[,c('Ntot.age','BAtot.age')],list(NW.age$sp),sum)
NW.age$BAtot.age/NW.age$Ntot.age

library(lattice)
xyplot(mw.est~mw.raw|sp,data=NW.age)

barchart(age~Btot|sp,NW.age.check,
         xlab='Total biomass per esdu (t)')

barchart(age~Ntot|sp,NW.age.check,
         xlab='Total abundance per esdu')

barchart(age~pN.age|sp,NW.age,
         xlab='Proportions of abundance at age per esdu')

barchart(age~mw.est|sp,NW.age,
         xlab='Mean weights at age per esdu (g)')

# 22.b.6. Format outputs ------------
#*************************************
Biomres.sp.age=merge(unique(ESDUDEVs[,c('esdu.id','TC','LONG','LAT')]),
                     NW.esdu.SPTR.age.longs,by.x='esdu.id',by.y='Esdu')
head(Biomres.sp.age)

Biomres.SP.age=Biomres.sp.age[Biomres.sp.age$sp=='SPRA-SPR',]
head(Biomres.SP.age)
Biomres.TR.age=Biomres.sp.age[Biomres.sp.age$sp=='TRAC-TRA',]
head(Biomres.TR.age)

# 22.b.7. check numbers at age ---------
#*************************************
esduNatot=aggregate(Biomres.sp.age$N,list(Biomres.sp.age$sp,Biomres.sp.age$esdu.id),
                    sum)
names(esduNatot)=c('sp','esdu','Natot')
esduNtot=aggregate(Biomres.sp.age$Ntot,list(Biomres.sp.age$sp,Biomres.sp.age$esdu.id),
                   unique)  
names(esduNtot)=c('sp','esdu','Ntot')
esduNtotcheck=merge(esduNatot,esduNtot)
names(esduNtotcheck)
summary(esduNtotcheck$Ntot-esduNtotcheck$Natot)

# 22.b.8. check age distributions -------
#*************************************
N.esdu.SP.age=list.N.esdu.SPTR.age[[1]]
head(N.esdu.SP.age)
sna.SP=colSums(N.esdu.SP.age[,!names(N.esdu.SP.age)%in%c('Esdu','sp','Ntot')])
barplot(sna.SP,main=paste(cruise,'Sprat numbers-at-age'))
#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_NatAge_SPR.png",sep=''),width=800,height=800)

Biomres.SP.age=merge(unique(ESDUDEVs[,c('esdu.id','TC','LONG','LAT')]),
                     N.esdu.SP.age,by.x='esdu.id',by.y='Esdu')


N.esdu.TR.age=list.N.esdu.SPTR.age[[2]]
head(N.esdu.TR.age)
sna.TR=colSums(N.esdu.TR.age[,!names(N.esdu.TR.age)%in%c('Esdu','sp','Ntot')])
barplot(sna.TR,main=paste(cruise,'Horse mackerel numbers-at-age'))
#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_NatAge_HOM.png",sep=''),width=800,height=800)
Biomres.TR.age=merge(unique(ESDUDEVs[,c('esdu.id','TC','LONG','LAT')]),
                     N.esdu.TR.age,by.x='esdu.id',by.y='Esdu')
head(Biomres.TR.age)

# 22.b.9. Data export -----------
#*************************************
write.table(Biomres.sp.age,paste(path.results.age,'Biomres.sp.age.refhaul_SPTR.csv',sep=''),
            row.names=FALSE,sep=';')  
write.table(NW.age,paste(path.results.age,'NW.age_SPTR.csv',sep=''),
            row.names=FALSE,sep=';')  

# 22.b.10. Maps of biomass per age class, species and esdu -------
#*************************************
#All ages on one plot
#*************************************
graphics.off()
head(Biomres.SP.age)
##SPR
#bars
pie.xy(x=Biomres.SP.age$LONG,y=Biomres.SP.age$LAT,
       z=as.matrix(Biomres.SP.age[,6:12]),bar=FALSE,mtitle='Sprat',
       pcol=c('black','grey40','grey50','grey60','grey70','grey80','grey90'),pradius=.3,smax=2)
legend('bottomleft',legend=paste('Age',seq(6)),
       fill=c('black','grey40','grey50','grey60','grey70','grey80','grey90'))

#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_BatAge_SPRbars.png",sep=''),width=800,height=800)
#dev.off()

#pies
pie.xy(x=Biomres.SP.age$LONG,y=Biomres.SP.age$LAT,
       z=as.matrix(Biomres.SP.age[,6:12]),bar=TRUE,mtitle='Sprat',
       pcol=c('black','grey40','grey50','grey60','grey70','grey80','grey90'),pradius=.3,smax=2)
legend('bottomleft',legend=paste('Age',seq(6)),
       fill=c('black','grey40','grey50','grey60','grey70','grey80','grey90'))

#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_BatAge_SPR.png",sep=''),width=800,height=800)
#dev.off()

##HOM
head(Biomres.TR.age)
#bars
pie.xy(x=Biomres.TR.age$LONG,y=Biomres.TR.age$LAT,
       z=as.matrix(Biomres.TR.age[,6:8]),bar=TRUE,mtitle='Horse mackerel',
       pcol=c("gold", "yellow","lightyellow"))
legend('bottomleft',legend=paste('Age',seq(3)),
       fill=c("gold", "yellow","lightyellow"))

#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_BatAge_HOMbars.png",sep=''),width=800,height=800)
#dev.off()

#pies
pie.xy(x=Biomres.TR.age$LONG,y=Biomres.TR.age$LAT,
       z=as.matrix(Biomres.TR.age[,6:8]),bar=FALSE,mtitle='Horse mackerel',
       pcol=c("gold", "yellow","lightyellow"))
legend('bottomleft',legend=paste('Age',seq(3)),
       fill=c("gold", "yellow","lightyellow"))
#saveplot
#dev.print(device=png,filename=paste(path.results.age,cruise,"_BatAge_HOM.png",sep=''),width=800,height=800)
#dev.off()

names(Biomres.SP.age)
names(Biomres.TR.age)

#One age per plot
#*************************************
graphics.off()
B.esdu.age.SPTR.plot(path.res.charef=path.results.age,Biomres.SP.age=Biomres.SP.age,
                     Biomres.TR.age=Biomres.TR.age)
