
#Packages
library(reshape2);library(raster);library(sp);library(ggplot2);library(gridExtra);library(data.table);library(plyr);library(dplyr);library(POT)
########################################################################################################################################################################################################################################################################################################################################################################################
dev.off()
remove (list=ls())



                                       ###################################################
                                       #    Data ROCKYFOR3D - Simulations AUGUST 2017    #
                                       #                   FINAL FILE                    #
                                       ###################################################



# Scenario choice
########################################################################################################################################################################################################################################################################################################################################################################################
##Choice of a specific scenario (with forest in 1850, 1956, 1975 or 2013)
ChoiceA   = c(winDialogString("Year : 1850 (1) 1956 (2) 1975 (3) 2013 (4)",""))
source("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_RCodes/R_Functions/Function_QRA_CrollesIV_FINAL.R")
outputs01 = Function_QRA_CrollesIV_FINAL(ChoiceA); year = outputs01[[5]]; remove(ChoiceA)
########################################################################################################################################################################################################################################################################################################################################################################################

# First input 
#########################################################################################################################################################################################################################################################################################################
##Cliffs
rS_C    = raster("C:/Users/manon.farvacque/Documents/_CROLLES-IV/RockyFor3D_FinalInput/2013/Coteau/blshape.asc") 
dataS_C = rasterToPoints(rS_C); remove(rS_C) #LE COTEAU
rS_A    = raster("C:/Users/manon.farvacque/Documents/_CROLLES-IV/RockyFor3D_FinalInput/2013/Ardillais/blshape.asc") 
dataS_A = rasterToPoints(rS_A); remove(rS_A) #ARDILLAIS 
rS_M    = raster("C:/Users/manon.farvacque/Documents/_CROLLES-IV/RockyFor3D_FinalInput/2013/Magny/blshape.asc")    
dataS_M = rasterToPoints(rS_M); remove(rS_M) #MAGNY
rS_F    = raster("C:/Users/manon.farvacque/Documents/_CROLLES-IV/RockyFor3D_FinalInput/2013/Fragnes/blshape.asc")  
dataS_F = rasterToPoints(rS_F); remove(rS_F) #FRAGNES
##City
rB      = raster("C:/Users/manon.farvacque/Documents/_CROLLES-IV/DonneesEntree/Net_number/net_number.asc")
dataB   = rasterToPoints(rB); colnames(dataB) = c("XBat","YBat","NetNumber")
##DEM Resolution
Resolution_DEM    = 5
##Number of simulation per cliff unit
NbSimu_Cell       = 10000 
##Total number of cliff cells 
NbTot_CliffCells  = length(dataS_F[,1])+length(dataS_M[,1])+length(dataS_A[,1])+length(dataS_C[,1])
##Cliff surfaces m2 (ArcGIS : surface per cell = 5 * (5/cos(alpha)), with alpha = slope)
Cliff_TOT_Surface = 1278524.48 #(m2)
##Price per square metre [e/m2]
Price_m2          = 3000
##Nb of class
breaks            = 19
##Volume classes 
VolumesClass      = seq(from = 1.5, to = 19.5, by = 1)
########################################################################################################################################################################################################################################################################################################################################################################################

# Merging data 
########################################################################################################################################################################################################################################################################################################################################################################################
##We merge the four lists of results (because of 4 districts) in one list
dataRF3D_Scenario = rbind(outputs01[[1]], outputs01[[2]], outputs01[[3]], outputs01[[4]])
dataRF3D_Scenario = as.data.frame(dataRF3D_Scenario)
##How many impacts ?
ImpactNumber      = length(dataRF3D_Scenario$NumSimu)
##List of the houses reached by rockfalls (ID number)  
HousesReached     = data.frame(unique(dataRF3D_Scenario$BatNumber)); colnames(HousesReached) = c("NetNumber")
########################################################################################################################################################################################################################################################################################################################################################################################

# Frequency determination 
########################################################################################################################################################################################################################################################################################################################################################################################
##Data
##Study considering [1-20m3] rock blocks. Survey data -> 1 bloc of 33m3. Problem : disturbs the tail (too heavy), so exclude from the GPD analysis. 
DataField     = fread("C:/Users/manon.farvacque/Documents/CROLLES-IIBIS/FrequencyDetermination_Crolles/CubageArdillais.txt"); colnames(DataField) = c("Volume")
DataField_Vol = DataField$Volume[c(1:29)]
Field_Cliff   = 11.5 #hm2
Window        = 100 #Time window of observations 
##Histogram
#hist(DataField_Vol, breaks=25)
## Generalized Pareto Distribution
#mrlplot(DataField_Vol)
threshold = 1
mle       = fitgpd(DataField_Vol, threshold = threshold, est ="mle")
Scale     = as.numeric(mle$param["scale"])
Shape     = as.numeric(mle$param["shape"])
Lambda    = (length(which(DataField_Vol>threshold))/Window)*(1/Field_Cliff)  
########################################################################################################################################################################################################################################################################################################################################################################################
   


                              ##################################################
                              #   Quantitative Risk Assessment QRA - CROLLES   #
                              ##################################################



start.time<-Sys.time() #TIC

# Houses reached loop
########################################################################################################################################################################################################################################################################################################################################################################################
QRA_Table = matrix(0, nrow=length(HousesReached$NetNumber), ncol=20)
for (k in 1:length(HousesReached$NetNumber)){
     QRA_Table[k,1] = HousesReached$NetNumber[k]
     
##Rockyfor3D results extract for the specifics k (one building, all introduced with the loop)
dataRF3D_specific_k = subset(dataRF3D_Scenario, dataRF3D_Scenario$BatNumber == HousesReached$NetNumber[k])

# Damage
########################################################################################################################################################################################################################################################################################################################################################################################
##Degree of loss - According to Agliardi F. et al (2009) 
dataRF3D_specific_k[,9] = 1-(1.358/(1 + exp((((dataRF3D_specific_k$Ebrute*10^3)-129000)/120300))))
dataRF3D_specific_k[,9] = replace(dataRF3D_specific_k[,9], dataRF3D_specific_k[,9] < 0, 0)
colnames(dataRF3D_specific_k) = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume","Damage_Agliardi")
# Quantitative Risk Assessment
########################################################################################################################################################################################################################################################################################################################################################################################
##Bulding k price estimation
PriceBat = Price_m2*length(subset(dataB,dataB[,3]==HousesReached$NetNumber[k])[,3])*Resolution_DEM^2
##Volume classification
dataRF3D_specific_k[,8]    = replace(dataRF3D_specific_k[,8],dataRF3D_specific_k[,8] == 1, 1.01) # Ceiling not efficient if vol == 1 
dataRF3D_specific_k$Volume = ceiling(dataRF3D_specific_k$Volume)-0.5 

  for (i in 1:length(VolumesClass)){
       ##GPD - frequency law
       GPDLaw            = Lambda*(((1+(Shape*(((VolumesClass[i]-0.5)-threshold)/Scale)))^(-1/Shape))-((1+(Shape*(((VolumesClass[i]+0.5)-threshold)/Scale)))^(-1/Shape)))
       #GPDLaw           = ((0.85*((VolumesClass[i]-0.5)^-0.75))-(0.85*((VolumesClass[i]+0.5)^-0.75)))             #GUERIN
       #GPDLaw           = ((4.2/(12000))*(VolumesClass[i]-0.5)^-0.41)-((4.2/(12000))*(VolumesClass[i]+0.5)^-0.41) #DUSSAUGE
       
       ##Risk by volumes class
       Int5             = subset(dataRF3D_specific_k, dataRF3D_specific_k$Volume == VolumesClass[i])
       ##Number of impacts in building k for class volume i 
       NbImpact         = length(Int5[,1])
       ##Damage mean
       DamageMean       = mean(Int5[,9]); DamageMean[is.na(DamageMean)] = 0 
       ##Number of events per year in class volume i and for Crolles area (hm2)
       Frequency        = GPDLaw*Cliff_TOT_Surface*10^-4
       ##Impact probability 
       Pz               = NbImpact/((NbSimu_Cell/breaks)*NbTot_CliffCells)
       ##QRA calculation
       QRA              = Frequency*Pz*DamageMean*PriceBat
       QRA_Table[k,i+1] = QRA 
       ##Result (compilation on all volume classes)
       remove(GPDLaw,Int5,NbImpact,DamageMean,Frequency,Pz,QRA)}

remove(dataRF3D_specific_k,PriceBat)}
colnames(QRA_Table) = c("Bat","1.5","2.5","3.5","4.5","5.5","6.5","7.5","8.5","9.5","10.5","11.5","12.5","13.5","14.5","15.5","16.5","17.5","18.5","19.5")
########################################################################################################################################################################################################################################################################################################################################################################################

end.time<-Sys.time();time.taken<-end.time-start.time;time.taken #TOC

## Results
###############
ImpactNumber
length(HousesReached$NetNumber)
sum(QRA_Table[,c(2:20)])
###############

##
#####
########
###########
##############
################## PLOTS - RASTERS - TABLES
setwd("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_RESULTS/1975")

# Reach probabilities
########################################################################################################################################################################################################################################################################################################################################################################################
# ##Reach probabilities - All houses/volume classes
# Pz = data.frame()
# for (i in 2:length(VolumesClass)){
#   Pz[1,1] = 1.5
#   Pz[1,2] = length(which(dataRF3D_Scenario$Volume <= 2))
#   Pz[i,1] = VolumesClass[i]
#   Pz[i,2] = length(which(dataRF3D_Scenario$Volume > (VolumesClass[i] - 0.5) & dataRF3D_Scenario$Volume <= (VolumesClass[i] + 0.5)))}
# Pz[,3] = (Pz[,2]/((NbSimu_Cell/breaks)*NbTot_CliffCells))*100
# colnames(Pz) = c("VolumeClass","NbImpact","Percentage")
#               # write.table(Pz,file="Pz2013.csv",sep=",",col.names=TRUE,row.names=FALSE)

##Reach probabilities - All volume/ house & Return Level
Pz_BatReached = data.frame()
yrRL          = Cliff_TOT_Surface*10^-4*Xu*npy*(1/Field_Cliff)*(((1+(Shape*(((1)-threshold)/Scale)))^(-1/Shape))-((1+(Shape*(((20)-threshold)/Scale)))^(-1/Shape)))
for (i in 1:length(HousesReached$NetNumber)){
    int                = subset(dataRF3D_Scenario, dataRF3D_Scenario$BatNumber == HousesReached$NetNumber[i])
    Pz_BatReached[i,1] = HousesReached$NetNumber[i]
    Pz_BatReached[i,2] = length(int$NumSimu)
    Pz_BatReached[i,3] = Pz_BatReached[i,2]/(NbSimu_Cell*NbTot_CliffCells)
    Pz_BatReached[i,4] = Pz_BatReached[i,3]*yrRL
    #Pz_BatReached[i,5] = sum(QRA_Table[which(QRA_Table[,1] == Pz_BatReached[i,1]),c(2:20)])
    Pz_BatReached[i,5] = length(subset(dataB,dataB[,3]==HousesReached$NetNumber[i])[,3])*Resolution_DEM^2
    if (Pz_BatReached[i,5] <= 50){
      Pz_BatReached[i,6] = 1
    }else if (Pz_BatReached[i,5] > 50  && Pz_BatReached[i,5] <= 100){
      Pz_BatReached[i,6] = 2
    }else if (Pz_BatReached[i,5] > 100 && Pz_BatReached[i,5] <= 150){
      Pz_BatReached[i,6] = 3
    }else if (Pz_BatReached[i,5] > 150 && Pz_BatReached[i,5] <= 200){
      Pz_BatReached[i,6] = 4
    }else{
      Pz_BatReached[i,6] = 5}
    Pz_BatReached[i,7] = Pz_BatReached[i,6]*500000
    Pz_BatReached[i,8] = Pz_BatReached[i,7]*Pz_BatReached[i,4]
    remove(int)}
colnames(Pz_BatReached) = c("NetNumber","NbImpacts","Pz","Alea","Surface","NbPeople","Price","Risk")
sum(Pz_BatReached$Risk)

# Return = c(0,30,300,1000,5000,100^6)
# TableRL     = cut(Pz_BatReached$ReturnLevel, Return)
# TableRL     = as.data.frame(table(TableRL))
# for (i in 1:(length(Return)-1)){
#     int00A       = subset(Pz_BatReached, Pz_BatReached$ReturnLevel > Return[i] & Pz_BatReached$ReturnLevel <= Return[i+1])
#     TableRL[i,3] = sum(int00A$QRA)}
#               #write.table(Pz_BatReached,file="Pz_BatReached1850.csv",sep=",",col.names=TRUE,row.names=FALSE)
#               #ALL                = full_join(dataB, Pz_BatReached, by ="NetNumber")
#               #r01                = rasterFromXYZ(ALL[,c(1,2,6)])
#               #writeRaster(r01,'ReturnLevel1975.asc',format='ascii'); remove(ALL)
########################################################################################################################################################################################################################################################################################################################################################################################

# Volume distribution
########################################################################################################################################################################################################################################################################################################################################################################################
VolClasses = hist(dataRF3D_Scenario[,8], breaks = breaks, main ="Volume frequency ", xlab = "Volume classes")
axis(side=1, at=seq(1, 20))
########################################################################################################################################################################################################################################################################################################################################################################################

# Volume Energy distribution
########################################################################################################################################################################################################################################################################################################################################################################################
##1 - BARPLOT
EnergyMax1    = max(dataRF3D_Scenario$Ebrute)   
EnergyClasses = seq(0, EnergyMax1+4000, by = 4000)
source("C:/Users/manon.farvacque/Documents/_CROLLES-Iv/_Rcodes/R_Functions/Function_VolumeEnergy.R")
outputs02     = Function_VolumeEnergy(dataRF3D_Scenario, EnergyMax1, EnergyClasses)
newTable01    = outputs02[[1]]; rownames(newTable01) = c(EnergyClasses[1:length(EnergyClasses)-1])
newTable01    = (newTable01/sum(newTable01))*100
colour=c("navajowhite3","peachpuff4","lightblue2","lightpink1","darkolivegreen3")
barplot(t(newTable01),xaxt="n",col=colour,ylim=c(0,100),xlab="Energy",ylab="Frequency",space=0); axis(side=1,at=seq_along(EnergyClasses)-1,tick=FALSE,labels=EnergyClasses)
legend(x="topright",legend=c("1-5 m3","5-10 m3","10-15 m3","> 15 m3"),cex=0.6,fill=c(colour)); title(main=c(year))
#
EnergyMax2     = 1000
EnergyClasses2 = seq(0, EnergyMax2, by = 100)
source("C:/Users/manon.farvacque/Documents/CROLLES-II/Rcodes/Functions/Function_VolumeEnergy.R")
outputs03     = Function_VolumeEnergy(dataRF3D_Scenario, EnergyMax2, EnergyClasses2)
newTable02    = outputs03[[1]]; rownames(newTable02) = c(EnergyClasses2[1:length(EnergyClasses2)-1])
newTable02    = (newTable02/ImpactNumber)*100
colour=c("lightyellow","navajowhite","navajowhite3","peachpuff4","lightblue2","lightpink1","darkolivegreen3")
barplot(t(newTable02),xaxt="n",col=colour,ylim=c(0,17),xlab="Energy",ylab="Frequency",space=0); axis(side=1,at=seq_along(EnergyClasses2)-1,tick=FALSE,labels=EnergyClasses2)
legend(x="topright",legend=c("1-2 m3","2-3 m3","3-4 m3","4-5 m3","5-10 m3","10-15 m3","> 15 m3"),cex=0.6,fill=c(colour)); title(main=c(year))
##2 - NORMAL DISTRIBUTION
A    = list() 
int4 = c(7.5,12.5,17.5)
for (k in 1:4){               # for 1 - 2 m3, +/- 1m3
  int2     = subset(dataRF3D_Scenario, dataRF3D_Scenario$Volume > (VolumesClass[k]-0.5) & dataRF3D_Scenario$Volume <= (VolumesClass[k]+0.5))
  int3     = seq(0, ceiling(max(int2$Ebrute)), length = 20)
  dNormE   = dnorm(int3, mean(int2$Ebrute), sd(int2$Ebrute))
  A[[k]]   = data.frame(int3,dNormE); remove(int2,int3,dNormE)}
for (k in 1:length(int4)){    # for 5 - 20 m3, +/- 2.5m3
  int2     = subset(dataRF3D_Scenario, dataRF3D_Scenario$Volume > (int4[k]-2.5) & dataRF3D_Scenario$Volume <= (int4[k]+2.5))
  int3     = seq(0, ceiling(max(int2$Ebrute)), length = 20)
  dNormE   = dnorm(int3, mean(int2$Ebrute), sd(int2$Ebrute))
  A[[k+4]] = data.frame(int3,dNormE); remove(int2,int3,dNormE)}
B   = melt(A, id.vars = c("int3","dNormE")); remove(A)
var = c(rep("1-2 m3", 20), rep("2-3 m3", 20), rep("3-4 m3", 20), rep("4-5 m3", 20), rep("5-10 m3", 20), rep("10-15 m3", 20), rep("15-20 m3", 20))
ggplot(data=B,aes(x=B$int3,y=B$dNormE,group=var,colour=var)) + geom_line() 
########################################################################################################################################################################################################################################################################################################################################################################################

# Quantitative Risk Assessment Plots
########################################################################################################################################################################################################################################################################################################################################################################################
# dataB = data.frame(dataB)
##QRA Table
          #write.table(QRA_Table,file="QRA1956.csv",sep=",",col.names=TRUE,row.names=FALSE)
# ##QRA/house
# QRA_Table = data.frame(QRA_Table)
# QRAhouse  = data.frame()
# for (i in 1:length(QRA_Table$Bat)){
# QRAhouse[i,1] = QRA_Table$Bat[i]
# QRAhouse[i,2] = sum(QRA_Table[i,c(2:20)])}
# colnames(QRAhouse) = c("NetNumber","QRA")
          #ALL                = full_join(dataB, QRAhouse, by ="NetNumber")
          #r01                = rasterFromXYZ(ALL[,c(1,2,4)])
          #writeRaster(r01,'QRAHouse2013.asc',format='ascii')

##QRA/volume
QRAVolume = data.frame()
for (i in 1:19){
QRAVolume[i,1] = VolumesClass[i]
QRAVolume[i,2] = sum(QRA_Table[,i+1])}
colour=c("lightyellow","navajowhite","navajowhite3","peachpuff4","lightblue2","lightpink1","darkolivegreen3","lightyellow","navajowhite","navajowhite3","peachpuff4","lightblue2","lightpink1","darkolivegreen3","navajowhite3","peachpuff4","lightblue2","lightpink1","darkolivegreen3")
barplot(QRAVolume$V2,col=colour,xlab="VolClass",ylab="QRA")
QRAVolume[,3]  = (QRAVolume[,2]*100)/sum(QRAVolume$V2)
barplot(QRAVolume$V3, ylim = c(0,50), col=colour,xlab="VolClass",ylab="QRA")

# QRAVolume[5,1] =  7.5; QRAVolume[5,2] = sum(QRA_Table[,c(6:10)])
# QRAVolume[6,1] = 12.5; QRAVolume[6,2] = sum(QRA_Table[,c(11:15)])
# QRAVolume[7,1] = 17.5; QRAVolume[7,2] = sum(QRA_Table[,c(16:20)])
# barplot(QRAVolume$V2,col=colour,xlab="VolClass",ylab="QRA")
########################################################################################################################################################################################################################################################################################################################################################################################

# Pz sensibility - Brut
########################################################################################################################################################################################################################################################################################################################################################################################
SimuNumber    = seq(0,10000,100)
AAA           = cut(dataRF3D_Scenario$NumSimu, SimuNumber)
AAA           = as.data.frame(table(AAA)); AAA[1,2] = length(which(dataRF3D_Scenario$NumSimu <= 100))
AAB           = cumsum(AAA[,2]); AAB = as.data.frame(AAB)
AAC           = cbind(SimuNumber[2:101], AAB)
colnames(AAC) = c("NbSimu","NbImpactsCum")
AAC[,2]       = AAC[,2]/(NbTot_CliffCells*(AAC[,1]))
PzTot         = AAC[100,2]
AAC[,2]       = (PzTot - AAC[,2])/PzTot
plot(AAC$NbSimu,AAC$NbImpactsCum, ylim = c(0.001,-0.011), pch=20, xlab="Nb simu", ylab="Pz Percent"); abline(h = 0, col="red")
########################################################################################################################################################################################################################################################################################################################################################################################
















