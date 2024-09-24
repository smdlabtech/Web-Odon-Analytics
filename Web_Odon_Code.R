#************************************************************************************************
#                       Projet: Etude des températures des cours d'eau 
#************************************************************************************************

# Chargement des libraries
library(lubridate)
library(fastICA)
library(dplyr)

# Recuperation des Coordonnées géographiques ----------------------------------------------------
# library(dplyr)
# 
# file_coord="C:/Users/mamad/Dropbox/UE_Prorjet_Partag/base/l-reseautemprivieres-d-r28.csv"
# cordonnes <- read.csv2(file_coord,header = T,encoding = 'UTF-8', sep = ",",stringsAsFactors = FALSE,na.strings = c(""," ","NA","N/A"))
# attach(cordonnes)
# 
# #Recuperations des coordonnées des 4 stations (T1,T2,T4 et T5)
# code<-cordonnes %>%
#   select(id_sonde,lib_sonde,lat_wgs84,lon_wgs84,x_lamb93,y_lamb93) %>%
#   filter(id_sonde %in% c("812","813","815","816"))
# 
# #Exportations de la table de coordonnees
# write.csv(code,file= 'coord_geo.csv',append = FALSE, quote = TRUE, sep = ",")

#------------------------------------------------------------------------------------------------
# Sources de donnees
file1="C:/Users/mamad/Dropbox/UE_Prorjet_Partag/base/OdonT1 812_2011-2017.csv"
file2="C:/Users/mamad/Dropbox/UE_Prorjet_Partag/base/OdonT2 813_2011-2017.csv"
file4="C:/Users/mamad/Dropbox/UE_Prorjet_Partag/base/OdonT4 815_2011-2017.csv"
file5="C:/Users/mamad/Dropbox/UE_Prorjet_Partag/base/OdonT5 816_2011-2017.csv"
#------------------------------------------------------------------------------------------------

#I) Importations, Nettoyages et Validations des donnees 
#************************************************************************************************
#                             Station OdonT1
#************************************************************************************************
OdonT1 <- read.csv2(file1,header = FALSE,encoding = 'UTF-8', sep = ",",stringsAsFactors = FALSE,na.strings = c(""," ","NA","N/A"))
attach(OdonT1)

OdonT1<-OdonT1[-1:-2,,]   #Suppression de lignes 1 et 2 dans les données
colnames(OdonT1)<-c("Indice","date","Heure","TempOd1")  #Renommer l'entête de la table
OdonT1
head(OdonT1)

#Recherche de Valeurs manquantes (NA) dans OdonT1
library(dplyr)
apply(OdonT1, MARGIN = 2, FUN = function(x){x%>%is.na%>%sum}) #Rech par colonne
#Il n'ya pas de valeurs manquantes dans la table OdonT1

#-- Mise en format des données 
OdonT1$Indice=as.integer(OdonT1$Indice)
OdonT1$TempOd1=as.double(OdonT1$TempOd1)

#Transformation de la date et l'heure
library(lubridate)
OdonT1$t <- paste(OdonT1$date, OdonT1$Heure)
OdonT1$t<-dmy_hms(OdonT1$t)
OdonT1$date=date(OdonT1$t)

#Statistique descriptives
summary(OdonT1)

#**************************************************************************
#                           Station OdonT2
#**************************************************************************
OdonT2 <- read.csv2(file2,header = FALSE,encoding = 'UTF-8', sep = ",",stringsAsFactors = FALSE,na.strings = c(""," ","NA","N/A"))
attach(OdonT2)
OdonT2<-OdonT2[-1:-2,,]   #Suppression de lignes 1 dans les données
colnames(OdonT2)<-c("Indice","date","Heure","TempOd2")  #Renommer l'entête de la table
OdonT2

#Recherche de Valeurs manquantes (NA) dans OdonT2
library(dplyr)
apply(OdonT2, MARGIN = 2, FUN = function(x){x%>%is.na%>%sum})
#Il n'ya pas de valeurs manquantes dans las table OdonT2

#-- Mise en format des données 
OdonT2$Indice=as.integer(OdonT2$Indice)
OdonT2$TempOd2=as.double(OdonT2$TempOd2)

#Transformation de la date et l'heure
library(lubridate)
OdonT2$t <- paste(OdonT2$date, OdonT2$Heure)
OdonT2$t<-dmy_hms(OdonT2$t)
OdonT2$date=date(OdonT2$t)

#Statistique descriptives
summary(OdonT2)


#***************************************************************************************
#                                 Station OdonT4
#***************************************************************************************
OdonT4 <- read.csv2(file4,header = FALSE,encoding = 'UTF-8', sep = ",",stringsAsFactors = FALSE,na.strings = c(""," ","NA","N/A"))
attach(OdonT4)
OdonT4<-OdonT4[-1:-2,,]   #Suppression de lignes 1 dans les données
colnames(OdonT4)<-c("Indice","date","Heure","TempOd4")  #Renommer l'entête de la table
OdonT4

#Recherche de Valeurs manquantes (NA) dans OdonT4
library(dplyr)
apply(OdonT4, MARGIN = 2, FUN = function(x){x%>%is.na%>%sum})
#Il n'ya pas de valeurs manquantes dans las table OdonT4

#-- Mise en format des données 
OdonT4$Indice=as.integer(OdonT4$Indice)
OdonT4$TempOd4=as.double(OdonT4$TempOd4)

#Transformation de la date et l'heure
library(lubridate)
OdonT4$t <- paste(OdonT4$date, OdonT4$Heure)
OdonT4$t<-dmy_hms(OdonT4$t)
OdonT4$date=date(OdonT4$t)


#Statistique descriptives
summary(OdonT4)

#**************************************************************************
#                     Station OdonT5
#**************************************************************************
OdonT5 <- read.csv2(file5,header = FALSE,encoding = 'UTF-8', sep = ",",stringsAsFactors = FALSE,na.strings = c(""," ","NA","N/A"))
attach(OdonT5)
OdonT5<-OdonT5[-1:-2,,]   #Suppression de lignes 1 et dans les données
colnames(OdonT5)<-c("Indice","date","Heure","TempOd5")  #Renommer l'entête de la table
OdonT5

#Recherche de Valeurs manquantes (NA) dans OdonT5
library(dplyr)
apply(OdonT5, MARGIN = 2, FUN = function(x){x%>%is.na%>%sum})
#Il n'ya pas de valeurs manquantes dans las table OdonT5

#-- Mise en format des données 
OdonT5$Indice=as.integer(OdonT5$Indice)
OdonT5$TempOd5=as.double(OdonT5$TempOd5)

#Transformation de la date et l'heure
library(lubridate)
OdonT5$t <- paste(OdonT5$date, OdonT5$Heure)
OdonT5$t<-dmy_hms(OdonT5$t)
OdonT5$date=date(OdonT5$t)

#Statistique descriptives
summary(OdonT5)

#**************************************************************************
# Merge des tables --------------------------------------------------------
#**************************************************************************
library(rlang)
library(tidyverse)
library(lubridate)
library(dplyr)

# Moyenne aggregate par jour
OdonT1ag<-aggregate(TempOd1~date, data=OdonT1, FUN = mean)
OdonT2ag<-aggregate(TempOd2~date, data=OdonT2, FUN = mean, na.rm=TRUE)
OdonT4ag<-aggregate(TempOd4~date, data=OdonT4, FUN = mean, na.rm=TRUE)
OdonT5ag<-aggregate(TempOd5~date, data=OdonT5, FUN = mean, na.rm=TRUE)

#Merge des différentes bases de données (avec date et l'heure) 
#pour obtenir une seule base de données
fusion1<-merge(OdonT1ag,OdonT2ag, by="date")
fusion2<-merge(fusion1,OdonT4ag, by="date")
base<-merge(fusion2,OdonT5ag, by="date")    #base de données principale

#base: Ce fichier contiendra les variables suivantes:
#date, TempOd1, TempOd2, TempOd4 et TempOd5

#----------------------- Exporter la base de donnees---------------------
#Celle-ci constituera le fichier (CSV data.file ) qu'on utilisera dans l'application 
# write.csv(base,file= 'base_bdd.csv',append = FALSE, quote = TRUE, sep = ",")

#*******************************************************************************
#         1°) Analyses descriptives des sites(Stations)----------------------------------------------
#*******************************************************************************
par(mfrow=c(2,2))
# Traçage de le courbe des températures ---------------------------------------------------------------------
plot(TempOd1~date, type="l", data=OdonT1ag, col="red", xlab="date", ylab = "Températures",main="Températures du site 812 entre 2011 et 2017")
plot(TempOd2~date, type="l", data=OdonT2ag, col="blue", xlab="date", ylab = "Températures",main="Températures du site 813 entre 2011 et 2017")
plot(TempOd4~date, type="l", data=OdonT4ag, col="green", xlab="date", ylab = "Températures",main="Températures du site 815 entre 2011 et 2017")
plot(TempOd5~date, type="l", data=OdonT5ag, col="yellow", xlab="date", ylab = "Températures",main="Températures du site 816 entre 2011 et 2017")

#Summary des données
summary(OdonT1ag$TempOd1)
summary(OdonT2ag$TempOd2)
summary(OdonT4ag$TempOd4)
summary(OdonT5ag$TempOd5)
summary(base) #résumé stats sur la base principale

#----------- Autre methode de calcul du summary --------
# library(dplyr)
# base %>%
#   summarise_each(funs(min,median,mean,max, sd), TempOd1,
#                  TempOd2, TempOd4,TempOd5)

#----- Histogrammes ------
par(mfrow=c(2,2))
#Histogramme OdonT1
hist(OdonT1ag$TempOd1,col='blue',xlab = 'Température', main="Histogramme des températures d'Od1 en ")
abline(v=mean(OdonT1ag$TempOd1),col='black',lty=3 , lwd = 3)
text(mean(OdonT1ag$TempOd1),y=6.5,paste("La temp moyenne est égale à 10.66 "))

#Histogramme OdonT2
hist(OdonT2ag$TempOd2,col='red',xlab = 'Température', main="Histogramme des températures d'Od2 ")
abline(v=mean(OdonT2ag$TempOd2),col='black',lty=3 , lwd = 3)
text(mean(OdonT2ag$TempOd2),y=6.5,paste("La temp moyenne est égale à 11.51 "))

#Histogramme OdonT4
hist(OdonT4ag$TempOd4,col='red',xlab = 'Température', main="Histogramme des températures d'Od4 ")
abline(v=mean(OdonT4ag$TempOd4),col='black',lty=3 , lwd = 3)
text(mean(OdonT4ag$TempOd4),y=6.5,paste("La temp moyenne est égale à 11.48 "))

#Histogramme OdonT5
hist(OdonT5ag$TempOd5,col='blue',xlab = 'Température', main="Histogramme des températures d'Od5 ")
abline(v=mean(OdonT5ag$TempOd5),col='black',lty=3 , lwd = 3)
text(mean(OdonT5ag$TempOd5),y=6.5,paste("La temp moyenne est égale à 12.08 "))

#**************************************************************************
#             2°)  Décomposition en séries temporelles 
#**************************************************************************
library(lubridate)
OdonT1ag$year=year(OdonT1ag$date)
OdonT2ag$year=year(OdonT2ag$date)
OdonT4ag$year=year(OdonT4ag$date)
OdonT5ag$year=year(OdonT5ag$date)

#Transformation de la date en format journalier

#**************************************************************************
#              3°) ACI: Analyse en composantes indépendantes
#**************************************************************************
#Composantes journalières
library(lubridate)
OdonT1$date=ymd(OdonT1$date)
OdonT2$date=ymd(OdonT2$date)
OdonT4$date=ymd(OdonT4$date)
OdonT5$date=ymd(OdonT5$date)

# Moyenne aggregate par jour (Tendance par jour)
OdonT1ag<-aggregate(TempOd1~date, data=OdonT1, FUN = mean)
OdonT2ag<-aggregate(TempOd2~date, data=OdonT2, FUN = mean, na.rm=TRUE)
OdonT4ag<-aggregate(TempOd4~date, data=OdonT4, FUN = mean, na.rm=TRUE)
OdonT5ag<-aggregate(TempOd5~date, data=OdonT5, FUN = mean, na.rm=TRUE)

#Merge des différentes bases de données (avec date et l'heure) 
#pour obtenir une seule base de données
fusion1<-merge(OdonT1ag,OdonT2ag, by="date")
fusion2<-merge(fusion1,OdonT4ag, by="date")
base<-merge(fusion2,OdonT5ag, by="date")    #base de données principale

base2<-select(base,-date) #base avec temperature et sans la date
base3<-select(base,date)  #table contenant uniquement les dates

summary(base2)

#---------------------------------------------------------------
#              Applications de la FastICA
#---------------------------------------------------------------
set.seed(1)
#Suivant journalière et saisonnière
a <- fastICA(base2, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)

#2 MatriceS passage A et des sources S
a$A
#Cette matrice nous permet d'obtenir les resultats respectivement suivant chaque ligne
#la composantes saisonniere et la composante journaliere
#Et les 4 colonnes representent les 4 stations

a$S
matA<-data.frame(a$S) #Création d'une dataframe
matB<-cbind(base3, matA) #création d'une matrice B avec les dates

#3 représentation des 2 signaux
par(mfrow = c(1,1))
plot(X1~date, type="l", col="blue", data=matB)
plot(X2~date, type="l", col="red", data=matB)

matB$comp1=a$A[1,1]*a$S[,1]
matB$comp2=a$A[2,1]*a$S[,2]

#Representation des 2 composantes de la première variable (OdonT1)
par(mfrow = c(1,2))
plot(comp1~date, type="l", data=matB, col="blue") 
plot(comp2~date, type="l", data=matB, col="red")

matB$Z=matB$comp1+matB$comp2+mean(OdonT1ag$TempOd1)
matB$Z

