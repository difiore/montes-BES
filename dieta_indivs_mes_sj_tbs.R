rm(list=ls(all=T))
library(nlme)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggparl)
install.packages("ggbeeswarm")
library(ggbeeswarm)
install.packages("gghalves")
library(gghalves)
library(spacetime)



setwd("/Users/andresmontes/Documents/Andres_Academico/Articulo_agrupamiento")

# Tiputini porcentaje consumo indv ----------------------------------------


dietatbs<-read.delim("dieta_subgrupo_completo.txt",h=T)
head(dietatbs)

#seleccion de columnas de la base de datos general
dietaf<-select(dietatbs,Date,FECHA,Focal.Animal,sex,Duration, Part.Eaten)
dietaf$Duration<-as.numeric(dietaf$Duration)
head(dietaf)

##Hacer columnas de fecha independientes
month <- unlist(lapply(as.character(dietaf$Date), function(x){strsplit(x,"-")[[1]][2]}))
dietaf$month<-month

day <- as.numeric(unlist(lapply(as.character(dietaf$Date), function(x){strsplit(x,"-")[[1]][1]})))
dietaf$day<-day

year <- as.numeric(unlist(lapply(as.character(dietaf$Date), function(x){strsplit(x,"-")[[1]][3]})))
dietaf$year<-year

head(dietaf)

##cambiar nombres en el dataframe
dietaf$Part.Eaten<-gsub("fruits","Fruits",dietaf$Part.Eaten)
dietaf$Part.Eaten<-gsub("Spike","Other",dietaf$Part.Eaten)
dietaf$Part.Eaten<-gsub("Unknown","Other",dietaf$Part.Eaten)

##Vectores para comparación
#Vector individuo
dietaf$Focal.Animal<-as.factor(dietaf$Focal.Animal)
indvs<-levels(dietaf$Focal.Animal)
indvs<-indvs[c(-1,-2,-3,-32,-33)]
indvs


dietaf$Part.Eaten<-as.factor(dietaf$Part.Eaten)
item<-levels(dietaf$Part.Eaten)
item

#dietaf$Date<-as.Date(dietaf$Date,format="%d-%b-%Y")

years <-c(2006,2007,2008,2010,2011,2012,2013,2014,2015,2016,2017)
meses <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

##Mirar si hay NAs en los meses
dietaf[which(is.na(dietaf$month)),]

##Iterar
datos_dieta_mes <- data.frame()
for(i in 1:length(indvs)){
  individuo.i<- dietaf[which(dietaf$Focal.Animal==indvs[i]),]
  mes.dieta<-data.frame()
  for (j in 1:length(years)) {
    year.j <- individuo.i[which(individuo.i$year==years[j]),]
    for(k in 1:length(meses)){
      mes.k <- year.j[which(year.j$month==meses[k]),]
      if(nrow(mes.k)==0) {
        new.data.i <- data.frame("year"=years[j],"mes"=meses[k],"individuo"=indvs[i],"proporcion_fr"=NA,"proporcion_h"=NA,"proporcion_fl"=NA,"proporcion_o"=NA,"registros"=0, "n.dias"=0)
      }
      else{
        total.consumo<-sum(mes.k$Duration)
        frutos.pr <- filter(mes.k, Part.Eaten =="Fruits")
        sum.fr <- sum(frutos.pr$Duration)
        proporcionFR <-(sum.fr/total.consumo)
        hojas <- filter(mes.k, Part.Eaten =="Leaves")
        sum.ho <- sum(hojas$Duration)
        proporcionHO <-(sum.ho/total.consumo)
        flores <- filter(mes.k, Part.Eaten =="Flowers")
        sum.fl <- sum(flores$Duration)
        proporcionFl <-(sum.fl/total.consumo)
        otros <- filter(mes.k, Part.Eaten =="Other")
        sum.ot <- sum(otros$Duration)
        proporcionOT <-(sum.ot/total.consumo)
        registros <- nrow(mes.k)
        dias <- length(unique(mes.k[,"day"]))
        new.data.i <- data.frame("year"=years[j],"mes"=meses[k],"individuo"=indvs[i],"proporcion_fr"=proporcionFR,"proporcion_h"=proporcionHO,"proporcion_fl"=proporcionFl,"proporcion_o"=proporcionOT,"registros"=registros, "n.dias"=dias)
      }
      mes.dieta <- rbind(mes.dieta, new.data.i)
    }
    
  }
  datos_dieta_mes <- rbind(datos_dieta_mes,mes.dieta)
}

tbs_sinNA <- filter(datos_dieta_mes, !is.na(proporcion_fr))
tbs_mayor_1dia <- filter(tbs_sinNA, n.dias > 1)%>%filter(registros>10)

write.csv(tbs_mayor_1dia, "dieta_item_indiv_mes_tbs.csv")






###generar datospromedio por individuos por quincena para SAN JUAN


dietasj<-read.delim("datos_dieta_SanJuan.txt",h=T)
head(dietasj)

#seleccion de columnas de la base de datos general
dieta_f<-select(dietasj,Fecha.arreglada,Fecha,Individuo,Sexo,Duracion.evento,Item)
dieta_f$Duracion.evento <-as.numeric(dieta_f$Duracion.evento)
head(dieta_f)

##Hacer columnas de fecha independientes
month <- unlist(lapply(as.character(dieta_f$Fecha.arreglada), function(x){strsplit(x,"-")[[1]][1]}))
dieta_f$month<-month

day <- as.numeric(unlist(lapply(as.character(dieta_f$Fecha), function(x){strsplit(x,"-")[[1]][1]})))
dieta_f$day<-day

year <- as.numeric(unlist(lapply(as.character(dieta_f$Fecha.arreglada), function(x){strsplit(x,"-")[[1]][2]})))
dieta_f$year<-year

head(dieta_f)
dieta_f$Item <- as.factor(dieta_f$Item)
levels(dieta_f$Item)

##cambiar nombres en el dataframe
dieta_f$Item<-gsub("Frutos","Fruits",dieta_f$Item)
dieta_f$Item<-gsub("Flores","Flowers",dieta_f$Item)
dieta_f$Item<-gsub("Hojas","Leaves",dieta_f$Item)
dieta_f$Item<-gsub("Indeterminado","Other",dieta_f$Item)
dieta_f$Item<-gsub("Madera","Other",dieta_f$Item)
dieta_f$Item<-gsub("Otros","Other",dieta_f$Item)

##Vectores para comparación
#Vector individuo
dieta_f$Individuo<-as.factor(dieta_f$Individuo)
indvs<-levels(dieta_f$Individuo)
indvs<-indvs[c(-1)]
indvs

dieta_f$Item <- as.factor(dieta_f$Item)
item<-levels(dieta_f$Item)
item

years <-c(2006,2007,2008,2010,2011,2012)
meses <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")



##Mirar si hay NAs en los meses
dieta_f[which(is.na(dieta_f$month)),]

##Iterar
datos_dieta_mes_sj <- data.frame()
for(i in 1:length(indvs)){
  individuo.i<- dieta_f[which(dieta_f$Individuo==indvs[i]),]
  mes.dieta.sj<-data.frame()
  for (j in 1:length(years)) {
    year.j <- individuo.i[which(individuo.i$year==years[j]),]
    for(k in 1:length(meses)){
      mes.k <- year.j[which(year.j$month==meses[k]),]
      if(nrow(mes.k)==0) {
        new.data.i <- data.frame("year"=years[j],"mes"=meses[k],"individuo"=indvs[i],"proporcion_fr"=NA,"proporcion_h"=NA,"proporcion_fl"=NA,"proporcion_o"=NA,"registros"=0, "n.dias"=0)
      }
      else{
        total.consumo<-sum(mes.k$Duracion.evento)
        frutos <- filter(mes.k, Item =="Fruits")
        sum.fr <- sum(frutos$Duracion.evento)
        proporcionFR <-(sum.fr/total.consumo)
        hojas <- filter(mes.k, Item =="Leaves")
        sum.ho <- sum(hojas$Duracion.evento)
        proporcionHO <-(sum.ho/total.consumo)
        flores <- filter(mes.k, Item =="Flowers")
        sum.fl <- sum(flores$Duracion.evento)
        proporcionFl <-(sum.fl/total.consumo)
        otros <- filter(mes.k, Item =="Other")
        sum.ot <- sum(otros$Duracion.evento)
        proporcionOT <-(sum.ot/total.consumo)
        registros <- nrow(mes.k)
        dias <- length(unique(mes.k[,"day"]))
        new.data.i <- data.frame("year"=years[j],"mes"=meses[k],"individuo"=indvs[i],"proporcion_fr"=proporcionFR,"proporcion_h"=proporcionHO,"proporcion_fl"=proporcionFl,"proporcion_o"=proporcionOT,"registros"=registros, "n.dias"=dias)
      }
      mes.dieta.sj <- rbind(mes.dieta.sj, new.data.i)
    }
    
  }
  datos_dieta_mes_sj <- rbind(datos_dieta_mes_sj,mes.dieta.sj)
}

sj_sinNA <- filter(datos_dieta_mes_sj, !is.na(proporcion_fr))
sj_mayor_1dia <- filter(sj_sinNA, n.dias > 1)%>%filter(registros>10)

write.csv(sj_mayor_1dia, "dieta_item_indiv_mes_sj.csv")

