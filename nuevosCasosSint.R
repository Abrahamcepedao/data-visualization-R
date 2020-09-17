rm(list=ls())

library(readxl)
library(plyr)
library(dplyr)
library(stringr)
library(sqldf)
library(ggplot2)
library(scales)

setwd("~/Documents/OnCampusJob/nuevos-casos-covid-19")


## Cambiar nombre de archivo y correr
######################################################
archivo <- "200916COVID19MEXICO.csv"
#######################################################

month <- substr(archivo, 3,4)
day <- substr(archivo, 5,6)

d <- read.csv(archivo, stringsAsFactors = FALSE)

mun <- read.table("municipios_todos.txt", sep=",", stringsAsFactors = FALSE, header=FALSE)
cnames <- mun[1,]
mun <- mun[2:nrow(mun),]
colnames(mun) <- cnames

d$ENTIDAD_RES2 <- ifelse(nchar(d$ENTIDAD_RES)==1, paste("0", d$ENTIDAD_RES, sep=""), as.character(d$ENTIDAD_RES))

d$MUNICIPIO_RES2 <- ifelse(nchar(d$MUNICIPIO_RES)==1, paste("00", d$MUNICIPIO_RES, sep=""), 
                           ifelse(nchar(d$MUNICIPIO_RES)==2, paste("0", d$MUNICIPIO_RES, sep="") , as.character(d$MUNICIPIO_RES)))

d$CLAVEGEO <- paste(d$ENTIDAD_RES2, d$MUNICIPIO_RES2, sep="")
d$CLAVEGEO <- ifelse(d$MUNICIPIO_RES2=="999", "99999", d$CLAVEGEO)
d$CLAVEGEO <- ifelse(!d$CLAVEGEO %in% mun$CVEGEO, "99999", d$CLAVEGEO)

d$POSITIVOS <- ifelse(d$RESULTADO==1, 1, 0)
d$DEFUNCION <- ifelse(d$FECHA_DEF=="9999-99-99" | d$FECHA_DEF=="", 0, 1)

positivos <- d[d$POSITIVOS==1,]

nac <- data.frame(table(positivos$FECHA_SINTOMAS))
dec <- data.frame(table(positivos$FECHA_DEF))

head(nac)
colnames(nac) <- c("fecha","casos")

edos <- data.frame(table(positivos$ENTIDAD_RES2, positivos$FECHA_SINTOMAS))
head(edos)
colnames(edos) <- c("entidad","fecha","casos")
edos$fecha <- as.character(edos$fecha)

dec <- data.frame(table(positivos$ENTIDAD_RES2, positivos$FECHA_DEF))
head(dec)
colnames(dec) <- c("entidad","fecha","decesos")
dec$fecha <- as.character(dec$fecha)

pruebas <- data.frame(table(d$ENTIDAD_RES2, d$FECHA_INGRESO))
head(pruebas)
colnames(pruebas) <- c("entidad","fecha","pruebas")
pruebas$fecha <- as.character(pruebas$fecha)

fechavec <- data.frame(sort(unique(c(edos$fecha, pruebas$fecha))))
colnames(fechavec) <- "fecha"
head(fechavec)

edosvec <- data.frame(unique(pruebas$entidad))
colnames(edosvec) <- "entidad"
head(edosvec)

df <- sqldf("SELECT * FROM fechavec, edosvec")
df$fecha <- as.character(df$fecha)
edos$fecha <- as.character(edos$fecha)
dec$fecha <- as.character(dec$fecha)
pruebas$fecha <- as.character(pruebas$fecha)

df2 <- sqldf("SELECT df.fecha, df.entidad, casos, decesos, pruebas
            FROM df
            LEFT OUTER JOIN edos ON (df.fecha=edos.fecha AND df.entidad=edos.entidad)
            LEFT OUTER JOIN dec ON (df.fecha=dec.fecha AND df.entidad=dec.entidad)
            LEFT OUTER JOIN pruebas ON (df.fecha=pruebas.fecha AND df.entidad=pruebas.entidad)")


df2$entidad <- as.integer(df2$entidad)

df2$fecha <- as.Date(df2$fecha, format="%Y-%m-%d")
df2[is.na(df2)] <- 0
df3 <- df2[df2$fecha >= "2020-03-15" & df2$fecha <= "2020-09-02", ]
df3 <- df3[order(df3$entidad, df3$fecha),]

df3$fecha <- format(df3$fecha, "%d/%m/%Y")

write.csv(df3, "iniciosint_fechadec.csv", row.names=FALSE)

#
nac$fecha <- as.Date(nac$fecha, format="%Y-%m-%d")
edos$fecha <- as.Date(edos$fecha, format="%Y-%m-%d")
edos$entidad <- as.integer(as.character(edos$entidad))

nac <- nac[order(nac$fecha),]
nac$mv <- 0

edos <- edos[order(edos$entidad, edos$fecha),]

nac <- nac[nac$fecha >= "2020-03-15" & nac$fecha <= "2020-09-02", ]
edos <- edos[edos$fecha >= "2020-03-15" & edos$fecha <= "2020-09-02", ]

row.names(nac) <- 1:nrow(nac)
row.names(edos) <- 1:nrow(edos)

fechasnac <- data.frame(fecha=nac$fecha)
edostod <- data.frame(edos = unique(edos$entidad))

d2 <- sqldf("SELECT fechasnac.*, edostod.* FROM fechasnac, edostod")

edos2 <- sqldf("SELECT d2.fecha, d2.edos AS entidad, edos.casos FROM d2 LEFT OUTER JOIN edos ON (d2.fecha=edos.fecha AND d2.edos=edos.entidad)
               ORDER BY d2.edos, d2.fecha")


head(nac)

plot1 <- ggplot(nac, aes(x=fecha, y=casos)) 

plot1 + geom_bar(stat = "identity", position="identity") +
    ggtitle("Número de casos Covid-19 por fecha de inicio de síntomas (Nacional)") +
    xlab("Fecha de inicio de síntomas") + ylab("Número de casos") +
    scale_x_date(labels=date_format ("%b %d"), breaks=breaks_width("2 weeks")) +
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
    theme(text = element_text(size=18)) + stat_smooth(colour="green") + 
    geom_vline(xintercept=as.numeric(nac$fecha[93]), col="red", size=2) 
    #geom_text(aes(x=fecha[80], label="\nApertura", y=5000), colour="red", angle=90, size=6)
  
df3$fecha <- as.Date(df3$fecha, format="%d/%m/%Y")


## Nivel de positividad.
plot2 <- ggplot(df3[df3$entidad==25,], aes(x=fecha, y=casos))

plot2 + geom_bar(stat = "identity", position="identity") +
  ggtitle("Número de casos Covid-19 por fecha de inicio de síntomas (Sinaloa)") +
  xlab("Fecha de inicio de síntomas") + ylab("Número de casos") +
  scale_x_date(labels=date_format ("%b %d"), breaks=breaks_width("2 weeks")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18)) + stat_smooth(colour="green") + 
  geom_vline(xintercept=as.numeric(nac$fecha[93]), col="red", size=2) 
  #geom_text(aes(x=fecha[80], label="\nApertura", y=200), colour="red", angle=90, size=6)




plot3 <- ggplot(edos2[edos2$entidad==31,], aes(x=fecha, y=casos))

plot3 + geom_bar(stat = "identity", position="identity") +
  ggtitle("Número de casos Covid-19 por fecha de inicio de síntomas (Yucatán)") +
  xlab("Fecha de inicio de síntomas") + ylab("Número de casos") +
  scale_x_date(labels=date_format ("%b %d"), breaks=breaks_width("2 weeks")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18)) + stat_smooth(colour="green") + 
  geom_vline(xintercept=as.numeric(nac$fecha[79]), col="red", size=2)  
  #geom_text(aes(x=fecha[80], label="\nApertura", y=250), colour="red", angle=90, size=6)


plot3 <- ggplot(edos2[edos2$entidad==32,], aes(x=fecha, y=casos))

plot3 + geom_bar(stat = "identity", position="identity") +
  ggtitle("Número de casos Covid-19 por fecha de inicio de síntomas (Zacatecas)") +
  xlab("Fecha de inicio de síntomas") + ylab("Número de casos") +
  scale_x_date(labels=date_format ("%b %d"), breaks=breaks_width("2 weeks")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18)) + stat_smooth(colour="green")  +
  geom_vline(xintercept=as.numeric(nac$fecha[79]), col="red", size=2) 
  #geom_text(aes(x=fecha[80], label="\nApertura", y=700), colour="red", angle=90, size=6)


##############################################################################################
### NYC


nyc <- read.csv("./nyc/data-yqfdF.csv", stringsAsFactors = FALSE)
nyc$DATE_OF_INTEREST <- as.Date(nyc$DATE_OF_INTEREST, format="%m/%d/%Y")

nyc2 <- nyc[nyc$DATE_OF_INTEREST>="2020-03-15" & nyc$DATE_OF_INTEREST<="2020-06-15",]
row.names(nyc2) <- 1:nrow(nyc2)

plot1 <- ggplot(nyc2, aes(x=DATE_OF_INTEREST, y=Cases)) 

plot1 + geom_bar(stat = "identity", position="identity") + stat_smooth(colour="green") + ylim(0, max(nyc2$Cases))  +
geom_vline(xintercept=as.numeric(nyc2$DATE_OF_INTEREST[86]), col="red", size=2) +
  ggtitle("Número de nuevos casos Covid-19 por fecha (New York City)") +
  xlab("Nuevos casos por día") + ylab("Número de casos") +
  scale_x_date(labels=date_format ("%b %d"), breaks=breaks_width("2 weeks")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18)) +
  geom_text(aes(x=DATE_OF_INTEREST[87], label="\nApertura", y=3000), colour="red", angle=90, size=6)

