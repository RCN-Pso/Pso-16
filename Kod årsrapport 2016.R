setwd("H:/Martin/PsoReg/Projekt PsoReg årsrapport 2016/årsrapport 2016 WD")

library(stringr)

#Läser in data med kontakter från PsoReg
Pso_contact <- read.delim("Uttag Årsrapport1.txt",header=TRUE,sep=";",encoding="UTF-8")


#namnger variablerna ändra datatyper, plockar bort testpatienter och plockar bort kontakter utan kontaktdatum
names(Pso_contact) <- c("OrganisationName","ContactDate","PERSNR","RegisteredAt")
Pso_contact$OrganisationName <- as.character(Pso_contact$OrganisationName)
Pso_contact <- Pso_contact[!(Pso_contact$OrganisationName=="Test"),]
Pso_contact$ContactDate <- as.Date(Pso_contact$ContactDate)
Pso_contact <- subset(Pso_contact, is.na(ContactDate)==FALSE)

#Sorterar datat och skapar en variabel som indikerar de unika individerna dupli_PatientID=FALSE=första kontakten per individ
Pso_contact <- Pso_contact[order(Pso_contact$ContactDate,decreasing=FALSE),]
Pso_contact <- Pso_contact[order(Pso_contact$PERSNR,decreasing=FALSE),]
Pso_contact$dupli_PatientID <- duplicated(Pso_contact$PERSNR)

#skapar dat
Pso_contact$Contact_year <- 1900 + as.POSIXlt(Pso_contact$ContactDate)$year
Pso_contact$Contact_moth <-1 + as.POSIXlt(Pso_contact$ContactDate)$mon
Pso_contact$Contact_year_moth <- paste(as.character(Pso_contact$Contact_year),as.character(Pso_contact$Contact_moth),sep ="")

Pso_contact$RegisteredAt <- as.character(Pso_contact$RegisteredAt)
Pso_contact$RegisteredAt[Pso_contact$RegisteredAt==""] <- "2013-12-24"

Pso_contact$RegisteredAt <- as.Date(Pso_contact$RegisteredAt)
Pso_contact$Registred_year <- 1900 + as.POSIXlt(Pso_contact$RegisteredAt)$year
Pso_contact$Registred_moth <-1 + as.POSIXlt(Pso_contact$RegisteredAt)$mon
Pso_contact$Registred_year_moth <- paste(as.character(Pso_contact$Registred_year),as.character(Pso_contact$Registred_moth),sep ="")

#Skapar dataset med den senaste kontakten per individ
Pso_individ<-subset(Pso_contact,dupli_PatientID==FALSE)
###############

#####Läser in fil med systembehandlingar#####

#Läser in data med PsoReg data för att skapa dataset med behandlingar
Pso_behandling <- read.csv("Uttag Årsrapport2.txt",header=TRUE,sep=";",encoding="UTF-8")
Pso_behandling <- Pso_behandling[as.numeric(substr(Pso_behandling$X.U.FEFF.PERSNR,1,8))>19130101,]

#Byter variabelnamn och ändrar format på personnummer
names(Pso_behandling) <- c("PERSNR","DrugName","InsertDate","RemoveDate","biologisk")
Pso_behandling$PERSNR <- gsub("[^0-9]", "", Pso_behandling$PERSNR) Pso_behandling <- merge(Pso_behandling,Pso_individ,all.x=TRUE, all.y=FALSE,by.x="PERSNR",by.y="PERSNR")

#Pso_behandling <- with(Pso_behandling,data.frame(PatientID2,DrugName,InsertDate,RemoveDate,Region,Vårdnivå,Ålder,OrganisationName,ShortName,Department,PostalCode,PostOffice,ContactDate,ny_gamal_registrering))
#Pso_behandling$InsertDate <-substr(Pso_behandling$InsertDate, 1, 10)
#Pso_behandling$RemoveDate <-substr(Pso_behandling$RemoveDate, 1, 10)

#Lite småfix
Pso_behandling$DrugName <- as.character(Pso_behandling$DrugName)
Pso_behandling$DrugName[Pso_behandling$DrugName=="Metotrexat (injektion)"] <- "Metotrexat"
Pso_behandling$DrugName[Pso_behandling$DrugName=="Metotrexat (tablett)"] <- "Metotrexat"
Pso_behandling$DrugName[Pso_behandling$DrugName=="PUVA (tablett)"] <- "PUVA"
Pso_behandling$RemoveDate <- as.character(Pso_behandling$RemoveDate)


###Skapar dataset med endast Metotrexatbehandlingar
Pso_mtx <- Pso_behandling[Pso_behandling$DrugName==c("Metotrexat"),]
## Sätter ett datum på Removedate på de behandlingar som inte än är utskrivna 
Pso_mtx$RemoveDate[Pso_mtx$RemoveDate==""] <- "2018-06-25"

##Skapar ett dataset med endast biologiska behandlingar
Pso_biologisk <- subset(Pso_behandling, biologisk="Ja")


Pso_bio <- Pso_biologisk[order(Pso_biologisk$InsertDate,decreasing=TRUE),]
Pso_bio <- Pso_bio[order(Pso_bio$PERSNR,decreasing=FALSE),]
Pso_bio$dupli_PatientID <- duplicated(Pso_bio$PERSNR)

Pso_bio<-subset(Pso_bio,dupli_PatientID==FALSE)

Pso_bio$RemoveDate[Pso_bio$RemoveDate==""] <- "2015-06-25"


Pso_mtx$ID_kontroll <- Pso_mtx$PERSNR



