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

