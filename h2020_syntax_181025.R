library(readxl)
library(polycor)
library(psycho)
library(nFactors)
library(ClustOfVar)
library(ape)

setwd("~/h2020")

cordis_h2020projects <- read_excel("cordis-h2020projects.xlsx")
mydata<-cordis_h2020projects
attach(mydata)
table(coordinatorCountry)
write.table(table(coordinatorCountry), file="countries.csv")
detach(mydata)

mydata$cn_AT<-grepl('AT',mydata$participantCountries)
mydata$cn_BE<-grepl('BE',mydata$participantCountries)
mydata$cn_BG<-grepl('BG',mydata$participantCountries)
mydata$cn_CY<-grepl('CY',mydata$participantCountries)
mydata$cn_CZ<-grepl('CZ',mydata$participantCountries)
mydata$cn_DE<-grepl('DE',mydata$participantCountries)
mydata$cn_DK<-grepl('DK',mydata$participantCountries)
mydata$cn_EE<-grepl('EE',mydata$participantCountries)
mydata$cn_EL<-grepl('EL',mydata$participantCountries)
mydata$cn_ES<-grepl('ES',mydata$participantCountries)
mydata$cn_FI<-grepl('FI',mydata$participantCountries)
mydata$cn_FR<-grepl('FR',mydata$participantCountries)
mydata$cn_HR<-grepl('HR',mydata$participantCountries)
mydata$cn_HU<-grepl('HU',mydata$participantCountries)
mydata$cn_IE<-grepl('IE',mydata$participantCountries)
mydata$cn_IT<-grepl('IT',mydata$participantCountries)
mydata$cn_LT<-grepl('LT',mydata$participantCountries)
mydata$cn_LU<-grepl('LU',mydata$participantCountries)
mydata$cn_LV<-grepl('LV',mydata$participantCountries)
mydata$cn_MT<-grepl('MT',mydata$participantCountries)
mydata$cn_NL<-grepl('NL',mydata$participantCountries)
mydata$cn_PL<-grepl('PL',mydata$participantCountries)
mydata$cn_PT<-grepl('PT',mydata$participantCountries)
mydata$cn_RO<-grepl('RO',mydata$participantCountries)
mydata$cn_SE<-grepl('SE',mydata$participantCountries)
mydata$cn_SI<-grepl('SI',mydata$participantCountries)
mydata$cn_SK<-grepl('SK',mydata$participantCountries)
mydata$cn_UK<-grepl('UK',mydata$participantCountries)


attach(mydata)
mydata$cn_AT[coordinatorCountry == 'AT'] <- TRUE
mydata$cn_BE[coordinatorCountry == 'BE'] <- TRUE
mydata$cn_BG[coordinatorCountry == 'BG'] <- TRUE
mydata$cn_CY[coordinatorCountry == 'CY'] <- TRUE
mydata$cn_CZ[coordinatorCountry == 'CZ'] <- TRUE
mydata$cn_DE[coordinatorCountry == 'DE'] <- TRUE
mydata$cn_DK[coordinatorCountry == 'DK'] <- TRUE
mydata$cn_EE[coordinatorCountry == 'EE'] <- TRUE
mydata$cn_EL[coordinatorCountry == 'EL'] <- TRUE
mydata$cn_ES[coordinatorCountry == 'ES'] <- TRUE
mydata$cn_FI[coordinatorCountry == 'FI'] <- TRUE
mydata$cn_FR[coordinatorCountry == 'FR'] <- TRUE
mydata$cn_HR[coordinatorCountry == 'HR'] <- TRUE
mydata$cn_HU[coordinatorCountry == 'HU'] <- TRUE
mydata$cn_IE[coordinatorCountry == 'IE'] <- TRUE
mydata$cn_IT[coordinatorCountry == 'IT'] <- TRUE
mydata$cn_LT[coordinatorCountry == 'LT'] <- TRUE
mydata$cn_LU[coordinatorCountry == 'LU'] <- TRUE
mydata$cn_LV[coordinatorCountry == 'LV'] <- TRUE
mydata$cn_MT[coordinatorCountry == 'MT'] <- TRUE
mydata$cn_NL[coordinatorCountry == 'NL'] <- TRUE
mydata$cn_PL[coordinatorCountry == 'PL'] <- TRUE
mydata$cn_PT[coordinatorCountry == 'PT'] <- TRUE
mydata$cn_RO[coordinatorCountry == 'RO'] <- TRUE
mydata$cn_SE[coordinatorCountry == 'SE'] <- TRUE
mydata$cn_SI[coordinatorCountry == 'SI'] <- TRUE
mydata$cn_SK[coordinatorCountry == 'SK'] <- TRUE
mydata$cn_UK[coordinatorCountry == 'UK'] <- TRUE

mydata_cn <- mydata[c(22:49)]


#LEÍRÓ ELEMZÉS
attach(cordis_h2020projects)
table(frameworkProgramme)
sum(totalCost, na.rm=TRUE)
sum(ecMaxContribution, na.rm=TRUE)
sum(ecMaxContribution, na.rm=TRUE)/sum(totalCost, na.rm=TRUE)
table(coordinatorCountry)
cordis_h2020projects$nparticipant <- "Egy"
cordis_h2020projects$nparticipant[nchar(cordis_h2020projects$participantCountries)>0] <- "Több"
attach(cordis_h2020projects)
table(nparticipant)

#EGY FŐRE JUTÓ TÁMOGATÁS ORSZÁGONKÉNT
write.table(tapply(cordis_h2020projects$ecMaxContribution, cordis_h2020projects$coordinatorCountry, FUN=sum), file="sumpercountry.csv")
cordis_h2020projects$finratio<-cordis_h2020projects$ecMaxContribution/cordis_h2020projects$totalCost
write.table(tapply(cordis_h2020projects$finratio, cordis_h2020projects$coordinatorCountry, FUN=mean), file="finratiopercountry.csv")


#FAKTORELEMZÉS
faktor <- sapply(mydata_cn, as.factor)
het.mat <- hetcor(faktor)$cor

fa.1 <- factanal(covmat = het.mat, factors = 2, rotation = "varimax")
fa.1


#MAGYAR KOORDINÁTOROK
HUN <- cordis_h2020projects[which(cordis_h2020projects$coordinatorCountry=="HU"), ] 
write.table(HUN, file="HUN.csv")

#MAGYAR KAPCSOLATERŐSSÉGEK
write.table(het.mat, file="CORR.csv")

#KLASZTERANALÍZIS

cluster <- sapply(mydata_cn, as.numeric)

orszagnev <- c("Ausztria", "Belgium", "Bulgária", "Ciprus", "Csehország", "Németország", "Dánia", "Észtország", "Görögország", "Spanyolország", "Finnország", "Franciaország", "Horvátország", "Magyarország", "Írország", "Olaszország", "Lettország", "Luxemburg", "Litvánia", "Málta", "Hollandia", "Lengyelország", "Portugália", "Románia", "Svédország", "Szlovénia", "Szlovákia", "Egyesült Királyág")

colnames(cluster) <- orszagnev

tree <- hclustvar(cluster)
plot(tree)

hcd <- as.dendrogram(tree)

colors = c("red", "blue")
clus2 = cutree(tree, 2)
plot(as.phylo(tree), type = "fan", tip.color = colors[clus2], font=2)