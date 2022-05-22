#install.packages(c('httr','foreign','rjstat','plyr','dplyr'))
library(jsonlite)
library(httr)
library(foreign)
library(rjstat)
library(plyr)
library(dplyr)

options(encoding="UTF-8")

#Solveigs
setwd("C:/Users/solme/OneDrive/Master/Endelig datasett")

#Majas
#setwd("~/Documents/Data master")

################################################################################
#####Danner grunnlaget for det sammensatte datasettet#####

#Henter HELE gjennomføring og frafall (studiumnivå)
grunnlag <- read.csv('20220310-undefined-Gjennomføring og frafall (studiumnivå).csv', sep = ';')

#Fjerner eventuelle duplikater
grunnlag <- data.frame(unique(grunnlag))

names(grunnlag)[3] <- "Avdelingskode"

#Gjør relevante kolonner nummeriske: (2-tallet står for kolonne, bruker 1 dersom det skal vø¦re rader) 
grunnlag[,c(15:24)] <- apply(grunnlag[,c(15:24)],2,function(x) as.numeric(as.character(x)))

#Fjerner alle med startkull = 0
grunnlag <- grunnlag[!(grunnlag$Startkull == 0),]

#Fjerner alle fakultet 
grunnlag <- grunnlag[!grepl('Fakultet', grunnlag$Avdelingsnavn),]
grunnlag <- grunnlag[!grepl('fakultet', grunnlag$Avdelingsnavn),]

#Fjerner "programområder" fordi det gir studiekompetanse (er altså ikke høyere utdanning)
grunnlag <- grunnlag[!grepl("Programområde", grunnlag$Avdelingsnavn),]

#Velger de nivåkodene vi skal fokusere på
grunnlag <- grunnlag[(grunnlag$Nivåkode == 'B3'|
                         grunnlag$Nivåkode == 'B4'|
                         grunnlag$Nivåkode =='M2'| 
                         grunnlag$Nivåkode == 'M5'),]

#Undersøker om det er mer som skal slettes som ikke inneholder institutt
#test <- grunnlag[!grepl("Institutt", grunnlag$Avdelingsnavn),]
#test <- test[!grepl("institutt", test$Avdelingsnavn),]
#Konklusjon: trenger ikke slette mer

#Antall menn/kvinner totalt: 
grunnlag <- data.frame(
  aggregate(cbind(
    Startkull, Frafalt)
    ~ Institusjonskode + Institusjonsnavn + Avdelingskode 
    + Avdelingsnavn + Studiumkode + Studnavn + Nivåkode
    + Årstall + Årstall.normert.tid + Kjønn.1, 
    data = grunnlag, FUN = sum)
)

grunnlag$Antall.K <- NA 
for (i in 1:nrow(grunnlag)){
  ifelse(grunnlag$Kjønn.1[i] == 'K', 
         grunnlag$Antall.K[i] <- grunnlag$Startkull[i],
         grunnlag$Antall.K[i] <- 0)
}

grunnlag$Antall.M <- NA
for (i in 1:nrow(grunnlag)){
  ifelse(grunnlag$Kjønn.1[i] == 'M', 
         grunnlag$Antall.M[i] <- grunnlag$Startkull[i],
         grunnlag$Antall.M[i] <- 0)
}

#Antall kvinner/menn som er frafalt:
grunnlag$Antall.K.F <- NA
for (i in 1:nrow(grunnlag)){
  ifelse(grunnlag$Kjønn.1[i] == 'K', 
         grunnlag$Antall.K.F[i] <- grunnlag$Frafalt[i],
         grunnlag$Antall.K.F[i] <- 0)
}

grunnlag$Antall.M.F <- NA
for (i in 1:nrow(grunnlag)){
  ifelse(grunnlag$Kjønn.1[i] == 'M', 
         grunnlag$Antall.M.F[i] <- grunnlag$Frafalt[i],
         grunnlag$Antall.M.F[i] <- 0)
}

#Slår sammen og får ett studium per institutt per år 
grunnlag <- data.frame(
  aggregate(cbind(
    Startkull, Frafalt, Antall.K, Antall.M, Antall.K.F, Antall.M.F)
    ~ Institusjonskode + Institusjonsnavn + Avdelingskode + Avdelingsnavn 
    + Studiumkode + Studnavn + Årstall,
    data = grunnlag, FUN = sum)
)


#Andel menn/kvinner av startkullet 
grunnlag$Kvinneandel <- grunnlag[,10]/grunnlag[,8]
grunnlag$Andel.M <- grunnlag[,11]/grunnlag[,8]

#Andel frafalt fra startkullet
grunnlag$Frafallsandel <- grunnlag[,9]/grunnlag[,8]

#Bør vi ha med andel frafalt mtp. kjønn? 

#Fjerner NaN-verdier 
grunnlag <- na.omit(grunnlag)
#Finnes studier der startkullet er på 3-6 stykk. Er dette realistisk?

grunnlag$Avdelingskode <- as.character(grunnlag$Avdelingskode)

#grunnlag <- grunnlag[,c(1:8, 14, 9, 16)]

grunnlag[, c(14,16)] <- round(grunnlag[, c(14,16)], digits = 2)

#Burde ta en titt på studiene der ALLE har falt fra.. virker ikke helt sannsynlig


#EKSTRA TITT PÅ EKSTREMVERDIER!!! 
#Sletter uspesifiserte underenheter 
grunnlag <- grunnlag[grunnlag$Avdelingskode!=0,]
grunnlag <- grunnlag[!(grunnlag$Startkull > 6000),]

################################################################################
#Henter gjennomsnittlig opptakspoeng
data1 <- read.csv('20220310-undefined-Gjennomsnittlige opptakspoeng for søkere.csv', sep = ';')

#Fjerner eventuelle duplikater
data1 <- data.frame(unique(data1))

#Fjerner ikke-relevante nivåkoder
data1 <- data1[data1$Nivåkode == 'B3'|
                 data1$Nivåkode == 'B4'|
                 data1$Nivåkode == 'M2'|
                 data1$Nivåkode == 'M5',]

#Fjerner studier med opptak på våren 
data1 <- data1[!(data1$Semester==1),]

#Sletter alle som ikke har møtt opp/fått tilbud 
data1 <- data1[!(data1$Møtt.til.studiestart=='0'),]

#Sletter rader der opptakspoeng, karakterpoeng eller søkere er lik null 
data1 <- data1[!(data1$Opptakspoeng.totalt == 0),]
data1 <- data1[!(data1$Karakterpoeng == 0),]
data1 <- data1[!(data1$Søkere.totalt == 0),]

#Ser vekk fra Lokale-opptak - blir komplisert
#OBS!!! OBS!!! Eks vil NTNU-handels vil kun få snittet til bachelor, men master inngår
data1 <- data1[!(data1$Opptakstype=='L'),]

#Finner gjennomsnittsprioritert per studium per institutt per år
#Fjerner supplement-prioriteringene fordi det er uavhengig av det ordinære opptaket
data1 <- data1[!(data1$Prioritet > 10),]

data1$P.SUM <- data1[,12]*data1[,18]
#Hmm er 4665 tilfeller med NA, men kan ikke slette disse radene fordi da vil karaktersnittet bli feil

#Gjør at en representerer ett studium per institutt per år
data1 <- data.frame(
  aggregate(cbind(Opptakspoeng.totalt, Karakterpoeng, Søkere.totalt, P.SUM)
            ~ Institusjonskode + Avdelingskode + Årstall + Studiumkode, data = data1, FUN = sum))

#Lager karaktersnitt og opptakssnitt per søker 
data1$Opptakssnitt <- data1[,5]/data1[,7]
data1$Karaktersnitt.vgs <- data1[,6]/data1[,7]

#Lager prioritetssnitt 
data1$Prioritetssnitt <- data1[,8]/data1[,7]
#MERK: For de fleste tilfeller er det 1 blank - vits å ha det med? 

data1[,c(9:11)] <- round(data1[,c(9:11)], digits = 2)

#Forberedende for hekting 
data1$Avdelingskode <- as.character(data1$Avdelingskode)

data1 <- data1[,-c(5:8,11)]
#Fjern "11" for ? inkludere prioritetssnitt / legg til for ? ta vekk. 

#Analyse hvorvidt prioritetssnitt skal være med
#sum(data1$Prioritetssnitt==1, na.rm=T) = 2782
#sum(is.na(data1$Prioritetssnitt))


#Fjerne de verdiene med for høyt opptakssnitt/karaktersnitt
data1$MAX <- data1[,5]-data1[,6]
data1 <- data1[!(data1$MAX > 14),]
data1 <- data1[,-c(7)]

#Lager utgangangspunktet for vårt endelige datasett
utg <- left_join(grunnlag, data1, by = c("Institusjonskode","Avdelingskode", "Årstall", "Studiumkode"))

################################################################################
#Henter Studenter fordelt på alder
data2 <- read.csv('20220314-undefined-Studenter fordelt på alder.csv', sep = ';')

#Fjerner eventuelle duplikater
data2<- data.frame(unique(data2))

#Velger nivåkodene vi  skal fokusere på
data2 <- data2[data2$Nivåkode == 'B3'|
                 data2$Nivåkode == 'B4'|
                 data2$Nivåkode == 'M2'|
                 data2$Nivåkode == 'M5',]

#Sletter vår-semestre 
data2 <- data2[!(data2$Semester==1),]

#Gjør at en representerer ett studium per insitutt per år
data2 <- data.frame(aggregate(cbind(Antall.totalt)
                              ~ Institusjonskode + Avdelingskode + Årstall + Alder + Studiumkode, data = data2, FUN = sum))

#Finner snittalder
data2$aldersum <- data2[,6]*data2[,4]

data2 <- data.frame(aggregate(cbind(Antall.totalt, aldersum)
                              ~ Institusjonskode + Avdelingskode + Årstall + Studiumkode, data = data2, FUN = sum))

data2$Snittalder <- data2[,6]/data2[,5]

data2$Avdelingskode <- as.character(data2$Avdelingskode)

data2 <- data2[,-c(5:6)]

data2[, c(5)] <- round(data2[, c(5)], digits = 2)

#Fjerner ekstremtilfellene
data2 <- data2[!(data2$Snittalder<18),]

utg <- left_join(utg, data2, by = c("Institusjonskode","Avdelingskode", "Årstall", "Studiumkode"))


################################################################################
#Henter avlagte doktorgrader
#Denne vil være for hvert institutt per år (vår+høst-semester)

data3 <- read.csv('20220310-undefined-Avlagte doktorgrader (aggregert).csv', sep = ';')

#Fjerner eventuelle duplikater
data3 <- data.frame(unique(data3))

data3 <- data.frame(aggregate(cbind(Antall.totalt)
                              ~ Institusjonskode + Avdelingskode + Årstall, data = data3, FUN = sum))

data3$Avdelingskode <- as.character(data3$Avdelingskode)

names(data3)[4] <- "Antall.doktor"

utg <- left_join(utg, data3, by = c("Institusjonskode", "Avdelingskode", "Årstall"))

#Gjør alle NA-verdier til 0 
for (i in 1:nrow(utg)){
  ifelse(is.na(utg$Antall.doktor[i]), 
         utg$Antall.doktor[i] <- 0, 
         utg$Antall.doktor[i] <- utg$Antall.doktor[i])
}

#Ta logaritmen av antall doktorgraden - for å gjøre forskjellene litt mindre
utg$Antall.doktor <- log(1+utg$Antall.doktor)

utg[, c(20)] <- round(utg[, c(20)], digits = 2)

################################################################################
#Henter vitenskapelig publiseringer 
#Denne vil være for hvert institutt per år 

data4 <- read.csv('20220315-undefined-Vitenskapelig publisering.csv', sep = ';')

#Fjerner eventuelle duplikater
data4 <- data.frame(unique(data4))

#Velger å ta med publiseringspoeng ettersom den vekter de ulike publiseringene
data4 <- data.frame(aggregate(cbind(Publiseringspoeng)
                              ~ Institusjonskode + Avdelingskode + Årstall, data = data4, FUN = sum))

#Prøvde å ta antall publiseringspoeng per ansatt, men var såpass mange NA-verdier på antall ansatte
# 1/6 at vi bestemte oss for å ta logaritmen i stedet 

data4$Publiseringspoeng <- log(1+data4$Publiseringspoeng)

data4$Avdelingskode <- as.character(data4$Avdelingskode)

data4[, c(4)] <- round(data4[, c(4)], digits = 2)

utg <- left_join(utg, data4, by = c("Institusjonskode", "Avdelingskode", "Årstall"))



################################################################################
#Henter karakterer aggregert 
#Denne blir karaktersnitt og Strykandel per studium per institutt per år

data5 <- read.csv('20220315-undefined-Karakterer (aggregert).csv', sep = ';')

#Fjerner eventuelle duplikater
data5 <- data.frame(distinct(data5))

#Fjerner nivåkodene vi ikke skal fokusere på 
data5 <- data5[data5$Nivåkode == 'B3'|
                 data5$Nivåkode == 'B4'|
                 data5$Nivåkode == 'M2'|
                 data5$Nivåkode == 'M5',]

#Beholder disse fordi vi har slettet det før 
data5 <- data5[data5$Studentkategori=='S',]

#Fjerner bestått/ikke-bestått fordi det ødelegger snittet 
data5 <- data5[!(data5$Karakter=='G'|data5$Karakter=='H'),]

data5 <- data5[!(data5$Antall.kandidater.totalt==0),]

data5 <- data.frame(aggregate(cbind(Antall.kandidater.totalt)
                              ~ Institusjonskode + Avdelingskode + Årstall + Studiumkode + Karakter, data = data5, FUN = sum))

data5$Karakterverdi <- NA
for (i in 1:nrow(data5)){
  ifelse(data5$Karakter[i]=='A', data5$Karakterverdi[i] <- 5,
         ifelse(data5$Karakter[i]=='B', data5$Karakterverdi[i] <- 4,
                ifelse(data5$Karakter[i]=='C', data5$Karakterverdi[i] <- 3,
                       ifelse(data5$Karakter[i]=='D', data5$Karakterverdi[i] <- 2,
                              ifelse(data5$Karakter[i]=='E', data5$Karakterverdi[i] <- 1,
                                     ifelse(data5$Karakter[i]=='F', data5$Karakterverdi[i] <- 0, 
                                            data5$Karakterverdi[i] <- NA))))))
  
}

data5 <- na.omit(data5)

data5 <- data5[!(data5$Antall.kandidater.totalt==0),]

data5$Karaktersum <- data5[,6]*data5[,7]

#Finner Strykandel 
data5$KarakterF <- NA
for (i in 1:nrow(data5)){
  ifelse(data5$Karakter[i]=='F', data5$KarakterF[i] <- data5$Antall.kandidater.totalt[i], data5$KarakterF <- 0)}


data5 <- data.frame(aggregate(cbind(Antall.kandidater.totalt, Karaktersum, KarakterF)
                              ~ Institusjonskode + Avdelingskode + Årstall + Studiumkode, data = data5, FUN = sum))

#Lager karaktersnitt for høyere utdanning 
data5$Karaktersnitt.H <- data5[,6]/data5[,5]
data5[, c(8)] <- round(data5[, c(8)], digits = 2)


#Lager strykandel
data5$Strykandel <- data5[,7]/data5[,5]
data5[, c(9)] <- round(data5[, c(9)], digits = 2)

data5 <- data5[,-c(5:7)]

data5$Avdelingskode <- as.character(data5$Avdelingskode)

utg <- left_join(utg, data5, by = c("Institusjonskode", "Avdelingskode", "Årstall", "Studiumkode"))


################################################################################
#Henter antall ansatte for å finne antall studenter per ansatt - instituttnivå
data6 <- read.csv('20220314-undefined-Tilsatte - gjennomsnittsalder.csv', sep = ';')

#Fjerner eventuelle duplikater
data6<- data.frame(unique(data6))

#Fjerner observasjoner som har verdi "INF"
data6 <- data6[!(data6$Antall.totalt == '0'),]

#Gjør at en representerer ett insitutt per år
data6 <- data.frame(aggregate(cbind(Antall.totalt)
                              ~ Institusjonskode + Avdelingskode + Årstall, data = data6, FUN = sum)) 

#Kaller Antall totalt ant.tot.ansatte
names(data6)[4] <- "Ant.tot.ansatte"

####################################
#Henter antall studenter
reg_stud <- read.csv('20220321-undefined-Registrerte studenter.csv', sep = ';')


#Velger de nivåkodene vi skal fokusere på 
reg_stud <- reg_stud[(reg_stud$Nivåkode == 'B3'|
                  reg_stud$Nivåkode == 'B4'|
                  reg_stud$Nivåkode =='M2'| 
                  reg_stud$Nivåkode == 'M5'),]

#Fjerner studenter på vårsemesteret
reg_stud <- reg_stud[!(reg_stud$Semester==1),]

#Fjerner studenter som ikke har studierett
reg_stud <- reg_stud[reg_stud$Studentkategori=="S",]

#Fjerner observasjoner som har verdi "INF"
reg_stud <- reg_stud[!(reg_stud$Antall.totalt == '0'),]

#Kaller Antall totalt ant.tot.stud
names(reg_stud)[14] <- "Ant.tot.stud"

#Gjør at en rad representerer ett studium per insitutt per år (brukes senere)
reg_stud1 <- data.frame(aggregate(cbind(Ant.tot.stud)
                              ~ Institusjonskode + Avdelingskode + Studiumkode + Årstall, data = reg_stud, FUN = sum)) 

#Gjør at en rad representerer ett institutt per år 
reg_stud2 <- data.frame(aggregate(cbind(Ant.tot.stud)
                              ~ Institusjonskode + Avdelingskode + Årstall, data = reg_stud, FUN = sum)) 


#Leftjoiner de to datasettene
data6 <- left_join(data6, reg_stud2, by = c("Institusjonskode", "Avdelingskode", "Årstall"))


#Lager en ny variabel som sier antall studenter per ansatt
data6$Antallstudperans <- data6[,5]/data6[,4]

#To desimaler
data6[, c(6)] <- round(data6[, c(6)], digits = 2)

data6$Avdelingskode <- as.character(data6$Avdelingskode)

#Fjerner totalt antall ansatte og totalt antall studenter
data6 <- data6[,-c(4:5)]

#Fjerner ekstremverdier 
data6 <- data6[!(data6$Antallstudperans > 400),]


#Slår sammen grunnlaget med nydata3
utg <- left_join(utg, data6, by = c("Institusjonskode","Avdelingskode", "Årstall"))

#Hmm.. ser i DBH at de beregner Studenter per faglig årsverk... Burde vi heller gjøre det? 


################################################################################
#Henter utvekslingsstudenter
data7 <- read.csv('20220315-undefined-Utvekslingsstudenter og studenter under kvoteprogram (aggregert).csv', sep = ';')

#Velger de nivåkodene vi skal fokusere på 
data7 <- data7[(data7$Nivåkode == 'B3'|
                  data7$Nivåkode == 'B4'|
                  data7$Nivåkode =='M2'| 
                  data7$Nivåkode == 'M5'),]

#Fjerner duplikater
data7 <- data.frame(unique(data7))

#Skal finne andel norske studenter på utveksling per studium per år, samt utenlandske studenter på utveksling i Norge

#Agrregerer per studium per institutt per år
data7 <- data.frame(
  aggregate(cbind(
    Antall.totalt)
    ~ Institusjonskode + Avdelingskode + Årstall + Studiumkode + Type,
    data = data7, FUN = sum))


data7$Utveksling_norsk <- NA
for (i in 1:nrow(data7)){
  ifelse(data7$Type[i]=='NORSK',
         data7$Utveksling_norsk[i] <- data7$Antall.totalt[i],
         data7$Utveksling_norsk[i] <- 0)
}

data7$Utveksling_UTENL <- NA
for (i in 1:nrow(data7)){
  ifelse(data7$Type[i]=='UTENL',
         data7$Utveksling_UTENL[i] <- data7$Antall.totalt[i],
         data7$Utveksling_UTENL[i] <- 0)
}

data7 <- data.frame(
  aggregate(cbind(
    Utveksling_norsk, Utveksling_UTENL)
    ~ Institusjonskode + Avdelingskode + Årstall + Studiumkode,
    data = data7, FUN = sum))

data7 <- left_join(data7,reg_stud1, by = c("Institusjonskode", "Avdelingskode", "Årstall", "Studiumkode"))

data7 <- na.omit(data7)

data7$Andel.ut.NORSK <- data7[,5]/data7[,7]

data7$Andel.ut.UTENL <- data7[,6]/data7[,7]

data7[, c(8:9)] <- round(data7[, c(8:9)], digits = 2)

data7 <- data7[,-c(5:7)]

#Gjør avdelingskode om til character
data7$Avdelingskode <- as.character(data7$Avdelingskode)

#Sletter ekstremverdier
data7 <- data7[!(data7$Andel.ut.NORSK>1),]
data7 <- data7[!(data7$Andel.ut.UTENL>1),]
data7 <- data7[!(data7$Andel.ut.NORSK>0.49),]
data7 <- data7[!(data7$Andel.ut.UTENL>0.19),]


#Slår sammen
utg <- left_join(utg, data7, by = c("Institusjonskode", "Avdelingskode", "Årstall", "Studiumkode"))


################################################################################
#Henter utenlandske studenter 
data8 <- read.csv('20220321-undefined-Utenlandske studenter.csv', sep = ';')

#Velger de nivåkodene vi skal fokusere på 
data8 <- data8[(data8$Nivåkode == 'B3'|
                  data8$Nivåkode == 'B4'|
                  data8$Nivåkode =='M2'| 
                  data8$Nivåkode == 'M5'),]

#Fjerner studier med opptak på våren 
data8 <- data8[!(data8$Semester==1),]

#Fjerner duplikater
data8 <- data.frame(unique(data8))

#Agrregerer per studium per institutt per år
data8 <- data.frame(
  aggregate(cbind(
    Antall.totalt)
    ~ Institusjonskode + Avdelingskode + Årstall + Studiumkode,
    data = data8, FUN = sum))

names(data8)[5] <- "Ant.tot.UTENL"


data8 <- left_join(data8,reg_stud1, by = c("Institusjonskode", "Avdelingskode", "Årstall", "Studiumkode"))

data8$Andel.UTENL <- data8[,5]/data8[,6]

data8[, c(7)] <- round(data8[, c(7)], digits = 2)

data8 <- data8[,-c(5:6)]

data8$Avdelingskode <- as.character(data8$Avdelingskode)

data8 <- data8[!(data8$Andel.UTENL>0.9),]


utg <- left_join(utg, data8, by = c("Institusjonskode", "Avdelingskode", "Årstall", "Studiumkode"))


################################################################################
#Gjør kategoriske variabler til faktorer 
utg$Institusjonsnavn <- factor(utg$Institusjonsnavn)
utg$Avdelingsnavn <- factor(utg$Avdelingsnavn)
utg$Studiumkode <- factor(utg$Studiumkode)
utg$Studnavn <- factor(utg$Studnavn)





################################################################################
#Lager datasettet til en CSV-fil

write.csv(utg,'C:/Users/solme/OneDrive/Master/Endelig datasett 333.csv', row.names = TRUE)

################################################################################
#Lager pdf 
#dir()
#knitr::stitch('BBAN5000 Endelig datasett.R')
