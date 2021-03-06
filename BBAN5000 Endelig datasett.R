library(jsonlite)
library(httr)
library(foreign)
library(rjstat)
library(plyr)
library(dplyr)

options(encoding="UTF-8")

#Solveigs
setwd("C:/Users/solme/OneDrive/Master/Endelig datasett")

################################################################################
#Danner grunnlaget for det sammensatte datasettet

#Henter HELE gjennomf�ring og frafall (studiumniv�)
grunnlag <- read.csv('20220310-undefined-Gjennomf�ring og frafall (studiumniv�).csv', sep = ';')

#Fjerner eventuelle duplikater
grunnlag <- data.frame(unique(grunnlag))

names(grunnlag)[3] <- "Avdelingskode"

#Gj�r relevante kolonner nummeriske: (2-tallet st�r for kolonne, bruker 1 dersom det skal v��re rader) 
grunnlag[,c(15:24)] <- apply(grunnlag[,c(15:24)],2,function(x) as.numeric(as.character(x)))

#Fjerner alle med startkull = 0
grunnlag <- grunnlag[!(grunnlag$Startkull == 0),]

#Fjerner alle fakultet 
grunnlag <- grunnlag[!grepl('Fakultet', grunnlag$Avdelingsnavn),]
grunnlag <- grunnlag[!grepl('fakultet', grunnlag$Avdelingsnavn),]

#Fjerner "programomr�der" fordi det gir studiekompetanse (er alts� ikke h�yere utdanning)
grunnlag <- grunnlag[!grepl("Programomr�de", grunnlag$Avdelingsnavn),]

#Velger de niv�kodene vi skal fokusere p�
grunnlag <- grunnlag[(grunnlag$Niv�kode == 'B3'|
                         grunnlag$Niv�kode == 'B4'|
                         grunnlag$Niv�kode =='M2'| 
                         grunnlag$Niv�kode == 'M5'),]

#Unders�ker om det er mer som skal slettes som ikke inneholder institutt
#test <- grunnlag[!grepl("Institutt", grunnlag$Avdelingsnavn),]
#test <- test[!grepl("institutt", test$Avdelingsnavn),]
#Konklusjon: trenger ikke slette mer

#Antall menn/kvinner totalt: 
grunnlag <- data.frame(
  aggregate(cbind(
    Startkull, Frafalt)
    ~ Institusjonskode + Institusjonsnavn + Avdelingskode 
    + Avdelingsnavn + Studiumkode + Studnavn + Niv�kode
    + �rstall + �rstall.normert.tid + Kj�nn.1, 
    data = grunnlag, FUN = sum)
)

grunnlag$Antall.K <- NA 
for (i in 1:nrow(grunnlag)){
  ifelse(grunnlag$Kj�nn.1[i] == 'K', 
         grunnlag$Antall.K[i] <- grunnlag$Startkull[i],
         grunnlag$Antall.K[i] <- 0)
}

grunnlag$Antall.M <- NA
for (i in 1:nrow(grunnlag)){
  ifelse(grunnlag$Kj�nn.1[i] == 'M', 
         grunnlag$Antall.M[i] <- grunnlag$Startkull[i],
         grunnlag$Antall.M[i] <- 0)
}

#Antall kvinner/menn som er frafalt:
grunnlag$Antall.K.F <- NA
for (i in 1:nrow(grunnlag)){
  ifelse(grunnlag$Kj�nn.1[i] == 'K', 
         grunnlag$Antall.K.F[i] <- grunnlag$Frafalt[i],
         grunnlag$Antall.K.F[i] <- 0)
}

grunnlag$Antall.M.F <- NA
for (i in 1:nrow(grunnlag)){
  ifelse(grunnlag$Kj�nn.1[i] == 'M', 
         grunnlag$Antall.M.F[i] <- grunnlag$Frafalt[i],
         grunnlag$Antall.M.F[i] <- 0)
}

#Sl�r sammen og f�r ett studium per institutt per �r 
grunnlag <- data.frame(
  aggregate(cbind(
    Startkull, Frafalt, Antall.K, Antall.M, Antall.K.F, Antall.M.F)
    ~ Institusjonskode + Institusjonsnavn + Avdelingskode + Avdelingsnavn 
    + Studiumkode + Studnavn + �rstall,
    data = grunnlag, FUN = sum)
)


#Andel menn/kvinner av startkullet 
grunnlag$Kvinneandel <- grunnlag[,10]/grunnlag[,8]
grunnlag$Andel.M <- grunnlag[,11]/grunnlag[,8]

#Andel frafalt fra startkullet
grunnlag$Frafallsandel <- grunnlag[,9]/grunnlag[,8]

#Fjerner NaN-verdier 
grunnlag <- na.omit(grunnlag)

grunnlag$Avdelingskode <- as.character(grunnlag$Avdelingskode)

grunnlag[, c(14,16)] <- round(grunnlag[, c(14,16)], digits = 2)

#Sletter uspesifiserte underenheter 
grunnlag <- grunnlag[grunnlag$Avdelingskode!=0,]
grunnlag <- grunnlag[!(grunnlag$Startkull > 6000),]

################################################################################
#Henter gjennomsnittlig opptakspoeng
data1 <- read.csv('20220310-undefined-Gjennomsnittlige opptakspoeng for s�kere.csv', sep = ';')

#Fjerner eventuelle duplikater
data1 <- data.frame(unique(data1))

#Fjerner ikke-relevante niv�koder
data1 <- data1[data1$Niv�kode == 'B3'|
                 data1$Niv�kode == 'B4'|
                 data1$Niv�kode == 'M2'|
                 data1$Niv�kode == 'M5',]

#Fjerner studier med opptak p� v�ren 
data1 <- data1[!(data1$Semester==1),]

#Sletter alle som ikke har m�tt opp/f�tt tilbud 
data1 <- data1[!(data1$M�tt.til.studiestart=='0'),]

#Sletter rader der opptakspoeng, karakterpoeng eller s�kere er lik null 
data1 <- data1[!(data1$Opptakspoeng.totalt == 0),]
data1 <- data1[!(data1$Karakterpoeng == 0),]
data1 <- data1[!(data1$S�kere.totalt == 0),]

#Ser vekk fra Lokale-opptak - blir komplisert
data1 <- data1[!(data1$Opptakstype=='L'),]

#Finner gjennomsnittsprioritert per studium per institutt per �r
#Fjerner supplement-prioriteringene fordi det er uavhengig av det ordin�re opptaket
data1 <- data1[!(data1$Prioritet > 10),]

data1$P.SUM <- data1[,12]*data1[,18]

#Gj�r at en representerer ett studium per institutt per �r
data1 <- data.frame(
  aggregate(cbind(Opptakspoeng.totalt, Karakterpoeng, S�kere.totalt, P.SUM)
            ~ Institusjonskode + Avdelingskode + �rstall + Studiumkode, data = data1, FUN = sum))

#Lager karaktersnitt og opptakssnitt per s�ker 
data1$Opptakssnitt <- data1[,5]/data1[,7]
data1$Karaktersnitt.vgs <- data1[,6]/data1[,7]

#Lager prioritetssnitt 
data1$Prioritetssnitt <- data1[,8]/data1[,7]
#MERK: For de fleste tilfeller er det 1 blank - vits � ha det med? 

data1[,c(9:11)] <- round(data1[,c(9:11)], digits = 2)

#Forberedende for hekting 
data1$Avdelingskode <- as.character(data1$Avdelingskode)

data1 <- data1[,-c(5:8,11)]
#Fjern "11" for � inkludere prioritetssnitt / legg til for � ta vekk. 

#Analyse hvorvidt prioritetssnitt skal v�re med
#sum(data1$Prioritetssnitt==1, na.rm=T) = 2782
#sum(is.na(data1$Prioritetssnitt))
#Velger � ikke ha det med

#Fjerne de verdiene med for h�yt opptakssnitt/karaktersnitt
data1$MAX <- data1[,5]-data1[,6]
data1 <- data1[!(data1$MAX > 14),]
data1 <- data1[,-c(7)]

#Lager utgangangspunktet for v�rt endelige datasett
utg <- left_join(grunnlag, data1, by = c("Institusjonskode","Avdelingskode", "�rstall", "Studiumkode"))

################################################################################
#Henter Studenter fordelt p� alder
data2 <- read.csv('20220314-undefined-Studenter fordelt p� alder.csv', sep = ';')

#Fjerner eventuelle duplikater
data2<- data.frame(unique(data2))

#Velger niv�kodene vi  skal fokusere p�
data2 <- data2[data2$Niv�kode == 'B3'|
                 data2$Niv�kode == 'B4'|
                 data2$Niv�kode == 'M2'|
                 data2$Niv�kode == 'M5',]

#Sletter v�r-semestre 
data2 <- data2[!(data2$Semester==1),]

#Gj�r at en representerer ett studium per insitutt per �r
data2 <- data.frame(aggregate(cbind(Antall.totalt)
                              ~ Institusjonskode + Avdelingskode + �rstall + Alder + Studiumkode, data = data2, FUN = sum))

#Finner snittalder
data2$aldersum <- data2[,6]*data2[,4]

data2 <- data.frame(aggregate(cbind(Antall.totalt, aldersum)
                              ~ Institusjonskode + Avdelingskode + �rstall + Studiumkode, data = data2, FUN = sum))

data2$Snittalder <- data2[,6]/data2[,5]

data2$Avdelingskode <- as.character(data2$Avdelingskode)

data2 <- data2[,-c(5:6)]

data2[, c(5)] <- round(data2[, c(5)], digits = 2)

#Fjerner ekstremtilfellene
data2 <- data2[!(data2$Snittalder<18),]

utg <- left_join(utg, data2, by = c("Institusjonskode","Avdelingskode", "�rstall", "Studiumkode"))

################################################################################
#Henter avlagte doktorgrader
#Denne vil v�re for hvert institutt per �r (v�r+h�st-semester)

data3 <- read.csv('20220310-undefined-Avlagte doktorgrader (aggregert).csv', sep = ';')

#Fjerner eventuelle duplikater
data3 <- data.frame(unique(data3))

data3 <- data.frame(aggregate(cbind(Antall.totalt)
                              ~ Institusjonskode + Avdelingskode + �rstall, data = data3, FUN = sum))

data3$Avdelingskode <- as.character(data3$Avdelingskode)

names(data3)[4] <- "Antall.doktor"

utg <- left_join(utg, data3, by = c("Institusjonskode", "Avdelingskode", "�rstall"))

#Gj�r alle NA-verdier til 0 
for (i in 1:nrow(utg)){
  ifelse(is.na(utg$Antall.doktor[i]), 
         utg$Antall.doktor[i] <- 0, 
         utg$Antall.doktor[i] <- utg$Antall.doktor[i])
}

#Ta logaritmen av antall doktorgraden - for � gj�re forskjellene litt mindre
utg$Antall.doktor <- log(1+utg$Antall.doktor)

utg[, c(20)] <- round(utg[, c(20)], digits = 2)

################################################################################
#Henter vitenskapelig publiseringer 
#Denne vil v�re for hvert institutt per �r 

data4 <- read.csv('20220315-undefined-Vitenskapelig publisering.csv', sep = ';')

#Fjerner eventuelle duplikater
data4 <- data.frame(unique(data4))

#Velger � ta med publiseringspoeng ettersom den vekter de ulike publiseringene
data4 <- data.frame(aggregate(cbind(Publiseringspoeng)
                              ~ Institusjonskode + Avdelingskode + �rstall, data = data4, FUN = sum))

data4$Publiseringspoeng <- log(1+data4$Publiseringspoeng)

data4$Avdelingskode <- as.character(data4$Avdelingskode)

data4[, c(4)] <- round(data4[, c(4)], digits = 2)

utg <- left_join(utg, data4, by = c("Institusjonskode", "Avdelingskode", "�rstall"))

################################################################################
#Henter karakterer aggregert 
#Denne blir karaktersnitt og Strykandel per studium per institutt per �r

data5 <- read.csv('20220315-undefined-Karakterer (aggregert).csv', sep = ';')

#Fjerner eventuelle duplikater
data5 <- data.frame(distinct(data5))

#Fjerner niv�kodene vi ikke skal fokusere p� 
data5 <- data5[data5$Niv�kode == 'B3'|
                 data5$Niv�kode == 'B4'|
                 data5$Niv�kode == 'M2'|
                 data5$Niv�kode == 'M5',]

#Beholder disse fordi vi har slettet det f�r 
data5 <- data5[data5$Studentkategori=='S',]

#Fjerner best�tt/ikke-best�tt fordi det �delegger snittet 
data5 <- data5[!(data5$Karakter=='G'|data5$Karakter=='H'),]

data5 <- data5[!(data5$Antall.kandidater.totalt==0),]

data5 <- data.frame(aggregate(cbind(Antall.kandidater.totalt)
                              ~ Institusjonskode + Avdelingskode + �rstall + Studiumkode + Karakter, data = data5, FUN = sum))

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
                              ~ Institusjonskode + Avdelingskode + �rstall + Studiumkode, data = data5, FUN = sum))

#Lager karaktersnitt for h�yere utdanning 
data5$Karaktersnitt.H <- data5[,6]/data5[,5]
data5[, c(8)] <- round(data5[, c(8)], digits = 2)


#Lager strykandel
data5$Strykandel <- data5[,7]/data5[,5]
data5[, c(9)] <- round(data5[, c(9)], digits = 2)

data5 <- data5[,-c(5:7)]

data5$Avdelingskode <- as.character(data5$Avdelingskode)

utg <- left_join(utg, data5, by = c("Institusjonskode", "Avdelingskode", "�rstall", "Studiumkode"))


################################################################################
#Henter antall ansatte for � finne antall studenter per ansatt - instituttniv�
data6 <- read.csv('20220314-undefined-Tilsatte - gjennomsnittsalder.csv', sep = ';')

#Fjerner eventuelle duplikater
data6<- data.frame(unique(data6))

#Fjerner observasjoner som har verdi "INF"
data6 <- data6[!(data6$Antall.totalt == '0'),]

#Gj�r at en representerer ett insitutt per �r
data6 <- data.frame(aggregate(cbind(Antall.totalt)
                              ~ Institusjonskode + Avdelingskode + �rstall, data = data6, FUN = sum)) 

#Kaller Antall totalt ant.tot.ansatte
names(data6)[4] <- "Ant.tot.ansatte"

####################################
#Henter antall studenter
reg_stud <- read.csv('20220321-undefined-Registrerte studenter.csv', sep = ';')


#Velger de niv�kodene vi skal fokusere p� 
reg_stud <- reg_stud[(reg_stud$Niv�kode == 'B3'|
                  reg_stud$Niv�kode == 'B4'|
                  reg_stud$Niv�kode =='M2'| 
                  reg_stud$Niv�kode == 'M5'),]

#Fjerner studenter p� v�rsemesteret
reg_stud <- reg_stud[!(reg_stud$Semester==1),]

#Fjerner studenter som ikke har studierett
reg_stud <- reg_stud[reg_stud$Studentkategori=="S",]

#Fjerner observasjoner som har verdi "INF"
reg_stud <- reg_stud[!(reg_stud$Antall.totalt == '0'),]

#Kaller Antall totalt ant.tot.stud
names(reg_stud)[14] <- "Ant.tot.stud"

#Gj�r at en rad representerer ett studium per insitutt per �r (brukes senere)
reg_stud1 <- data.frame(aggregate(cbind(Ant.tot.stud)
                              ~ Institusjonskode + Avdelingskode + Studiumkode + �rstall, data = reg_stud, FUN = sum)) 

#Gj�r at en rad representerer ett institutt per �r 
reg_stud2 <- data.frame(aggregate(cbind(Ant.tot.stud)
                              ~ Institusjonskode + Avdelingskode + �rstall, data = reg_stud, FUN = sum)) 


#Leftjoiner de to datasettene
data6 <- left_join(data6, reg_stud2, by = c("Institusjonskode", "Avdelingskode", "�rstall"))


#Lager en ny variabel som sier antall studenter per ansatt
data6$Antallstudperans <- data6[,5]/data6[,4]

#To desimaler
data6[, c(6)] <- round(data6[, c(6)], digits = 2)

data6$Avdelingskode <- as.character(data6$Avdelingskode)

#Fjerner totalt antall ansatte og totalt antall studenter
data6 <- data6[,-c(4:5)]

#Fjerner ekstremverdier 
data6 <- data6[!(data6$Antallstudperans > 400),]


#Sl�r sammen grunnlaget med nydata3
utg <- left_join(utg, data6, by = c("Institusjonskode","Avdelingskode", "�rstall"))


################################################################################
#Henter utvekslingsstudenter
data7 <- read.csv('20220315-undefined-Utvekslingsstudenter og studenter under kvoteprogram (aggregert).csv', sep = ';')

#Velger de niv�kodene vi skal fokusere p� 
data7 <- data7[(data7$Niv�kode == 'B3'|
                  data7$Niv�kode == 'B4'|
                  data7$Niv�kode =='M2'| 
                  data7$Niv�kode == 'M5'),]

#Fjerner duplikater
data7 <- data.frame(unique(data7))

#Skal finne andel norske studenter p� utveksling per studium per �r, samt utenlandske studenter p� utveksling i Norge

#Agrregerer per studium per institutt per �r
data7 <- data.frame(
  aggregate(cbind(
    Antall.totalt)
    ~ Institusjonskode + Avdelingskode + �rstall + Studiumkode + Type,
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
    ~ Institusjonskode + Avdelingskode + �rstall + Studiumkode,
    data = data7, FUN = sum))

data7 <- left_join(data7,reg_stud1, by = c("Institusjonskode", "Avdelingskode", "�rstall", "Studiumkode"))

data7 <- na.omit(data7)

data7$Andel.ut.NORSK <- data7[,5]/data7[,7]

data7$Andel.ut.UTENL <- data7[,6]/data7[,7]

data7[, c(8:9)] <- round(data7[, c(8:9)], digits = 2)

data7 <- data7[,-c(5:7)]

#Gj�r avdelingskode om til character
data7$Avdelingskode <- as.character(data7$Avdelingskode)

#Sletter ekstremverdier
data7 <- data7[!(data7$Andel.ut.NORSK>1),]
data7 <- data7[!(data7$Andel.ut.UTENL>1),]
data7 <- data7[!(data7$Andel.ut.NORSK>0.49),]
data7 <- data7[!(data7$Andel.ut.UTENL>0.19),]


#Sl�r sammen
utg <- left_join(utg, data7, by = c("Institusjonskode", "Avdelingskode", "�rstall", "Studiumkode"))


################################################################################
#Henter utenlandske studenter 
data8 <- read.csv('20220321-undefined-Utenlandske studenter.csv', sep = ';')

#Velger de niv�kodene vi skal fokusere p� 
data8 <- data8[(data8$Niv�kode == 'B3'|
                  data8$Niv�kode == 'B4'|
                  data8$Niv�kode =='M2'| 
                  data8$Niv�kode == 'M5'),]

#Fjerner studier med opptak p� v�ren 
data8 <- data8[!(data8$Semester==1),]

#Fjerner duplikater
data8 <- data.frame(unique(data8))

#Agrregerer per studium per institutt per �r
data8 <- data.frame(
  aggregate(cbind(
    Antall.totalt)
    ~ Institusjonskode + Avdelingskode + �rstall + Studiumkode,
    data = data8, FUN = sum))

names(data8)[5] <- "Ant.tot.UTENL"


data8 <- left_join(data8,reg_stud1, by = c("Institusjonskode", "Avdelingskode", "�rstall", "Studiumkode"))

data8$Andel.UTENL <- data8[,5]/data8[,6]

data8[, c(7)] <- round(data8[, c(7)], digits = 2)

data8 <- data8[,-c(5:6)]

data8$Avdelingskode <- as.character(data8$Avdelingskode)

data8 <- data8[!(data8$Andel.UTENL>0.9),]


utg <- left_join(utg, data8, by = c("Institusjonskode", "Avdelingskode", "�rstall", "Studiumkode"))


################################################################################
#Gj�r kategoriske variabler til faktorer 
utg$Institusjonsnavn <- factor(utg$Institusjonsnavn)
utg$Avdelingsnavn <- factor(utg$Avdelingsnavn)
utg$Studiumkode <- factor(utg$Studiumkode)
utg$Studnavn <- factor(utg$Studnavn)




################################################################################
#Lager datasettet til en CSV-fil

write.csv(utg,'C:/Users/solme/OneDrive/Master/Endelig datasett 333.csv', row.names = TRUE)
