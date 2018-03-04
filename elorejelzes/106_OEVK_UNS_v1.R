# CLEAR MEMORY
rm(list=ls())

# Uncomment the below lines if you don't have the package:
library(arm)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(descr)
library(DataCombine)
library(stargazer)
library(mfx)
library(data.table)
library(readstata13)


# CHECK  WORKING DIRECTORY
getwd()
setwd('C:/Users/adasan01/OneDrive - ARM/Desktop/TacticalVoting/Capstone')

# LOAD  DATA
data <- read.csv("106/adat/jelolt/vote_counts_precincts.csv")

data_telepules <- read.csv("106/adat/telepules/telepules_kodok.csv")

##data_erosorrend <- 

#ksh_ddata <- read.dta13("kh.dta")
#save.dta13(dat, file="xxx.dta")

data <- as.data.table(data)

#kamupartok egyesitese es szurese
kamu <- subset(data, select = c(id, mcp, haza_nem_elado, sms, fkgp, udp, sem, jesz, ump, munkaspart, szocdemek, kti, egyutt2014, zoldek, osszefogas))

kamu$egyeb <- rowSums( kamu[,2:15] )

kamu2 <- subset(kamu, select = c(id, egyeb))

data2 <- merge(data, kamu2, by="id")

#reszveteli adatok, 4 part listaja, egyeni, egyeb partok lista + megye, telepules, szavazokor kulon

data_clean <- subset(data2, select = c(id, atjelentkezettek, szavazokor, oevk, szavazok, reszvetel, fidesz, lmp, kormanyvaltok, jobbik, egyeni_fidesz, egyeni_lmp, egyeni_kormanyvaltok, egyeni_jobbik, egyeb))



#szavazasra jogosultak szama / szavazokor

data_clean <- data_clean[, ossz_szavazo := round(szavazok / (reszvetel /100), digits = 0)] 

#szavazokorok szama 

data_szavazokorok_szama <- data_clean[, list(szavazokorok_szama = .N), by = oevk]

data_clean <- merge(data_clean, data_szavazokorok_szama, by = "oevk")


#UJ VALTOZOK - partok szazalekos egyeni eredmenye egymashoz viszonyitva (kamupartok nelkul)

data_clean <- data_clean[, egyeni_fidesz_pc := egyeni_fidesz / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)] 

data_clean <- data_clean[, egyeni_kormanyvaltok_pc := egyeni_kormanyvaltok / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)]

data_clean <- data_clean[, egyeni_jobbik_pc := egyeni_jobbik / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)]

data_clean <- data_clean[, egyeni_lmp_pc := egyeni_lmp / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)]


#TEST egyeni szavazat partonkent 

fidesz_egyeni_ossz <- sum(data_clean$egyeni_fidesz)
fidesz_egyeni_ossz

baloldal_egyeni_ossz <- sum(data_clean$egyeni_kormanyvaltok)
baloldal_egyeni_ossz

jobbik_egyeni_ossz <- sum(data_clean$egyeni_jobbik)
jobbik_egyeni_ossz

lmp_egyeni_ossz <- sum(data_clean$egyeni_lmp)
lmp_egyeni_ossz

#oevk szintu eredmenyek (4 fobb part)

by_szavazokor_2014 <- data_clean[, list(szavazok, reszvetel, ossz_szavazo, egyeni_fidesz, egyeni_lmp, egyeni_kormanyvaltok, egyeni_jobbik, szavazokorok_szama), 
                              by = list(oevk, id)]

by_oevk_2014 <- by_szavazokor_2014[, list(szavazok = sum(szavazok),
                            reszvetel = sum(szavazok) / sum(ossz_szavazo),
                            ossz_szavazo = sum(ossz_szavazo),
                           egyeni_fidesz = sum(egyeni_fidesz),
                           egyeni_lmp = sum(egyeni_lmp),
                           egyeni_kormanyvaltok = sum(egyeni_kormanyvaltok),
                           egyeni_jobbik = sum(egyeni_jobbik),
                egyeni_fidesz = sum(egyeni_fidesz) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik),
                 egyeni_lmp_pc = sum(egyeni_lmp) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik),
              egyeni_kormanyvaltok_pc = sum(egyeni_kormanyvaltok) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik),
               egyeni_jobbik_pc = sum(egyeni_jobbik) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik)),
                           by = oevk][order(oevk)] 


#uniform swing kalkulacio data_UNS

#new inputs - orszagos pollok átlaga -4% Fidesz, frissítve március 2-án

#2014 eredmény Fidesz 
# Fidesz 44.13
# Jobbik 20.34
# Kormányváltók 26.86
# LMP 4.98
# Egyéb 3.69

#2018 március 
#Fidesz 45%, 
#MSZP-P 17%
#Jobbik 18% 
#LMP 9%
# DK 6%
#Momentum 2%
#Egyéb 3%

#állandó 2014-es párt szorzók

data_UNS <- data_clean[, Fidesz_2014_orszagos := 44.13] 
data_UNS <- data_UNS[, Kormanyvaltok_2014_orszagos := 26.86]
data_UNS <- data_UNS[, Jobbik_2014_orszagos := 20.34]
data_UNS <- data_UNS[, LMP_2014_orszagos := 4.98]
data_UNS <- data_UNS[, Egyeb_2014_orszagos := 3.69]

data_UNS <- data_UNS[, reszvetel_2014 := 60.99] 

#frissítheto új adatok alapján

data_UNS <- data_UNS[, Fidesz_2018_orszagos := 45] 
data_UNS <- data_UNS[, MSZP_P_2018_orszagos := 17]
data_UNS <- data_UNS[, Jobbik_2018_orszagos := 18]
data_UNS <- data_UNS[, LMP_2018_orszagos := 9]
data_UNS <- data_UNS[, DK_2018_orszagos := 6]
data_UNS <- data_UNS[, Momentum_2018_orszagos := 2]

data_UNS <- data_UNS[, reszvetel_2018 := 64.5]  

#kiegeszito 2014-es benchmarkok az MSZP-P, DK, es Momentum adataihoz

data_UNS <- data_UNS[, MSZP_P_2014_orszagos := (MSZP_P_2018_orszagos / (DK_2018_orszagos + MSZP_P_2018_orszagos)) * Kormanyvaltok_2014_orszagos ]

data_UNS <- data_UNS[, DK_2014_orszagos := (DK_2018_orszagos / (DK_2018_orszagos + MSZP_P_2018_orszagos)) * Kormanyvaltok_2014_orszagos ]

data_UNS <- data_UNS[, Momentum_2014_orszagos :=  0.4 * LMP_2014_orszagos + 0.2 * Egyeb_2014_orszagos]

data_UNS <- data_UNS[, reszvetel_2018_szorzo :=  reszvetel_2018 / reszvetel_2014]

# 2018-as UNS vote share


by_oevk_UNS_2018 <- data_UNS[, list(Fidesz_2014 = sum(egyeni_fidesz),
                            MSZP_DK_P_2014 = sum(egyeni_kormanyvaltok),
                            LMP_2014 = sum(egyeni_lmp),
                            Jobbik_2014 = sum(egyeni_jobbik),
                            Fidesz_2018 = sum(egyeni_fidesz * (Fidesz_2018_orszagos / Fidesz_2014_orszagos) * reszvetel_2018_szorzo),
                            Jobbik_2018 = sum(egyeni_jobbik * (Jobbik_2018_orszagos / Jobbik_2014_orszagos) * reszvetel_2018_szorzo),
                            MSZP_P_2018 = sum(egyeni_kormanyvaltok * ((MSZP_P_2018_orszagos / MSZP_P_2014_orszagos) * (MSZP_P_2018_orszagos / (DK_2018_orszagos + MSZP_P_2018_orszagos))) * reszvetel_2018_szorzo),
                            DK_2018 = sum(egyeni_kormanyvaltok * ((DK_2018_orszagos / DK_2014_orszagos) * (DK_2018_orszagos / (DK_2018_orszagos + MSZP_P_2018_orszagos))) * reszvetel_2018_szorzo),
                            LMP_2018 = sum(egyeni_lmp * (LMP_2018_orszagos / LMP_2014_orszagos) * reszvetel_2018_szorzo),
                            Momentum_2018 = sum((egyeni_lmp * 0.2) + (egyeb * 0.4) * (Momentum_2018_orszagos / Momentum_2014_orszagos) * reszvetel_2018_szorzo)),
                     by = oevk][order(oevk)]

Fidesz_2018 <- sum(by_oevk_UNS_2018$Fidesz_2018)
Fidesz_2018

Jobbik_2018 <- sum(by_oevk_UNS_2018$Jobbik_2018)
Jobbik_2018

MSZP_P_2018 <- sum(by_oevk_UNS_2018$MSZP_P_2018)
MSZP_P_2018

DK_2018 <- sum(by_oevk_UNS_2018$DK_2018)
DK_2018

LMP_2018 <- sum(by_oevk_UNS_2018$LMP_2018)
LMP_2018

Momentum_2018 <- sum(by_oevk_UNS_2018$Momentum_2018)
Momentum_2018

osszes_szavazat <- by_oevk_UNS_2018[, list(Fidesz = round(sum(Fidesz_2018), digits = 0), Jobbik = round(sum(Jobbik_2018), digits = 0), 
                                           MSZP = round(sum(MSZP_P_2018), digits = 0), 
                                           DK = round(sum(DK_2018), digits = 0), LMP  = round(sum(LMP_2018), digits = 0), 
                                           Momentum = round(sum(Momentum_2018), digits = 0))]

rowSums(osszes_szavazat)


write.csv(by_oevk_UNS_2018,'2018_UNS.csv')

##### TEST PART - playing with some regressions on szavazokor nagysaga

#TEST - szavazokorok nagysaga - kicsi 1, 200 alatt, nem kicsi, 0, 200 felett

data_clean$szavazokor_size_binary [(data_clean$ossz_szavazo <= 200)] <- 1
data_clean$szavazokor_size_binary [(data_clean$ossz_szavazo > 200)] <- 0

#TEST - frequency stats mini szavazokorok szama + TEST fidesz regresszio

ggplot(data_clean, aes(x = ossz_szavazo)) + geom_histogram(binwidth = 200)

freq(data_clean$szavazokor_size_binary)  #6.7% mini valasztokerulet


regression_fidesz_kicsi <- lm(egyeni_fidesz_pc ~ szavazokor_size_binary, data=data_clean)
summary(regression_fidesz_kicsi) #A Fidesz 10%-kal erÅ‘sebb kis szavazÃ³kÃ¶rÃ¶kben

kis_nagy_szavazokor <- data_clean[, .N, by = list(id, oevk)] 

#####SPLIT DATA PART (az ID alapú matching elvész, a data_clean a stabil dataset)

#TEST - create data by megye, szamold meg a szavazokat es szavazokoroket telepulesenkent

#megye, telepules, szavazokor szetvalasztasa

data_split <- separate(data_clean,
         col = "id",
         into = c("megye", "telepules_kerulet", "szavazokor_id"),
         sep = "/")

#data_clean_M01 <- data_clean[which(data_clean$megye=='M01')]
#data_clean_M02 <- data_clean[which(data_clean$megye=='M02')]
#data_clean_M03 <- data_clean[which(data_clean$megye=='M03')]
#data_clean_M04 <- data_clean[which(data_clean$megye=='M04')]
#data_clean_M05 <- data_clean[which(data_clean$megye=='M05')]

data_split_M06 <- data_split[which(data_split$megye=='M06')]

#data_clean_M07 <- data_clean[which(data_clean$megye=='M07')]
#data_clean_M08 <- data_clean[which(data_clean$megye=='M08')]
#data_clean_M09 <- data_clean[which(data_clean$megye=='M09')]
#data_clean_M10 <- data_clean[which(data_clean$megye=='M10')]
#data_clean_M11 <- data_clean[which(data_clean$megye=='M11')]
#data_clean_M12 <- data_clean[which(data_clean$megye=='M12')]
#data_clean_M13 <- data_clean[which(data_clean$megye=='M13')]
#data_clean_M14 <- data_clean[which(data_clean$megye=='M14')]
#data_clean_M15 <- data_clean[which(data_clean$megye=='M15')]
#data_clean_M16 <- data_clean[which(data_clean$megye=='M16')]
#data_clean_M17 <- data_clean[which(data_clean$megye=='M17')]
#data_clean_M18 <- data_clean[which(data_clean$megye=='M18')]
#data_clean_M19 <- data_clean[which(data_clean$megye=='M19')]
#data_clean_M20 <- data_clean[which(data_clean$megye=='M20')]

#PELDA - hodmezovasarhelyi 2014es adatok (2018-as majd kesobb addolva, egyelore Excelben van)


hodmezo2014 <- data_split_M06[telepules_kerulet == "T024"]

by_oevk_hodmezo <- hodmezo2014[, list(egyeni_fidesz, egyeni_jobbik, egyeni_lmp,
                                      egyeni_kormanyvaltok = egyeni_kormanyvaltok), by = list(oevk, megye, szavazokor_id, telepules_kerulet)]

hodmezo2014 <- hodmezo2014[, ossz_ellenzek := egyeni_jobbik + egyeni_lmp + egyeni_kormanyvaltok]

hodmezo2014_fidesz_ellenzek <- subset(hodmezo2014, select = c(egyeni_fidesz, ossz_ellenzek))

szavazokorok <- hodmezo2014[, hodmezo2014$egyeni_kormanyvaltok, by = szavazokor_id] 

hodmezo_2014_baloldal <- szavazokorok[c(1:50)]

baloldal_egyeni_ossz <- sum(hodmezo2014$egyeni_kormanyvaltok)
baloldal_egyeni_ossz <- sum(hodmezo2014$egyeni_kormanyvaltok)


#PELDA - csongrad 4-es korzet Hodmezovasarhely nelkul

csongrad4 <- data_split_M06[oevk == "CSONGRÃD 04"] 

csongrad4 <- csongrad4[, ossz_ellenzek := egyeni_jobbik + egyeni_lmp + egyeni_kormanyvaltok]

csongrad_fidesz_ellenzek <- subset(csongrad4, select = c(telepules_kerulet, egyeni_fidesz, ossz_ellenzek))

csongrad_fidesz_ellenzek_2 <- csongrad_fidesz_ellenzek[!telepules_kerulet %in% c("T024")]
