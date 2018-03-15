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
library(plyr)
library(dplyr)


# CHECK  WORKING DIRECTORY

setwd('C:/Users/adasan01/OneDrive - ARM/Desktop/TacticalVoting/Capstone')

# LOAD  DATA


### TELEPÜLÉS SZINTU ADATOK + KSH kodok addolása

data_telepules_kodok <- read.csv("106/adat/telepules/telepules_kodok.csv")

data_telepules_listas_arany <- read.csv("106/adat/telepules/telepules_listas_arany.csv")


data_telepules_merged <- merge(data_telepules_kodok, data_telepules_listas_arany, by = "id2010")

data_telepules_clean <- subset(data_telepules_merged, select = c(telepules_nev, telepules_id, ksh_kod, nuts3, partnev, szavazat2014, osszes2014, arany2014))

data_telepules_clean <- as.data.table(data_telepules_clean)

data_telepules_clean <- data_telepules_clean[!is.na(szavazat2014)]

osszes_listas_szavazat <- data_telepules_clean[, sum(szavazat2014), by = c("partnev", "telepules_nev")]

## FILTERING BY PARTY AND MERGING TO COLUMNS

data_telepules_baloldal <- data_telepules_clean[partnev == "baloldal"]

data_telepules_baloldal <- data_telepules_baloldal[, baloldal := szavazat2014] 
data_telepules_baloldal <- data_telepules_baloldal[, baloldal_pc := arany2014] 

data_telepules_baloldal <- subset(data_telepules_baloldal, select = c(telepules_id, baloldal, baloldal_pc))

data_telepules_fidesz <- data_telepules_clean[partnev == "fidesz"]

data_telepules_fidesz <- data_telepules_fidesz[, fidesz := szavazat2014] 
data_telepules_fidesz <- data_telepules_fidesz[, fidesz_pc := arany2014] 

data_telepules_fidesz <- subset(data_telepules_fidesz, select = c(telepules_id, fidesz, fidesz_pc))

data_telepules_jobbik <- data_telepules_clean[partnev == "jobbik"]

data_telepules_jobbik <- data_telepules_jobbik[, jobbik := szavazat2014] 
data_telepules_jobbik <- data_telepules_jobbik[, jobbik_pc := arany2014] 

data_telepules_jobbik <- subset(data_telepules_jobbik, select = c(telepules_id, jobbik, jobbik_pc))

data_telepules_lmp <- data_telepules_clean[partnev == "lmp"]

data_telepules_lmp <- data_telepules_lmp[, lmp := szavazat2014] 
data_telepules_lmp <- data_telepules_lmp[, lmp_pc := arany2014] 

data_telepules_lmp <- subset(data_telepules_lmp, select = c(telepules_id, lmp, lmp_pc))


data_telepules_egyeb <- data_telepules_clean[partnev == "egyeb"]

data_telepules_egyeb <- data_telepules_egyeb[, egyeb := szavazat2014] 
data_telepules_egyeb <- data_telepules_egyeb[, egyeb_pc := arany2014] 

data_telepules_egyeb <- subset(data_telepules_egyeb, select = c(telepules_id, egyeb, egyeb_pc))

#MERGING

data_clean_2 <- merge(data_telepules_clean, data_telepules_baloldal, by = "telepules_id")

data_clean_2 <- merge(data_clean_2, data_telepules_fidesz, by = "telepules_id")

data_clean_2 <- merge(data_clean_2, data_telepules_jobbik, by = "telepules_id")

data_clean_2 <- merge(data_clean_2, data_telepules_lmp, by = "telepules_id")

data_clean_2 <- merge(data_clean_2, data_telepules_egyeb, by = "telepules_id")

write.csv(data_clean_2, 'telepules_party_votes_columns.csv')

##removed duplicates in Excel and then finalizing the CLEAN data

data_telepules_reimport <- read.csv("telepules_party_votes_columns_clean.csv")

#data_telepules_final <- setnames(data_telepules_reimport, "ï..telepules_id", "telepules_id")

data_telepules_final <- within(data_telepules_reimport, rm(partnev, szavazat2014))

data_telepules_final <- as.data.table(data_telepules_final)

data_telepules_final <- data_telepules_final[, ksh_kod := ksh_kod / 10]

data_telepules_final <- data_telepules_final[, ksh_kod := trunc(ksh_kod)]

data_telepules_final <- data_telepules_final[, ksh_kod := as.factor(ksh_kod)]


### TStar adatok

data_koraranyok <- read.csv("106/adat/telepules/tstar/koraranyok.csv")

data_tamogatottak <- read.csv("106/adat/telepules/tstar/tamogatottak.csv")

data_munkaadok <- read.csv("106/adat/telepules/tstar/munkaadok.csv")

data_nyugdijasok <- read.csv("106/adat/telepules/tstar/nyugdijasok.csv")

data_allaskeresok <- read.csv("106/adat/telepules/tstar/allaskeresok.csv")

data_koraranyok <- as.data.table(data_koraranyok)
data_tamogatottak <- as.data.table(data_tamogatottak)
data_munkaadok <- as.data.table(data_munkaadok)
data_nyugdijasok <- as.data.table(data_nyugdijasok)
data_allaskeresok <- as.data.table(data_allaskeresok)


data_koraranyok_filtered <- data_koraranyok[ev.3 == "2014"] 

data_koraranyok_filtered <- setnames(data_koraranyok_filtered, "ksh_azonosito", "ksh_kod")

tstar_merge_1 <- join(data_telepules_final, data_koraranyok_filtered, type="left")
#merge_test2[is.na(merge_test2)] <- NA  #can be 0 or NA

data_tamogatottak_filtered <- data_tamogatottak[ev.3 == "2014"] 

data_tamogatottak_filtered <- setnames(data_tamogatottak_filtered, "ksh_azonosito", "ksh_kod")

tstar_merge_2 <- join(tstar_merge_1, data_tamogatottak_filtered, type="left")

data_munkaadok_filtered <- data_munkaadok[ev.3 == "2014"] 

data_munkaadok_filtered <- setnames(data_munkaadok_filtered, "ksh_azonosito", "ksh_kod")

tstar_merge_3 <- join(tstar_merge_2, data_munkaadok_filtered, type="left")

data_nyugdijasok_filtered <- data_nyugdijasok[ev.3 == "2014"] 

data_nyugdijasok_filtered <- setnames(data_nyugdijasok_filtered, "ksh_azonosito", "ksh_kod")

tstar_merge_4 <- join(tstar_merge_3, data_nyugdijasok_filtered, type="left")

data_allaskeresok_filtered <- data_allaskeresok[ev.3 == "2014"] 

data_allaskeresok_filtered <- setnames(data_allaskeresok_filtered, "ksh_azonosito", "ksh_kod")

tstar_merge_5 <- join(tstar_merge_4, data_allaskeresok_filtered, type="left")

tstar_telepules_all <- tstar_merge_5

### GLM települések szintjén

telepules_fidesz <- glm(fidesz_pc ~ ffi_65._aranya + nok_65._aranya + kozmunka_aranya + nyudijas_ffi_arany + nyudijas_noi_arany + allaskeresok_aranya_8osztalyse + allaskeresok_aranya_8osztaly, data = tstar_telepules_all)
summary(telepules_fidesz)
coeftest(telepules_fidesz)


#### SZAVAZOKÖRI ADATOK 

data <- read.csv("106/adat/jelolt/vote_counts_precincts_2a.csv")

data <- as.data.table(data)

#kamupartok egyesitese es szurese
kamu <- subset(data, select = c(id, mcp, haza_nem_elado, sms, fkgp, udp, sem, jesz, ump, munkaspart, szocdemek, kti, egyutt2014, zoldek, osszefogas))

kamu$egyeb <- rowSums( kamu[,2:15] )

kamu2 <- subset(kamu, select = c(id, egyeb))

data2 <- merge(data, kamu2, by="id")

#reszveteli adatok, 4 part listaja, egyeni, egyeb partok lista + megye, telepules, szavazokor kulon

data_clean <- subset(data2, select = c(id, telepules_id, atjelentkezettek, szavazokor, oevk, szavazok, reszvetel, fidesz, lmp, kormanyvaltok, jobbik, egyeni_fidesz, egyeni_lmp, egyeni_kormanyvaltok, egyeni_jobbik, egyeb))

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
               egyeni_jobbik_pc = sum(egyeni_jobbik) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik),
              szavazokorok_szama = sqrt(sum(szavazokorok_szama))),
                           by = oevk][order(oevk)] 

write.csv(by_oevk_2014, 'oevk2014_clean.csv')

new_by_oevk_2014 <- read.csv("oevk2014_clean_new.csv")

regression_fidesz_test <- glm(egyeni_fidesz_pc ~ szavazokorok_szama + telepulesek_szama + profil + varos_aranya, data=new_by_oevk_2014)
summary(regression_fidesz_test)
coeftest(regression_fidesz_test)

regression_kormanyvaltok_test <- glm(egyeni_kormanyvaltok_pc ~ szavazokorok_szama + telepulesek_szama + profil + varos_aranya, data=new_by_oevk_2014)
summary(regression_kormanyvaltok_test)
coeftest(regression_kormanyvaltok_test)

regression_jobbik_test <- glm(egyeni_jobbik_pc ~ szavazokorok_szama + telepulesek_szama + profil + varos_aranya, data=new_by_oevk_2014)
summary(regression_jobbik_test)
coeftest(regression_jobbik_test)

#uniform swing kalkulacio data_UNS
#REMOVED - it is in a different file


#TEST basic linear regressions - Fidesz vs. szavazokorok nagysaga, szama

data_clean$szavazokor_size <- cut(data_clean$ossz_szavazo, breaks = c(0, seq(100, 1000, by = 100), Inf), labels = 0:10)

data_clean$szavazokor_size <- as.integer(data_clean$szavazokor_size)

regression_fidesz_szavazokorok_nagysaga <- lm(egyeni_fidesz_pc ~ szavazokor_size, data=data_clean)
summary(regression_fidesz_szavazokorok_nagysaga)

regression_fidesz_szavazokorok_szama <- lm(egyeni_fidesz_pc ~ szavazokorok_szama, data=data_clean)
summary(regression_fidesz_szavazokorok_szama)



#1 OEVK nem szamolva (225+ szavazokorrel)
ggplot(data_clean, aes(szavazokorok_szama, egyeni_fidesz_pc)) + geom_point() + xlim(50, 170) + geom_smooth(method="lm", colour="green")

ggplot(data_clean, aes(szavazokor_size, egyeni_fidesz_pc )) + geom_point()  + geom_smooth(method="lm", colour="green")

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




##### Tstar adatok DRAFT 



data_telepules <- data_telepules[, tazon := telepules_nev]

write.csv(data_telepules, 'data_telepules_2018.csv')

data_koraranyok_filtered <- data_koraranyok[ev.3 == "2014"] 
data_koraranyok_filtered2 <- data_koraranyok[ev.3 == "2018"] 

data_tamogatottak_filtered <- data_tamogatottak[ev.3 == "2014"] 
data_tamogatottak_filtered2 <- data_tamogatottak[ev.3 == "2018"] 

data_munkaadok_filtered <- data_munkaadok[ev.3 == "2014"] 
data_munkaadok_filtered2 <- data_munkaadok[ev.3 == "2018"] 

data_nyugdijasok_filtered <- data_nyugdijasok[ev.3 == "2014"]
data_nyugdijasok_filtered2 <- data_nyugdijasok[ev.3 == "2018"] 

data_allaskeresok_filtered <- data_allaskeresok[ev.3 == "2014"]
data_allaskeresok_filtered2 <- data_allaskeresok[ev.3 == "2018"]
