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
library(dplyr)
library(rgenoud)
library(anchors)
library(descr)
library(dplyr)
library(plyr)

# CHECK  WORKING DIRECTORY
getwd()
setwd('C:/Users/adasan01/OneDrive - ARM/Documents/GitHub')

# LOAD  DATA
data <- read.csv("106/adat/jelolt/vote_counts_precincts_2a.csv")

data <- as.data.table(data)

#kamupartok egyesitese es szurese
kamu <- subset(data, select = c(id, mcp, haza_nem_elado, sms, fkgp, udp, sem, jesz, ump, munkaspart, szocdemek, kti, egyutt2014, zoldek, osszefogas))

kamu$egyeb <- rowSums( kamu[,2:15] )

kamu2 <- subset(kamu, select = c(id, egyeb))

data2 <- merge(data, kamu2, by="id")

#reszveteli adatok, 4 part listaja, egyeni, egyeb partok lista + megye, telepules, szavazokor kulon

data_clean <- subset(data2, select = c(id, telepules_id, atjelentkezettek, szavazokor, oevk, szavazok, reszvetel, fidesz, lmp, kormanyvaltok, jobbik, egyeni_fidesz, egyeni_lmp, egyeni_kormanyvaltok, egyeni_jobbik, egyeb))

###NEW STUFF - telepules szintu adatok + KSH kodok addol?sa

#data_telepules_kodok <- read.csv("106/adat/telepules/telepules_kodok.csv")

#data_telepules_listas_arany <- read.csv("106/adat/telepules/telepules_listas_arany.csv")


#data_telepules_merged <- merge(data_telepules_kodok, data_telepules_listas_arany, by = "id2010")

#data_telepules_clean <- subset(data_telepules_merged, select = c(telepules_nev, telepules_id, ksh_kod, nuts3, partnev, szavazat2014, osszes2014, arany2014))

#data_telepules_clean <- as.data.table(data_telepules_clean)

#osszes_listas_szavazat <- data_telepules_clean[, sum(szavazat2014), by = partnev]


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

new_by_oevk_2014 <- read.csv("106/adat/jelolt/oevk2014_clean_new.csv")




######2010-es adatok beolvas?sa ?s merge-?l?se

data_2010 <- read.csv("106/adat/2010/listas.csv")

data2_2010 <- read.csv("106/adat/2010/szervezet.csv")

data_reszvetel_2010 <- read.csv("106/adat/2010/reszvetel.csv")

colnames(data2_2010)[1] <- "part"

data_2010_merge <- join(data_2010, data2_2010, type="left")

#filter 4 p?rtra - merge and cleanse az osszes elnevezes variaciojat

data_2010_merge <- as.data.table(data_2010_merge)

data_2010_merge_MSZP <- data_2010_merge[szervezet == "MSZP"]

data_2010_merge_MSZP2 <- data_2010_merge[szervezet == "MAGYAR SZOCIALISTA PÁRT"] 
data_2010_merge_MSZP2$szervezet[data_2010_merge_MSZP2$szervezet=='MAGYAR SZOCIALISTA PÁRT'] <- 'MSZP'

data_2010_merge_MSZP3 <- data_2010_merge[szervezet == "Magyar Szocialista Párt"] 
data_2010_merge_MSZP3$szervezet[data_2010_merge_MSZP3$szervezet=='Magyar Szocialista Párt'] <- 'MSZP'

data_2010_merge_MSZP_all<-rbind(data_2010_merge_MSZP, data_2010_merge_MSZP2, data_2010_merge_MSZP3)

data_2010_merge_LMP <- data_2010_merge[szervezet == "LMP"] 

data_2010_merge_LMP2 <- data_2010_merge[szervezet == "Lehet Más a Politika"] 
data_2010_merge_LMP2$szervezet[data_2010_merge_LMP2$szervezet=='Lehet Más a Politika'] <- 'LMP'

data_2010_merge_LMP3 <- data_2010_merge[szervezet == "LEHET MÁS A POLITIKA"]
data_2010_merge_LMP3$szervezet[data_2010_merge_LMP3$szervezet=='LEHET MÁS A POLITIKA'] <- 'LMP'

data_2010_merge_LMP_all <-rbind(data_2010_merge_LMP, data_2010_merge_LMP2, data_2010_merge_LMP3)

data_2010_merge_JOBBIK <- data_2010_merge[szervezet == "JOBBIK"]

data_2010_merge_JOBBIK2 <- data_2010_merge[szervezet == "Jobbik"]
data_2010_merge_JOBBIK2$szervezet[data_2010_merge_JOBBIK2$szervezet=='Jobbik'] <- 'JOBBIK'

data_2010_merge_JOBBIK_all <-rbind(data_2010_merge_JOBBIK, data_2010_merge_JOBBIK2)

data_2010_merge_FIDESZ <- data_2010_merge[szervezet == "FIDESZ-KDNP"]
data_2010_merge_FIDESZ$szervezet <- as.character(as.factor(data_2010_merge_FIDESZ$szervezet))
data_2010_merge_FIDESZ$szervezet[data_2010_merge_FIDESZ$szervezet=='FIDESZ-KDNP'] <- 'FIDESZ'
data_2010_merge_FIDESZ$szervezet <- as.factor(as.character(data_2010_merge_FIDESZ$szervezet))

data_2010_merge_FIDESZ2 <- data_2010_merge[szervezet == "FIDESZ - KDNP"] 
data_2010_merge_FIDESZ2$szervezet <- as.character(as.factor(data_2010_merge_FIDESZ2$szervezet))
data_2010_merge_FIDESZ2$szervezet[data_2010_merge_FIDESZ2$szervezet == "FIDESZ - KDNP"] <- 'FIDESZ'
data_2010_merge_FIDESZ2$szervezet <- as.factor(as.character(data_2010_merge_FIDESZ2$szervezet))


data_2010_merge_FIDESZ3 <- data_2010_merge[szervezet == "Fidesz-KDNP"] 
data_2010_merge_FIDESZ3$szervezet <- as.character(as.factor(data_2010_merge_FIDESZ3$szervezet))
data_2010_merge_FIDESZ3$szervezet[data_2010_merge_FIDESZ3$szervezet == "Fidesz-KDNP"] <- 'FIDESZ'
data_2010_merge_FIDESZ3$szervezet <- as.factor(as.character(data_2010_merge_FIDESZ3$szervezet))

data_2010_merge_FIDESZ_all <-rbind(data_2010_merge_FIDESZ, data_2010_merge_FIDESZ2, data_2010_merge_FIDESZ3)

#?tnevezni az oszlopokat, hogy egy oszlop 1 p?rt szavazata legyen a merge-n?l

colnames(data_2010_merge_MSZP_all)[2] <- "MSZP_2010"

colnames(data_2010_merge_FIDESZ_all)[2] <- "FIDESZ_2010"

colnames(data_2010_merge_JOBBIK_all)[2] <- "JOBBIK_2010"

colnames(data_2010_merge_LMP_all)[2] <- "LMP_2010"

data_2010_all <- merge(data_2010_merge_FIDESZ_all, data_2010_merge_JOBBIK_all, by="szavazokor")

data_2010_all_2 <- merge(data_2010_all, data_2010_merge_MSZP_all, by="szavazokor")

data_2010_all_3 <- merge(data_2010_all_2, data_2010_merge_LMP_all, by="szavazokor")

data_reszvetel_2010 <- as.data.table(data_reszvetel_2010)

data_2010_clean <- subset(data_2010_all_3, select = c(szavazokor, FIDESZ_2010, JOBBIK_2010, MSZP_2010, LMP_2010))

data_2010_clean <-  merge(data_2010_clean, data_reszvetel_2010, by="szavazokor")

### MERGING 2010 ?s 2014 by szavazokor

colnames(data_2010_clean)[1] <- "id"


data_2010_2014 <- merge(by_szavazokor_2014, data_2010_clean, by="id")


data_2010_2014 <- data_2010_2014[, reszvetel_2010 := round(ervenyes / (valasztopolgarok /100), digits = 2)] 

colnames(data_2010_2014)[3] <- "szavazo_2014"
colnames(data_2010_2014)[4] <- "reszvetel_2014"
colnames(data_2010_2014)[5] <- "ossz_szavazo_2014"
colnames(data_2010_2014)[6] <- "FIDESZ_2014"
colnames(data_2010_2014)[7] <- "LMP_2014"
colnames(data_2010_2014)[8] <- "MSZP_2014"
colnames(data_2010_2014)[9] <- "JOBBIK_2014"
colnames(data_2010_2014)[15] <- "ossz_szavazo_2010"
colnames(data_2010_2014)[16] <- "szavazo_2010"
colnames(data_2010_2014)[18] <- "reszvetel_2010"

data_2010_2014 <- data_2010_2014[, FIDESZ_2010_pc := round((FIDESZ_2010 / ossz_szavazo_2010), digits = 4)] 
data_2010_2014 <- data_2010_2014[, MSZP_2010_pc := round((MSZP_2010 / ossz_szavazo_2010), digits = 4)] 
data_2010_2014 <- data_2010_2014[, JOBBIK_2010_pc := round((JOBBIK_2010 / ossz_szavazo_2010), digits = 4)]  
data_2010_2014 <- data_2010_2014[, LMP_2010_pc := round((LMP_2010 / ossz_szavazo_2010), digits = 4)] 

data_2010_2014 <- data_2010_2014[, FIDESZ_2014_pc := round((FIDESZ_2014 / ossz_szavazo_2014), digits = 4)] 
data_2010_2014 <- data_2010_2014[, MSZP_2014_pc := round((MSZP_2014 / ossz_szavazo_2014), digits = 4)] 
data_2010_2014 <- data_2010_2014[, JOBBIK_2014_pc := round((JOBBIK_2014 / ossz_szavazo_2014), digits = 4)]  
data_2010_2014 <- data_2010_2014[, LMP_2014_pc := round((LMP_2014 / ossz_szavazo_2014), digits = 4)] 

## MERGE szavazokori profilok 2014-bol - v?rosok ar?nya, telepulesszam, stb.

data_2010_2014_szavazokor <-  merge(data_2010_2014, new_by_oevk_2014, by="oevk")

###GLM by szavazokor ?s OEVK - profil, varos, 2010-2014-es adat ?sszehasonl?tva

##GLM SZAVAZOKOR

regression_2010_2014_fidesz <- glm(FIDESZ_2014_pc ~ FIDESZ_2010_pc + szavazokorok_szama.x + telepulesek_szama + profil + varos_aranya, data=data_2010_2014_szavazokor)
summary(regression_2010_2014_fidesz)
coeftest(regression_2010_2014_fidesz)

logitcoeffs_fidesz_2010_2014 <- glm(FIDESZ_2014_pc ~ FIDESZ_2010_pc + szavazokorok_szama.x + telepulesek_szama + profil + varos_aranya, data=data_2010_2014_szavazokor, family='binomial')

summary(logitcoeffs_fidesz_2010_2014)

data_2010_2014_szavazokor$pred_logit <- predict.glm(logitcoeffs_fidesz_2010_2014, type="response")

ggplot(data = data_2010_2014_szavazokor, aes(x=FIDESZ_2014_pc, y= pred_logit)) + xlim(0.08, 0.55) + ylim(0.08, 0.55) +
  geom_line(aes(x=pred_logit, y=pred_logit), colour="orange") +
  geom_point()


## GLM OEVK

data_2010_2014_OEVK <- data_2010_2014_szavazokor[, list(szavazo_2014 = sum(szavazo_2014),
                                          reszvetel_2014 = sum(szavazo_2014) / sum(ossz_szavazo_2014),
                                          reszvetel_2010 = sum(szavazo_2010) / sum(ossz_szavazo_2010),
                                          FIDESZ_2010 = sum(FIDESZ_2010),
                                          FIDESZ_2014 = sum(FIDESZ_2014),
                                          LMP_2010 = sum(LMP_2010),
                                          LMP_2014 = sum(LMP_2014),
                                          MSZP_2010 = sum(MSZP_2010),
                                          MSZP_2014 = sum(MSZP_2014),
                                          JOBBIK_2010 = sum(JOBBIK_2010),
                                          JOBBIK_2014 = sum(JOBBIK_2014),
                                          FIDESZ_2010_pc = sum(FIDESZ_2010) / (sum(FIDESZ_2010) + sum(LMP_2010) + sum(MSZP_2010) + sum(JOBBIK_2010)),
                                          FIDESZ_2014_pc = sum(FIDESZ_2014) / (sum(FIDESZ_2014) + sum(LMP_2014) + sum(MSZP_2014) + sum(JOBBIK_2014)),
                                          MSZP_2010_pc = sum(MSZP_2010) / (sum(FIDESZ_2010) + sum(LMP_2010) + sum(MSZP_2010) + sum(JOBBIK_2010)),
                                          MSZP_2014_pc = sum(MSZP_2014) / (sum(FIDESZ_2014) + sum(LMP_2014) + sum(MSZP_2014) + sum(JOBBIK_2014)),
                                          JOBBIK_2010_pc = sum(JOBBIK_2010) / (sum(FIDESZ_2010) + sum(LMP_2010) + sum(MSZP_2010) + sum(JOBBIK_2010)),
                                          JOBBIK_2014_pc = sum(JOBBIK_2014) / (sum(FIDESZ_2014) + sum(LMP_2014) + sum(MSZP_2014) + sum(JOBBIK_2014)),
                                          LMP_2010_pc = sum(LMP_2010) / (sum(FIDESZ_2010) + sum(LMP_2010) + sum(MSZP_2010) + sum(JOBBIK_2010)),
                                          LMP_2014_pc = sum(LMP_2014) / (sum(FIDESZ_2014) + sum(LMP_2014) + sum(MSZP_2014) + sum(JOBBIK_2014)),
                                          szavazokorok_szama = szavazokorok_szama.x,
                                          profil = profil,
                                          telepulesek_szama = telepulesek_szama,
                                          varos_aranya = varos_aranya),
                                   by = oevk][order(oevk)] 

data_2010_2014_OEVK <- unique(data_2010_2014_OEVK, by = "oevk")

regression_2010_2014_fidesz_OEVK <- glm(FIDESZ_2014_pc ~ FIDESZ_2010_pc + szavazokorok_szama + telepulesek_szama + profil + varos_aranya, data=data_2010_2014_OEVK)
summary(regression_2010_2014_fidesz_OEVK)
coeftest(regression_2010_2014_fidesz_OEVK)

##FIDESZ

logitcoeffs_FIDESZ_2010_2014_OEVK <- glm(FIDESZ_2014_pc ~ FIDESZ_2010_pc + szavazokorok_szama + telepulesek_szama + profil + varos_aranya, data=data_2010_2014_OEVK, family='binomial')

summary(logitcoeffs_FIDESZ_2010_2014_OEVK)

data_2010_2014_OEVK$pred_logit_FIDESZ <- predict.glm(logitcoeffs_FIDESZ_2010_2014_OEVK, type="response")

ggplot(data = data_2010_2014_OEVK, aes(x=FIDESZ_2014_pc, y= pred_logit_FIDESZ)) + xlim(0.3, 0.6) + ylim(0.3, 0.6) +
  geom_line(aes(x=pred_logit_FIDESZ, y=pred_logit_FIDESZ), colour="orange") +
  geom_point()

##MSZP

logitcoeffs_MSZP_2010_2014_OEVK <- glm(MSZP_2014_pc ~ MSZP_2010_pc + szavazokorok_szama + telepulesek_szama + profil + varos_aranya, data=data_2010_2014_OEVK, family='binomial')

summary(logitcoeffs_MSZP_2010_2014_OEVK)

data_2010_2014_OEVK$pred_logit_MSZP <- predict.glm(logitcoeffs_MSZP_2010_2014_OEVK, type="response")

ggplot(data = data_2010_2014_OEVK, aes(x=MSZP_2014_pc, y= pred_logit_MSZP)) + xlim(0.15, 0.55) + ylim(0.15, 0.55) +
  geom_line(aes(x=pred_logit_MSZP, y=pred_logit_MSZP), colour="red") +
  geom_point()

#JOBBIK

logitcoeffs_JOBBIK_2010_2014_OEVK <- glm(JOBBIK_2014_pc ~ JOBBIK_2010_pc + szavazokorok_szama + telepulesek_szama + profil + varos_aranya, data=data_2010_2014_OEVK, family='binomial')

summary(logitcoeffs_JOBBIK_2010_2014_OEVK)

data_2010_2014_OEVK$pred_logit_JOBBIK <- predict.glm(logitcoeffs_JOBBIK_2010_2014_OEVK, type="response")

ggplot(data = data_2010_2014_OEVK, aes(x=JOBBIK_2014_pc, y= pred_logit_JOBBIK)) + xlim(0.05, 0.4) + ylim(0.05, 0.4) +
  geom_line(aes(x=pred_logit_JOBBIK, y=pred_logit_JOBBIK), colour="blue") +
  geom_point()

#LMP

logitcoeffs_LMP_2010_2014_OEVK <- glm(LMP_2014_pc ~ LMP_2010_pc + szavazokorok_szama + telepulesek_szama + profil + varos_aranya, data=data_2010_2014_OEVK, family='binomial')

summary(logitcoeffs_JOBBIK_2010_2014_OEVK)

data_2010_2014_OEVK$pred_logit_LMP <- predict.glm(logitcoeffs_LMP_2010_2014_OEVK, type="response")

ggplot(data = data_2010_2014_OEVK, aes(x=LMP_2014_pc, y= pred_logit_LMP)) + xlim(0.00, 0.15) + ylim(0.00, 0.15) +
  geom_line(aes(x=pred_logit_LMP, y=pred_logit_LMP), colour="pink") +
  geom_point()


### REGRESSION TEST

data_telepules_baloldal <- data_telepules_clean[partnev == "baloldal"]

data_telepules_baloldal <- data_telepules_baloldal[, baloldal := szavazat2014] 

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

#new inputs - orszagos pollok ?tlaga -4% Fidesz, friss?tve m?rcius 2-?n

#2014 eredm?ny Fidesz 
# Fidesz 44.13
# Jobbik 20.34
# Korm?nyv?lt?k 26.86
# LMP 4.98
# Egy?b 3.69

#2018 m?rcius 
#Fidesz 45%, 
#MSZP-P 17%
#Jobbik 18% 
#LMP 9%
# DK 6%
#Momentum 2%
#Egy?b 3%

#?lland? 2014-es p?rt szorz?k

data_UNS <- data_clean[, Fidesz_2014_orszagos := 44.13] 
data_UNS <- data_UNS[, Kormanyvaltok_2014_orszagos := 26.86]
data_UNS <- data_UNS[, Jobbik_2014_orszagos := 20.34]
data_UNS <- data_UNS[, LMP_2014_orszagos := 4.98]
data_UNS <- data_UNS[, Egyeb_2014_orszagos := 3.69]

data_UNS <- data_UNS[, reszvetel_2014 := 60.99] 

#friss?theto ?j adatok alapj?n

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


by_szavazokor_UNS_2018 <- data_UNS[, list(Fidesz_2014 = egyeni_fidesz,
                                    MSZP_DK_P_2014 = egyeni_kormanyvaltok,
                                    LMP_2014 = egyeni_lmp,
                                    Jobbik_2014 = egyeni_jobbik,
                                    Fidesz_2018 = egyeni_fidesz * (Fidesz_2018_orszagos / Fidesz_2014_orszagos) * reszvetel_2018_szorzo,
                                    Jobbik_2018 = egyeni_jobbik * (Jobbik_2018_orszagos / Jobbik_2014_orszagos) * reszvetel_2018_szorzo,
                                    MSZP_P_2018 = egyeni_kormanyvaltok * ((MSZP_P_2018_orszagos / MSZP_P_2014_orszagos) * (MSZP_P_2018_orszagos / (DK_2018_orszagos + MSZP_P_2018_orszagos))) * reszvetel_2018_szorzo,
                                    DK_2018 = egyeni_kormanyvaltok * ((DK_2018_orszagos / DK_2014_orszagos) * (DK_2018_orszagos / (DK_2018_orszagos + MSZP_P_2018_orszagos))) * reszvetel_2018_szorzo,
                                    LMP_2018 = egyeni_lmp * (LMP_2018_orszagos / LMP_2014_orszagos) * reszvetel_2018_szorzo,
                                    Momentum_2018 = (egyeni_lmp * 0.2) + (egyeb * 0.4) * (Momentum_2018_orszagos / Momentum_2014_orszagos) * reszvetel_2018_szorzo,
                                    reszvetel_2014 = reszvetel,
                                    reszvetel_2018 = reszvetel * reszvetel_2018_szorzo,
                                   szavazokorok_szama = szavazokorok_szama),
                             by = list(oevk, id)][order(oevk)]

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

by_szavazokor_UNS_2018 <- by_szavazokor_UNS_2018[, egyeni_fidesz_2018_pc := egyeni_fidesz / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)] 

by_szavazokor_UNS_2018 <- by_szavazokor_UNS_2018[, egyeni_MSZP_2018_pc := egyeni_kormanyvaltok / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)]

by_szavazokor_UNS_2018 <- by_szavazokor_UNS_2018[, egyeni_jobbik_2018_pc := egyeni_jobbik / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)]

by_szavazokor_UNS_2018 <- by_szavazokor_UNS_2018[, egyeni_lmp_2018_pc := egyeni_lmp / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)]

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
summary(regression_fidesz_kicsi) #A Fidesz 10%-kal erősebb kis szavazókörökben


