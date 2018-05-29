
#uniform swing kalkulacio data_UNS

#new inputs - orszagos pollok ?tlaga -4% Fidesz, friss?tve m?rcius 2-?n

#2014 eredm?ny Fidesz 
# Fidesz 44.13
# Jobbik 20.34
# Korm?nyv?lt?k 26.86
# LMP 4.98
# Egy?b 3.69

#2018 marcius 
#Fidesz 44.5%, 
#MSZP-P 14%
#Jobbik 20.5% 
#LMP 9%
# DK 6.5%
#Momentum 2.5%
#Egyeb 3%

#?lland? 2014-es p?rt szorz?k

data_UNS <- data_clean[, Fidesz_2014_orszagos := 44.13] 
data_UNS <- data_UNS[, Kormanyvaltok_2014_orszagos := 26.86]
data_UNS <- data_UNS[, Jobbik_2014_orszagos := 20.34]
data_UNS <- data_UNS[, LMP_2014_orszagos := 4.98]
data_UNS <- data_UNS[, Egyeb_2014_orszagos := 3.69]

data_UNS <- data_UNS[, reszvetel_2014 := 60.99] 

#friss?theto ?j adatok alapj?n

data_UNS <- data_UNS[, Fidesz_2018_orszagos := 41] 
data_UNS <- data_UNS[, MSZP_P_2018_orszagos := 12.5]
data_UNS <- data_UNS[, Jobbik_2018_orszagos := 23.5]
data_UNS <- data_UNS[, LMP_2018_orszagos := 6]
data_UNS <- data_UNS[, DK_2018_orszagos := 7.5]
data_UNS <- data_UNS[, Momentum_2018_orszagos := 5]
data_UNS <- data_UNS[, Egyutt_2018 := 2]
data_uns <- data_UNS[, MKKP_2018 := 1]
#egyeb 1.5%


data_UNS <- data_UNS[, reszvetel_2018 := 71.5]  

#kiegeszito 2014-es benchmarkok az MSZP-P, DK, es Momentum adataihoz

data_UNS <- data_UNS[, MSZP_P_2014_orszagos := (MSZP_P_2018_orszagos / (DK_2018_orszagos + MSZP_P_2018_orszagos)) * Kormanyvaltok_2014_orszagos ]

data_UNS <- data_UNS[, DK_2014_orszagos := (DK_2018_orszagos / (DK_2018_orszagos + MSZP_P_2018_orszagos)) * Kormanyvaltok_2014_orszagos ]

data_UNS <- data_UNS[, Momentum_2014_orszagos :=  0.1 * LMP_2014_orszagos + 0.2 * Egyeb_2014_orszagos]

data_UNS <- data_UNS[, reszvetel_2018_szorzo :=  reszvetel_2018 / reszvetel_2014]

# 2018-as UNS vote share


by_oevk_UNS_2018 <- data_UNS[, list(Fidesz_2014 = sum(egyeni_fidesz),
                                    MSZP_DK_P_2014 = sum(egyeni_kormanyvaltok),
                                    LMP_2014 = sum(egyeni_lmp),
                                    Jobbik_2014 = sum(egyeni_jobbik),
                                    Fidesz_2018 = sum(egyeni_fidesz * (Fidesz_2018_orszagos / Fidesz_2014_orszagos) * reszvetel_2018_szorzo),
                                    Jobbik_2018 = sum(egyeni_jobbik * (Jobbik_2018_orszagos / Jobbik_2014_orszagos) * reszvetel_2018_szorzo),
                                    MSZP_P_2018 = sum(egyeni_kormanyvaltok * (MSZP_P_2018_orszagos / Kormanyvaltok_2014_orszagos) * reszvetel_2018_szorzo),
                                    DK_2018 = sum(egyeni_kormanyvaltok * (DK_2018_orszagos / Kormanyvaltok_2014_orszagos) * reszvetel_2018_szorzo),
                                    LMP_2018 = sum(egyeni_lmp * (LMP_2018_orszagos / LMP_2014_orszagos) * reszvetel_2018_szorzo),
                                    Momentum_2018 = sum((egyeni_lmp * 1) * reszvetel_2018_szorzo),
                                    Egyutt_2018 = sum((egyeni_lmp * 0.4) * reszvetel_2018_szorzo),
                                    MKKP_2018 = sum((egyeni_lmp * 0.2) * reszvetel_2018_szorzo)),
                             by = oevk][order(oevk)]

Fidesz_2018_votes <- sum(by_oevk_UNS_2018$Fidesz_2018)
Fidesz_2018_votes

Jobbik_2018_votes <- sum(by_oevk_UNS_2018$Jobbik_2018)
Jobbik_2018_votes

MSZP_P_2018_votes <- sum(by_oevk_UNS_2018$MSZP_P_2018)
MSZP_P_2018_votes

DK_2018 <- sum(by_oevk_UNS_2018$DK_2018)
DK_2018

LMP_2018_votes <- sum(by_oevk_UNS_2018$LMP_2018)
LMP_2018_votes

Momentum_2018_votes <- sum(by_oevk_UNS_2018$Momentum_2018)
Momentum_2018_votes

Egyutt_2018_votes <- sum(by_oevk_UNS_2018$Egyutt_2018)
Egyutt_2018_votes

MKKP_2018_votes <- sum(by_oevk_UNS_2018$MKKP_2018)
MKKP_2018_votes

osszes_szavazat <- by_oevk_UNS_2018[, list(Fidesz = round(sum(Fidesz_2018), digits = 0), 
                                           Jobbik = round(sum(Jobbik_2018), digits = 0), 
                                           MSZP = round(sum(MSZP_P_2018), digits = 0), 
                                           DK = round(sum(DK_2018), digits = 0), LMP  = round(sum(LMP_2018), digits = 0), 
                                           Momentum = round(sum(Momentum_2018), digits = 0),
                                           Egyutt = round(sum(Egyutt_2018), digits = 0),
                                           MKKP = round(sum(MKKP_2018), digits = 0))]

rowSums(osszes_szavazat)


write.csv(by_oevk_UNS_2018,'2018_UNS_06_April_optimist.csv')

##### TEST PART - playing with some regressions on szavazokor nagysaga

#TEST - szavazokorok nagysaga - kicsi 1, 200 alatt, nem kicsi, 0, 200 felett

data_clean$szavazokor_size_binary [(data_clean$ossz_szavazo <= 200)] <- 1
data_clean$szavazokor_size_binary [(data_clean$ossz_szavazo > 200)] <- 0

#TEST - frequency stats mini szavazokorok szama + TEST fidesz regresszio

ggplot(data_clean, aes(x = ossz_szavazo)) + geom_histogram(binwidth = 200)

freq(data_clean$szavazokor_size_binary)  #6.7% mini valasztokerulet


regression_fidesz_kicsi <- lm(egyeni_fidesz_pc ~ szavazokor_size_binary, data=data_clean)
summary(regression_fidesz_kicsi) #A Fidesz 10%-kal erÅsebb kis szavazÃ³kÃ¶rÃ¶kben

kis_nagy_szavazokor <- data_clean[, .N, by = list(id, oevk)] 

#####SPLIT DATA PART (az ID alap? matching elv?sz, a data_clean a stabil dataset)

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
