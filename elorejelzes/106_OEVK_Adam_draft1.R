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

#megye, telepules, szavazokor szetvalasztasa

data_clean <- separate(data_clean,
         col = "id",
         into = c("megye", "telepules_kerulet", "szavazokor_id"),
         sep = "/")

#szavazokor sorszam - (converting character to num)

data_clean$szavazokor_id <- as.numeric(as.character(data_clean$szavazokor_id))


#szavazasra jogosultak szama / szavazokor

data_clean <- data_clean[, ossz_szavazo := round(szavazok / (reszvetel /100), digits = 0)] 

#szavazokorok szama 

data_szavazokorok_szama <- data_clean[, list(szavazokorok_szama = .N), by = oevk]

data_clean <- merge(data_clean, data_szavazokorok_szama, by = "oevk")


#UJ VALTOZOK - partok szazalekos egyeni eredmenye egymashoz viszonyitva (kamupartok nelkul)

data_clean <- data_clean[, egyeni_fidesz_pc := egyeni_fidesz / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp)] 

data_clean <- data_clean[, egyeni_kormanyvaltok_pc := egyeni_kormanyvaltok / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp)]

data_clean <- data_clean[, egyeni_jobbik_pc := egyeni_jobbik / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp)]

data_clean <- data_clean[, egyeni_lmp_pc := egyeni_lmp / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp)]


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

by_oevk <- data_clean[, list(szavazok, reszvetel, ossz_szavazo, egyeni_fidesz, egyeni_lmp, egyeni_kormanyvaltok, egyeni_jobbik, 
                             egyeni_fidesz_pc, egyeni_lmp_pc, egyeni_kormanyvaltok_pc, egyeni_jobbik_pc), 
                              by = list(oevk, megye, szavazokor_id, telepules_kerulet)]

by_oevk_2014 <- by_oevk[, list(szavazok = sum(szavazok),
                            reszvetel = sum(reszvetel),
                            ossz_szavazo = sum(ossz_szavazo),
                           egyeni_fidesz = sum(egyeni_fidesz),
                           egyeni_lmp = sum(egyeni_lmp),
                           egyeni_kormanyvaltok = sum(egyeni_kormanyvaltok),
                           egyeni_jobbik = sum(egyeni_jobbik),
                           egyeni_fidesz_pc = sum(egyeni_fidesz_pc),
                           egyeni_lmp_pc = sum(egyeni_lmp_pc),
                           egyeni_kormanyvaltok_pc = sum(egyeni_kormanyvaltok_pc),
                           egyeni_jobbik_pc = sum(egyeni_jobbik_pc)),
                           by = oevk][order(oevk)] 



#TEST - szavazokorok nagysaga - kicsi 1, 200 alatt, nem kicsi, 0, 200 felett

data_clean$szavazokor_size_binary [(data_clean$ossz_szavazo <= 200)] <- 1
data_clean$szavazokor_size_binary [(data_clean$ossz_szavazo > 200)] <- 0

#TEST - frequency stats mini szavazokorok szama + TEST fidesz regresszio

ggplot(data_clean, aes(x = ossz_szavazo)) + geom_histogram(binwidth = 200)

freq(data_clean$szavazokor_size_binary)  #6.7% mini valasztokerulet


regression_fidesz_kicsi <- lm(egyeni_fidesz_pc ~ szavazokor_size_binary, data=data_clean)
summary(regression_fidesz_kicsi) #A Fidesz 10%-kal erősebb kis szavazókörökben

kis_nagy_szavazokor <- data_clean[, .N, by = list(szavazokor_id, oevk)] 


#TEST - create data by megye, szamold meg a szavazokat es szavazokoroket telepulesenkent

#data_clean_M01 <- data_clean[which(data_clean$megye=='M01')]
#data_clean_M02 <- data_clean[which(data_clean$megye=='M02')]
#data_clean_M03 <- data_clean[which(data_clean$megye=='M03')]
#data_clean_M04 <- data_clean[which(data_clean$megye=='M04')]
#data_clean_M05 <- data_clean[which(data_clean$megye=='M05')]

data_clean_M06 <- data_clean[which(data_clean$megye=='M06')]

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


hodmezo2014 <- data_clean_M06[telepules_kerulet == "T024"]

by_oevk_hodmezo <- hodmezo2014[, list(egyeni_fidesz, egyeni_jobbik, egyeni_lmp,
                                      egyeni_kormanyvaltok = egyeni_kormanyvaltok), by = list(oevk, megye, szavazokor_id, telepules_kerulet)]

hodmezo2014 <- hodmezo2014[, ossz_ellenzek := egyeni_jobbik + egyeni_lmp + egyeni_kormanyvaltok]

hodmezo2014_fidesz_ellenzek <- subset(hodmezo2014, select = c(egyeni_fidesz, ossz_ellenzek))

szavazokorok <- hodmezo2014[, hodmezo2014$egyeni_kormanyvaltok, by = szavazokor_id] 

hodmezo_2014_baloldal <- szavazokorok[c(1:50)]

baloldal_egyeni_ossz <- sum(hodmezo2014$egyeni_kormanyvaltok)
baloldal_egyeni_ossz <- sum(hodmezo2014$egyeni_kormanyvaltok)


#PELDA - csongrad 4-es korzet Hodmezovasarhely nelkul

csongrad4 <- data_clean_M06[oevk == "CSONGRÁD 04"] 

csongrad4 <- csongrad4[, ossz_ellenzek := egyeni_jobbik + egyeni_lmp + egyeni_kormanyvaltok]

csongrad_fidesz_ellenzek <- subset(csongrad4, select = c(telepules_kerulet, egyeni_fidesz, ossz_ellenzek))

csongrad_fidesz_ellenzek_2 <- csongrad_fidesz_ellenzek[!telepules_kerulet %in% c("T024")]



########### IGNORE BELOW THIS LINE

#filtering
data$kamu <- subset(, select = c(deceased, female, age, eduyears_mod, income10g, sports, sports_never, sports_often))

#filtering
share <- subset(share, age>=50 & age<=80)
share2 <- subset(share, ever_smoked!="." & eduyears_mod!="." & income10g!="." & sports!="." & sports_never!="." & sports_often!=".")

#keeping only relevant colums - 21848 observations in total
dataset <- subset(share2, select = c(deceased, female, age, eduyears_mod, income10g, sports, sports_never, sports_often))
dataset$deceased <- as.numeric(dataset$deceased)


#original variable: sports 1-4, sports often = 1 if sports = 1, sports never = 1 if sports = 4
#new binary variable for sports: 1-2 = sports more, 3-4 = sports less

dataset$sports_index [(dataset$sports <= 2)] <- 1
dataset$sports_index [(dataset$sports >= 3)] <- 0

# Frequency tables, deceased = died 
freq(dataset$deceased)  # 5.735% died in 6 years
freq(dataset$sports_index) # 49% exercises less, 51% exercises more


# LPM: linear probability model
lpm2 <- lm(deceased ~ sports_index, data=dataset)
lpm1 <- lm(deceased ~  sports, data=dataset)
coeftest(lpm1, vcov=sandwich)
coeftest(lpm2, vcov=sandwich)

stargazer(list(lpm1, lpm2), digits=3, type="html", out="sports_mortality1.doc")

# regression line with deceased vs sports
ggplot(data = dataset, aes(x=sports, y=deceased)) +
  geom_smooth(method="lm", colour="navy") 

# regression line with deceased vs. sports more / sports less
ggplot(data = dataset, aes(x=sports_index, y=deceased)) +
  geom_smooth(method="lm", colour="red") 

# HANDLING CONFOUNDERS IN LPM to validate our estimation
# we will control for AGE, GENDER, EDUCATION, and INCOME GROUP

# Functional for for age - quadriatic relationship between age and deceased probability

ggplot(data = dataset, aes(x=age, y=deceased)) +
  geom_smooth(method="lm", colour="orange") +
  geom_smooth(method="lm", formula=y~poly(x,2), colour="navy") 

# fraction deceased by single years of age - shows that the quadriatic is the best relationship
# first creage single years of age
dataset$age_groups <- round(dataset$age)
byage <- aggregate(dataset$deceased, list(age_groups=dataset$age_groups), mean)
ggplot(data = dataset, aes(x=age, y=deceased)) +
  geom_line(data = byage, aes(x=age_groups, y=x), colour="orange", size=3) +
  geom_smooth(method="lm", colour="blue") +
  geom_smooth(method="lm", formula=y~poly(x,2), colour="navy") 

# Functional for for education - cubic is the best, but linear can also be interpreted

ggplot(data = dataset, aes(x=eduyears_mod, y=deceased)) +
  geom_smooth(method="lm", colour="orange") +
  geom_smooth(method="lm", formula=y~poly(x,3), colour="navy")


# Functional for for income group

# same without loess - cubic is the best
ggplot(data = dataset, aes(x=income10g, y=deceased)) +
  geom_smooth(method="lm", colour="orange") +
  geom_smooth(method="lm", formula=y~poly(x,3), colour="navy") 


lpm3 <- lm(deceased ~ sports + female + age +eduyears_mod + income10g, data=dataset)
lpm4 <- lm(deceased ~ sports + female + poly(age,2) + poly(eduyears_mod,3) + poly(income10g,3), data=dataset)
coeftest(lpm3, vcov=sandwich)
coeftest(lpm4, vcov=sandwich)

stargazer(list(lpm1, lpm3, lpm4), digits=3, type="html",out="sports_mortality_2.doc")


# PROBIT & LOGIT
logitcoeffs <- glm(deceased ~ sports, data=dataset, family='binomial')
logitmarg <- logitmfx(formula = deceased ~ sports, data=dataset, atmean=FALSE)
summary(logitcoeffs)
print(logitmarg)

logitcoeffs_2 <- glm(deceased ~ sports + female + age +eduyears_mod + income10g, data=dataset, family='binomial')
logitmarg_2 <- logitmfx(formula = deceased ~ sports + female + age +eduyears_mod + income10g, data=dataset, atmean=FALSE)
summary(logitcoeffs_2)
print(logitmarg_2)

logitcoeffs_3 <- glm(deceased ~ sports + female + poly(age,2) + poly(eduyears_mod,3) + poly(income10g,3), data=dataset, family='binomial')
logitmarg_3 <- logitmfx(formula = deceased ~ sports + female + poly(age,2) + poly(eduyears_mod,3) + poly(income10g,3), data=dataset, atmean=FALSE)
summary(logitcoeffs_3)
print(logitmarg_3)


stargazer(list(lpm4, logitcoeffs_3), digits=3, type="html", out="sports_mortality_3.doc")

dataset$pred_lpm <- predict.lm(lpm4)
dataset$pred_logit <- predict.glm(logitcoeffs_3, type="response")

ggplot(data = dataset, aes(x=pred_logit, y=pred_lpm)) +
  geom_line(aes(x=pred_logit, y=pred_logit), colour="orange") +
  geom_point()

