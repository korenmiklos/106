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
setwd('C:/Users/adasan01/OneDrive - ARM/Desktop/TacticalVoting/2018 adat')

# LOAD  DATA
data <- read.csv("2018_egyeni_listas_29May.csv")
data[is.na(data)] <- 0

#data <- as.data.table(data)

#data_telepules <- read.csv("106/adat/telepules/telepules_kodok.csv")

#kamupartok - nem 8 fo p?rt - egyesitese es szurese
egyeni_egyeb <- subset(data, select = c(megyeid, megye, oevk, telepid, szavazokor, egyeni_na, egyeni_haza_partja, egyeni_aqp, egyeni_civil_mozgalom, egyeni_csp, egyeni_demokrata_part, egyeni_dmp, egyeni_ecdp, egyeni_emmo, egyeni_ep, egyeni_erp, egyeni_eu_alternativa, egyeni_eu_rom, egyeni_fitip, egyeni_fkgp, egyeni_fuggetlen, egyeni_hajra_magyarorszag, egyeni_ham, egyeni_haza_mindenkie, egyeni_hhp, egyeni_ima, egyeni_iranytu, egyeni_jmp, egyeni_jo_ut_mpp, egyeni_kedn, egyeni_kpp, egyeni_kossz, egyeni_kozos_nevezo, egyeni_lendulettel, egyeni_mcp, egyeni_medete_part, egyeni_minokp, egyeni_miszep, egyeni_miep, egyeni_mmm, egyeni_moma, egyeni_munkaspart, egyeni_mvmp, egyeni_neem, egyeni_nemzet_beke, egyeni_nemzeti_zoldek, egyeni_nop, egyeni_np, egyeni_ocp, egyeni_op, egyeni_opre_roma, egyeni_rend_part, egyeni_sem, egyeni_szem_part, egyeni_szp, egyeni_tamp, egyeni_ebmp, egyeni_ertunk_ertetek, egyeni_umf))
orsz_egyeb <- subset(data, select = c(megyeid, megye, oevk, telepid, szavazokor, orsz_csaladok_partja, orsz_eu_roma_keresztenyek, orsz_iranytu, orsz_kossz, orsz_kozos_nevezo, orsz_miep, orsz_magyar_munkaspart, orsz_ciganypart, orsz_mo_dolgozo_tanulo_emberek, orsz_net_part, orsz_rend_elszamoltatas, orsz_sportos_egeszseges_mo, orsz_szegeny_emberek_mo, orsz_tenni_akaras_mozgalom, orsz_osszefogas_part))

egyeni_egyeb$egyeni_egyeb <- rowSums( egyeni_egyeb[, 6:59] )

orsz_egyeb$orsz_egyeb <- rowSums( orsz_egyeb[, 6:20] )

egyeni_egyeb2 <- subset(egyeni_egyeb, select = c(megyeid, megye, oevk, telepid, szavazokor, egyeni_egyeb))

orsz_egyeb2 <- subset(orsz_egyeb, select = c(megyeid, megye, oevk, telepid, szavazokor, orsz_egyeb))

data2 <- merge(data, egyeni_egyeb2, by=c("megyeid", "megye", "oevk", "telepid", "szavazokor"))

data3 <- merge(data2, orsz_egyeb2, by=c("megyeid", "megye", "oevk", "telepid", "szavazokor"))

#leszurni a fobb p?rtokat + egy?b p?rtokat egy?tt

data3 <- as.data.table(data3)

data_clean <- data3[, c("megyeid", "megye", "oevk", "telepid", "szavazokor", "telep", "nevjegyz", "megjel", "nevjegyz_atjel", "megjel_atjel", "boritek_atjel", "pecset_nelkul", "pecsettel", "tobblet_hiany", "ervenytelen", "ervenyes", "egyeni_dk", "egyeni_egyutt", "egyeni_fidesz", "egyeni_jobbik", "egyeni_lmp", "egyeni_mszp", "egyeni_momentum", "egyeni_mkkp", "mellar_tamas", "kesz_zoltan", "egyeni_egyeb", "orsz_dk", "orsz_egyutt", "orsz_fidesz", "orsz_jobbik", "orsz_lmp", "orsz_mszp", "orsz_momentum","orsz_mkkp", "orsz_egyeb")]

write.csv(data_clean,'2018_egyeni_listas_clean.csv')

#UJ VALTOZOK - partok szazalekos egyeni es orszagos eredmenye egymashoz viszonyitva 

data_clean <- data_clean[, egyeni_fidesz_pc := egyeni_fidesz / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

data_clean <- data_clean[, egyeni_jobbik_pc := egyeni_jobbik / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

data_clean <- data_clean[, egyeni_mszp_pc := egyeni_mszp / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

data_clean <- data_clean[, egyeni_dk_pc := egyeni_dk / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

data_clean <- data_clean[, egyeni_lmp_pc := egyeni_lmp / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

data_clean <- data_clean[, egyeni_momentum_pc := egyeni_momentum / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

data_clean <- data_clean[, egyeni_egyutt_pc := egyeni_egyutt / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

data_clean <- data_clean[, egyeni_mkkp_pc := egyeni_mkkp / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

data_clean <- data_clean[, egyeni_egyeb_pc := egyeni_egyeb / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

data_clean <- data_clean[, egyeni_mellar_pc := mellar_tamas / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

data_clean <- data_clean[, kesz_zoltan_pc := kesz_zoltan / (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)] 

#add mellar es kesz zoltan


data_clean <- data_clean[, orsz_fidesz_pc := orsz_fidesz / (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)] 

data_clean <- data_clean[, orsz_jobbik_pc := orsz_jobbik / (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)] 

data_clean <- data_clean[, orsz_mszp_pc := orsz_mszp / (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)] 

data_clean <- data_clean[, orsz_dk_pc := orsz_dk / (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)] 

data_clean <- data_clean[, orsz_lmp_pc := orsz_lmp / (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)] 

data_clean <- data_clean[, orsz_momentum_pc := orsz_momentum / (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)] 

data_clean <- data_clean[, orsz_egyutt_pc := orsz_egyutt / (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)] 

data_clean <- data_clean[, orsz_mkkp_pc := orsz_mkkp / (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)] 

data_clean <- data_clean[, orsz_egyeb_pc := orsz_egyeb / (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)] 



data_clean <- data_clean[,elteres_fidesz := egyeni_fidesz - orsz_fidesz]

data_clean <- data_clean[,elteres_lmp :=  egyeni_lmp - orsz_lmp]

data_clean <- data_clean[,elteres_mszp := egyeni_mszp - orsz_mszp]

data_clean <- data_clean[,elteres_mszp_dk := (egyeni_mszp + egyeni_dk) - (orsz_mszp + orsz_dk)]

data_clean <- data_clean[,elteres_dk := egyeni_dk - orsz_dk]

data_clean <- data_clean[,elteres_jobbik := egyeni_jobbik - orsz_jobbik]

data_clean <- data_clean[,elteres_momentum := egyeni_momentum - orsz_momentum]

data_clean <- data_clean[,elteres_egyutt := egyeni_egyutt - orsz_egyutt]

data_clean <- data_clean[,elteres_mkkp := egyeni_mkkp - orsz_mkkp]

data_clean <- data_clean[,elteres_egyeb := egyeni_egyeb - orsz_egyeb]

data_clean <- data_clean[,balodal_jobbik_kihivo := orsz_mszp + orsz_dk - orsz_jobbik]

data_clean <- 

#szavazokorok szama 





#TEST egyeni es listas szavazat partonkent 

fidesz_egyeni_ossz <- sum(data$egyeni_fidesz)
fidesz_egyeni_ossz

fidesz_orszagos_ossz <- sum(data$orsz_fidesz)
fidesz_orszagos_ossz

mszp_egyeni_ossz <- sum(data$egyeni_mszp)
mszp_egyeni_ossz

mszp_orszagos_ossz <- sum(data$orsz_mszp)
mszp_orszagos_ossz

jobbik_egyeni_ossz <- sum(data$egyeni_jobbik)
jobbik_egyeni_ossz

jobbik_orszagos_ossz <- sum(data$orsz_jobbik)
jobbik_orszagos_ossz

lmp_orszagos_ossz <- sum(data_clean$orsz_lmp)
lmp_orszagos_ossz

lmp_egyeni_ossz <- sum(data_clean$egyeni_lmp)
lmp_egyeni_ossz

###SZAVAZOKORI ES TELEPULESADATOK

data_clean$oevk_id <- paste(data_clean$megye, data_clean$oevk)

by_szavazokor_2018 <- data_clean[, , by = list(megyeid, megye, telepid, szavazokor, telep)]

write.csv(by_szavazokor_2018,'2018_by_szavazokor.csv', fileEncoding = "UTF-8")

#OEVK szintu eredmenyek (8 fo part)

egyeni_lista_by_oevk_2018 <- by_szavazokor_2018[, list(
                       #     megyeid = megyeid,
                        #    megye = megye,
                        #    telepid = telepid,
                        #    telep = telep,
                            szavazok = sum(nevjegyz + nevjegyz_atjel),
                            reszvetel = sum(megjel + megjel_atjel) / sum(nevjegyz + nevjegyz_atjel),
                            ossz_szavazo = sum(megjel + megjel_atjel),
                            
                           egyeni_fidesz = sum(egyeni_fidesz),
                           egyeni_lmp = sum(egyeni_lmp),
                           egyeni_mszp = sum(egyeni_mszp),
                           egyeni_dk = sum(egyeni_dk),
                           egyeni_mszp_dk = sum(egyeni_mszp) + sum(egyeni_dk),
                           egyeni_jobbik = sum(egyeni_jobbik),
                           egyeni_momentum = sum(egyeni_momentum),
                           egyeni_egyutt = sum(egyeni_egyutt),
                           egyeni_mkkp = sum(egyeni_mkkp),
                           egyeni_mellar = sum(mellar_tamas),
                           egyeni_kesz = sum(kesz_zoltan),
                           egyeni_egyeb = sum(egyeni_egyeb),
                           
                           egyeni_fidesz_pc = sum(egyeni_fidesz) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           egyeni_lmp_pc = sum(egyeni_lmp) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           egyeni_mszp_pc = sum(egyeni_mszp) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           egyeni_dk_pc = sum(egyeni_dk) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           egyeni_jobbik_pc = sum(egyeni_jobbik) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           egyeni_momentum_pc = sum(egyeni_momentum) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           egyeni_egyutt_pc = sum(egyeni_egyutt) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           egyeni_mkkp_pc = sum(egyeni_mkkp) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           egyeni_mellar_pc = sum(mellar_tamas) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           egyeni_kesz_pc = sum(kesz_zoltan) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           egyeni_egyeb_pc = sum(egyeni_egyeb) / (sum(kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           
                           orsz_fidesz = sum(orsz_fidesz),
                           orsz_lmp = sum(orsz_lmp),
                           orsz_mszp = sum(orsz_mszp),
                           orsz_dk = sum(orsz_dk),
                           orsz_mszp_dk = sum(orsz_mszp) + sum(orsz_dk),
                           orsz_jobbik = sum(orsz_jobbik),
                           orsz_momentum = sum(orsz_momentum),
                           orsz_egyutt = sum(orsz_egyutt),
                           orsz_mkkp = sum(orsz_mkkp),
                           orsz_egyeb = sum(orsz_egyeb),
                           
                           orsz_fidesz_pc = sum(orsz_fidesz) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_lmp_pc = sum(orsz_lmp) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_mszp_pc = sum(orsz_mszp) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_dk_pc = sum(orsz_dk) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_jobbik_pc = sum(orsz_jobbik) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_momentum_pc = sum(orsz_momentum) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_egyutt_pc = sum(orsz_egyutt) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_mkkp_pc = sum(orsz_mkkp) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_egyeb_pc = sum(orsz_egyeb) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           
                           
                           elteres_fidesz = sum(egyeni_fidesz) - sum(orsz_fidesz),
                           elteres_lmp = sum(egyeni_lmp) - sum(orsz_lmp),
                           elteres_mszp = sum(egyeni_mszp) - sum(orsz_mszp),
                           elteres_mszp_dk = (sum(egyeni_mszp) + sum(egyeni_dk)) - (sum(orsz_mszp) + sum(orsz_dk)),
                           elteres_dk = sum(egyeni_dk) - sum(orsz_dk),
                           elteres_jobbik = sum(egyeni_jobbik) - sum(orsz_jobbik),
                           elteres_momentum = sum(egyeni_momentum) - sum(orsz_momentum),
                           elteres_egyutt = sum(egyeni_egyutt) - sum(orsz_egyutt),
                           elteres_mkkp = sum(egyeni_mkkp) - sum(orsz_mkkp),
                           elteres_egyeb = sum(egyeni_egyeb) - sum(orsz_egyeb),
                           
                           balodal_jobbik_kihivo = sum(orsz_mszp) + sum(orsz_dk) - sum(orsz_jobbik)),

                           by = oevk_id][order(oevk_id)] 

write.csv(egyeni_lista_by_oevk_2018,'2018_by_oevk.csv', fileEncoding = "UTF-8")
