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
setwd('C:/Users/adasan01/OneDrive - ARM/Documents/GitHub/106/capstone_adat')

# LOAD  DATA
data <- read.csv("2018_egyeni_listas_29May.csv")
data[is.na(data)] <- 0



#kamupartok - nem 8 fo part - egyesitese es szurese
egyeni_egyeb <- subset(data, select = c(megyeid, megye, oevk, telepid, szavazokor, egyeni_na, egyeni_haza_partja, egyeni_aqp, egyeni_civil_mozgalom, egyeni_csp, egyeni_demokrata_part, egyeni_dmp, egyeni_ecdp, egyeni_emmo, egyeni_ep, egyeni_erp, egyeni_eu_alternativa, egyeni_eu_rom, egyeni_fitip, egyeni_fkgp, egyeni_fuggetlen, egyeni_hajra_magyarorszag, egyeni_ham, egyeni_haza_mindenkie, egyeni_hhp, egyeni_ima, egyeni_iranytu, egyeni_jmp, egyeni_jo_ut_mpp, egyeni_kedn, egyeni_kpp, egyeni_kossz, egyeni_kozos_nevezo, egyeni_lendulettel, egyeni_mcp, egyeni_medete_part, egyeni_minokp, egyeni_miszep, egyeni_miep, egyeni_mmm, egyeni_moma, egyeni_munkaspart, egyeni_mvmp, egyeni_neem, egyeni_nemzet_beke, egyeni_nemzeti_zoldek, egyeni_nop, egyeni_np, egyeni_ocp, egyeni_op, egyeni_opre_roma, egyeni_rend_part, egyeni_sem, egyeni_szem_part, egyeni_szp, egyeni_tamp, egyeni_ebmp, egyeni_ertunk_ertetek, egyeni_umf))
orsz_egyeb <- subset(data, select = c(megyeid, megye, oevk, telepid, szavazokor, orsz_csaladok_partja, orsz_eu_roma_keresztenyek, orsz_iranytu, orsz_kossz, orsz_kozos_nevezo, orsz_miep, orsz_magyar_munkaspart, orsz_ciganypart, orsz_mo_dolgozo_tanulo_emberek, orsz_net_part, orsz_rend_elszamoltatas, orsz_sportos_egeszseges_mo, orsz_szegeny_emberek_mo, orsz_tenni_akaras_mozgalom, orsz_osszefogas_part))
nemzetisegi_lista <- subset(data, select = c(megyeid, megye, oevk, telepid, szavazokor, nemzetisegi_bolgar, nemzetisegi_gorog, nemzetisegi_horvat, nemzetisegi_lengyel, nemzetisegi_nemet, nemzetisegi_roma, nemzetisegi_roman, nemzetisegi_ruszin, nemzetisegi_szerb, nemzetisegi_szlovak, nemzetisegi_szloven, nemzetisegi_ukran, nemzetisegi_ormeny))

egyeni_egyeb$egyeni_egyeb <- rowSums( egyeni_egyeb[, 6:59] )

orsz_egyeb$orsz_egyeb <- rowSums( orsz_egyeb[, 6:20] )

nemzetisegi_lista$nemzetisegi_osszes <- rowSums( nemzetisegi_lista[, 6:18] ) - rowSums( nemzetisegi_lista[, 10:11] )

egyeni_egyeb2 <- subset(egyeni_egyeb, select = c(megyeid, megye, oevk, telepid, szavazokor, egyeni_egyeb))

orsz_egyeb2 <- subset(orsz_egyeb, select = c(megyeid, megye, oevk, telepid, szavazokor, orsz_egyeb))

nemzetisegi_2 <- subset(nemzetisegi_lista, select = c(megyeid, megye, oevk, telepid, szavazokor, nemzetisegi_nemet, nemzetisegi_osszes))

data2 <- merge(data, egyeni_egyeb2, by=c("megyeid", "megye", "oevk", "telepid", "szavazokor"))

data3 <- merge(data2, orsz_egyeb2, by=c("megyeid", "megye", "oevk", "telepid", "szavazokor"))

data4 <- merge(data3, nemzetisegi_2, by=c("megyeid", "megye", "oevk", "telepid", "szavazokor"))

#leszurni a fobb partokat + egyeb partokat + nemzetisegieket

data4 <- as.data.table(data4)

data_clean <- data4[, c("megyeid", "megye", "oevk", "telepid", "szavazokor", "telep", "nevjegyz", "megjel", "nevjegyz_atjel", "megjel_atjel", "boritek_atjel", "pecset_nelkul", "pecsettel", "tobblet_hiany", "ervenytelen", "ervenyes", "egyeni_dk", "egyeni_egyutt", "egyeni_fidesz", "egyeni_jobbik", "egyeni_lmp", "egyeni_mszp", "egyeni_momentum", "egyeni_mkkp", "mellar_tamas", "kesz_zoltan", "egyeni_egyeb", "orsz_dk", "orsz_egyutt", "orsz_fidesz", "orsz_jobbik", "orsz_lmp", "orsz_mszp", "orsz_momentum","orsz_mkkp", "orsz_egyeb", "nemzetisegi_nemet.y", "nemzetisegi_osszes")]


data_clean <- rename(data_clean, nemzetisegi_nemet = nemzetisegi_nemet.y)

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



data_clean <- data_clean[,elteres_fidesz := egyeni_fidesz - (orsz_fidesz + nemzetisegi_nemet)]

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

data_clean <- data_clean[, listas_hianyzo := (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb + nemzetisegi_osszes + nemzetisegi_nemet) - (kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)]

data_clean <- data_clean[, osszes_listas := (orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)]

data_clean <- data_clean[, listas_elteres := ((orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb + nemzetisegi_osszes + nemzetisegi_nemet)) - (megjel + megjel_atjel)]

data_clean <- data_clean[, egyeni_elteres := ((kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)) - (megjel + megjel_atjel)]



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

                            ossz_szavazo = sum(megjel + megjel_atjel),
                            
                            tobblet_hiany = sum(tobblet_hiany),
                            ervenytelen = sum(ervenytelen),
                            ervenyes = sum(ervenyes),
                            ervenytelen_szazalek = sum(ervenytelen) / (sum(ervenytelen) + sum(ervenyes)),
                            
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
                           
                           nemzetisegi_osszes = sum(nemzetisegi_osszes),
                           nemzetisegi_nemet = sum(nemzetisegi_nemet),
                           
                           orsz_fidesz_pc = sum(orsz_fidesz) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_lmp_pc = sum(orsz_lmp) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_mszp_pc = sum(orsz_mszp) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_dk_pc = sum(orsz_dk) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_jobbik_pc = sum(orsz_jobbik) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_momentum_pc = sum(orsz_momentum) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_egyutt_pc = sum(orsz_egyutt) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_mkkp_pc = sum(orsz_mkkp) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           orsz_egyeb_pc = sum(orsz_egyeb) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
                           
                           
                           elteres_fidesz = sum(egyeni_fidesz) - (sum(orsz_fidesz)),
                           elteres_lmp = sum(egyeni_lmp) - sum(orsz_lmp),
                           elteres_mszp = sum(egyeni_mszp) - sum(orsz_mszp),
                           elteres_mszp_dk = (sum(egyeni_mszp) + sum(egyeni_dk)) - (sum(orsz_mszp) + sum(orsz_dk)),
                           elteres_dk = sum(egyeni_dk) - sum(orsz_dk),
                           elteres_jobbik = sum(egyeni_jobbik) - sum(orsz_jobbik),
                           elteres_momentum = sum(egyeni_momentum) - sum(orsz_momentum),
                           elteres_egyutt = sum(egyeni_egyutt) - sum(orsz_egyutt),
                           elteres_mkkp = sum(egyeni_mkkp) - sum(orsz_mkkp),
                           elteres_egyeb = sum(egyeni_egyeb) - sum(orsz_egyeb),
                           elteres_nemzetisegi_osszes = sum(nemzetisegi_osszes) * -1,
                           elteres_nemzetisegi_nemet = sum(nemzetisegi_nemet) * -1,
                           
                           listas_hianyzo = sum((orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb + nemzetisegi_osszes + nemzetisegi_nemet)) - sum((kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
                           
                           balodal_jobbik_kihivo = sum(orsz_mszp) + sum(orsz_dk) - sum(orsz_jobbik)),

                           by = oevk_id][order(oevk_id)] 

write.csv(egyeni_lista_by_oevk_2018,'2018_by_oevk.csv', fileEncoding = "UTF-8")

### TELEPULES szintű adatok

egyeni_lista_by_telep_2018 <- by_szavazokor_2018[, list(
  megyeid = megyeid,
  megye = megye,
  telepid = telepid,
  telep = telep,
  szavazok = sum(nevjegyz + nevjegyz_atjel),
  
  ossz_szavazo = sum(megjel + megjel_atjel),
  
  tobblet_hiany = sum(tobblet_hiany),
  ervenytelen = sum(ervenytelen),
  ervenyes = sum(ervenyes),
  ervenytelen_szazalek = sum(ervenytelen) / (sum(ervenytelen) + sum(ervenyes)),
  
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
  
  nemzetisegi_osszes = sum(nemzetisegi_osszes),
  nemzetisegi_nemet = sum(nemzetisegi_nemet),
  
  orsz_fidesz_pc = sum(orsz_fidesz) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
  orsz_lmp_pc = sum(orsz_lmp) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
  orsz_mszp_pc = sum(orsz_mszp) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
  orsz_dk_pc = sum(orsz_dk) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
  orsz_jobbik_pc = sum(orsz_jobbik) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
  orsz_momentum_pc = sum(orsz_momentum) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
  orsz_egyutt_pc = sum(orsz_egyutt) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
  orsz_mkkp_pc = sum(orsz_mkkp) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
  orsz_egyeb_pc = sum(orsz_egyeb) / (sum(orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb)),
  
  
  elteres_fidesz = sum(egyeni_fidesz) - (sum(orsz_fidesz)),
  elteres_lmp = sum(egyeni_lmp) - sum(orsz_lmp),
  elteres_mszp = sum(egyeni_mszp) - sum(orsz_mszp),
  elteres_mszp_dk = (sum(egyeni_mszp) + sum(egyeni_dk)) - (sum(orsz_mszp) + sum(orsz_dk)),
  elteres_dk = sum(egyeni_dk) - sum(orsz_dk),
  elteres_jobbik = sum(egyeni_jobbik) - sum(orsz_jobbik),
  elteres_momentum = sum(egyeni_momentum) - sum(orsz_momentum),
  elteres_egyutt = sum(egyeni_egyutt) - sum(orsz_egyutt),
  elteres_mkkp = sum(egyeni_mkkp) - sum(orsz_mkkp),
  elteres_egyeb = sum(egyeni_egyeb) - sum(orsz_egyeb),
  elteres_nemzetisegi_osszes = sum(nemzetisegi_osszes) * -1,
  elteres_nemzetisegi_nemet = sum(nemzetisegi_nemet) * -1,
  
  listas_hianyzo = sum((orsz_fidesz + orsz_jobbik + orsz_mszp + orsz_dk + orsz_lmp + orsz_momentum + orsz_egyutt + orsz_mkkp + orsz_egyeb + nemzetisegi_osszes + nemzetisegi_nemet)) - sum((kesz_zoltan + mellar_tamas + egyeni_fidesz + egyeni_jobbik + egyeni_mszp + egyeni_dk + egyeni_lmp + egyeni_momentum + egyeni_egyutt + egyeni_mkkp + egyeni_egyeb)),
  
  balodal_jobbik_kihivo = sum(orsz_mszp) + sum(orsz_dk) - sum(orsz_jobbik)),
  
  by = telep][order(orsz_fidesz_pc)] 

egyeni_lista_by_telep_2018 <-unique(egyeni_lista_by_telep_2018)

write.csv(egyeni_lista_by_telep_2018,'2018_by_telep.csv', fileEncoding = "UTF-8")

### OEVK profilok


data_OEVK_profil <- read.csv("2018_by_oevk_OEVK_profiles.csv")


freq(data_OEVK_profil$OEVK_profil)

## ANALYSIS - Fidesz and other party votes

fidesz_vote_by_OEVK_profile <- lm(orsz_fidesz_pc ~ OEVK_profil, data_OEVK_profil)
coeftest(fidesz_vote_by_OEVK_profile)

mszp_vote_by_OEVK_profile <- lm(orsz_mszp_pc ~ OEVK_profil, data_OEVK_profil)
coeftest(mszp_vote_by_OEVK_profile)

dk_vote_by_OEVK_profile <- lm(orsz_dk_pc ~ OEVK_profil, data_OEVK_profil)
coeftest(dk_vote_by_OEVK_profile)

jobbik_vote_by_OEVK_profile <- lm(orsz_jobbik_pc ~ OEVK_profil, data_OEVK_profil)
coeftest(jobbik_vote_by_OEVK_profile)

lmp_vote_by_OEVK_profile <- lm(orsz_lmp_pc ~ OEVK_profil, data_OEVK_profil)
coeftest(lmp_vote_by_OEVK_profile)

momentum_vote_by_OEVK_profile <- lm(orsz_momentum_pc ~ OEVK_profil, data_OEVK_profil)
coeftest(momentum_vote_by_OEVK_profile)


ggplot(data_OEVK_profil, aes(OEVK_profil, orsz_fidesz_pc)) + geom_point(color ="red") +
  geom_smooth(method = "loess") + ggtitle("Fidesz vote share % by OEVK profile") + xlab("OEVK profile") + ylab("Fidesz vote share") +
  theme_minimal()
ggsave(("fidesz_vote_oevk_profile.png"), width = 30, height = 18, units = c("cm"))

ggplot(data_OEVK_profil, aes(szavazok, orsz_fidesz_pc)) + geom_point(color ="red") +
  geom_smooth(method = "lm") + ggtitle("Fidesz vote share % by OEVK size (number of voters)") + xlab("Number of voters") + ylab("Fidesz vote share") +
  theme_minimal()
ggsave(("fidesz_vote_oevk_size.png"), width = 30, height = 18, units = c("cm"))

ggplot(data_OEVK_profil, aes(OEVK_profil, orsz_jobbik_pc)) + geom_point(color ="red") +
  geom_smooth(method = "loess") + ggtitle("Jobbik vote share % by OEVK profile") + xlab("OEVK profile") + ylab("Jobbik vote share") +
  theme_minimal()
ggsave(("jobbik_vote_oevk_profile.png"), width = 30, height = 18, units = c("cm"))


ggplot(data_OEVK_profil, aes(OEVK_profil, orsz_mszp_pc)) + geom_point(color ="red") +
  geom_smooth(method = "loess") + ggtitle("MSZP vote share % by OEVK profile") + xlab("OEVK profile") + ylab("MSZP vote share") +
  theme_minimal()
ggsave(("mszp_vote_oevk_profile.png"), width = 30, height = 18, units = c("cm"))


ggplot(data_OEVK_profil, aes(OEVK_profil, orsz_lmp_pc)) + geom_point(color ="red") +
  geom_smooth(method = "loess") + ggtitle("LMP vote share % by OEVK profile") + xlab("OEVK profile") + ylab("LMP vote share") +
  theme_minimal()
ggsave(("lmp_vote_oevk_profile.png"), width = 30, height = 18, units = c("cm"))


### TELEPULES LELEKSZAM

egyeni_lista_by_telep_2018 <- egyeni_lista_by_telep_2018[,telepules_tipus := orsz_mszp + orsz_dk - orsz_jobbik]

egyeni_lista_by_telep_2018$telep_tipus <- cut(egyeni_lista_by_telep_2018$szavazok, c(0, 500, 1000, 2000, 5000, 10000, 30000, 60000, 100000, Inf), 
                                              labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8"))

egyeni_lista_by_telep_2018$telep_tipus <- as.integer(egyeni_lista_by_telep_2018$telep_tipus)

freq(egyeni_lista_by_telep_2018$telep_tipus)

szavazok_by_telep_tipus <- egyeni_lista_by_telep_2018[, list(ossz_szavazo = sum(ossz_szavazo),
                                                             ossz_telepules = sum(ossz_szavazo / ossz_szavazo)),
                                                      by = telep_tipus][order(telep_tipus)] 

hist(szavazok_by_telep_tipus$ossz_szavazo)

ggplot(egyeni_lista_by_telep_2018, aes(telep_tipus, orsz_fidesz_pc)) + geom_point(color ="red") +
  geom_smooth(method = "lm") + ggtitle("Fidesz vote share % by town size") +
  theme_minimal()
ggsave(("fidesz_vote_town_size.png"), width = 30, height = 18, units = c("cm"))

ggplot(egyeni_lista_by_telep_2018, aes(telep_tipus, orsz_mszp_pc)) + geom_point(color ="red") +
  geom_smooth(method = "lm") + ggtitle("MSZP vote share % by town size") +
  theme_minimal()
ggsave(("mszp_vote_town_size.png"), width = 30, height = 18, units = c("cm"))

ggplot(egyeni_lista_by_telep_2018, aes(telep_tipus, orsz_jobbik_pc)) + geom_point(color ="red") +
  geom_smooth(method = "loess") + ggtitle("Jobbik vote share % by town size") +
  theme_minimal()
ggsave(("jobbik_vote_town_size.png"), width = 30, height = 18, units = c("cm"))

ggplot(egyeni_lista_by_telep_2018, aes(telep_tipus, orsz_lmp_pc)) + geom_point(color ="red") +
  geom_smooth(method = "lm") + ggtitle("LMP vote share % by town size") +
  theme_minimal()
ggsave(("lmp_vote_town_size.png"), width = 30, height = 18, units = c("cm"))



### OEVK predictions

data_pred_egyeni <- read.csv("2018_egyeni_becsles_elteres.csv", fileEncoding = "UTF-8")

OEVK_pred_elemzes <- merge(egyeni_lista_by_oevk_2018, data_pred_egyeni, by ="oevk_id")


ggplot(OEVK_pred_elemzes, aes(Fidesz_egyeni_becsles, egyeni_fidesz_pc)) + geom_point(color ="red") +
  geom_text(aes(label=oevk_id),
            size = 2) + geom_smooth(method = "loess") + ggtitle("Fidesz predicted and actual vote share % by OEVK") +
  xlim(0.25, 0.65) +
  ylim(0.25, 0.65) +
  theme_minimal()
ggsave(("fidesz_pred_actual.png"), width = 25, height = 25, units = c("cm"))

ggplot(OEVK_pred_elemzes, aes(egyeni_fidesz_pc, Fidesz_pred_elteres)) + geom_point(color ="red") +
  geom_text(aes(label=oevk_id),
            size = 2) + geom_smooth(method = "loess") + ggtitle("Fidesz actual vote share and prediction error by OEVK") +
  xlab("Fidesz vote share") + ylab("Prediction error") +
  xlim(0.25, 0.65) +
theme_minimal()
ggsave(("fidesz_deviation.png"), width = 25, height = 25, units = c("cm"))


ggplot(OEVK_pred_elemzes, aes(Jobbik_egyeni_becsles, egyeni_jobbik_pc)) + geom_point(color ="red") +
  geom_text(aes(label=oevk_id),
            size = 2) + geom_smooth(method = "loess") + ggtitle("Jobbik predicted and actual vote share % by OEVK") +
  xlim(0.00, 0.50) +
  ylim(0.00, 0.50) +
  theme_minimal()
ggsave(("jobbik_pred_actual.png"), width = 25, height = 25, units = c("cm"))


ggplot(OEVK_pred_elemzes, aes(egyeni_jobbik_pc, Jobbik_pred_elteres)) + geom_point(color ="red") +
  geom_text(aes(label=oevk_id),
            size = 2) + geom_smooth(method = "loess") + ggtitle("Jobbik actual vote share and prediction error by OEVK") + 
  xlab("Jobbik vote share") + ylab("Prediction error") +
  xlim(0.05, 0.45) +
theme_minimal()
ggsave(("jobbik_deviation.png"), width = 25, height = 25, units = c("cm"))


ggplot(OEVK_pred_elemzes, aes(Baloldal_egyeni_becsles, egyeni_dk_pc)) + geom_point(color ="red") +
  geom_text(aes(label=oevk_id),
            size = 2) + geom_smooth(method = "loess") + ggtitle("DK predicted and actual vote share % by OEVK") +
  xlim(0.001, 0.55) +
  ylim(0.001, 0.55) +
theme_minimal()
ggsave(("dk_pred_actual.png"), width = 25, height = 25, units = c("cm"))

ggplot(OEVK_pred_elemzes, aes(egyeni_dk_pc, Baloldal_pred_elteres)) + geom_point(color ="red") +
  geom_text(aes(label=oevk_id),
            size = 2) + geom_smooth(method = "loess") + ggtitle("DK actual vote share and prediction error by OEVK") +
  xlab("DK vote share") + ylab("Prediction error") +
  xlim(0.001, 0.55) +
  theme_minimal()
  ggsave(("dk_deviation.png"), width = 25, height = 25, units = c("cm"))
  
  ggplot(OEVK_pred_elemzes, aes(Baloldal_egyeni_becsles, egyeni_mszp_pc)) + geom_point(color ="red") +
    geom_text(aes(label=oevk_id),
              size = 2) + geom_smooth(method = "loess") + ggtitle("MSZP predicted and actual vote share % by OEVK") +
    xlim(0.001, 0.60) +
    ylim(0.001, 0.60) +
  theme_minimal()
  ggsave(("mszp_pred_actual.png"), width = 25, height = 25, units = c("cm"))
  
  ggplot(OEVK_pred_elemzes, aes(egyeni_mszp_pc, Baloldal_pred_elteres)) + geom_point(color ="red") +
    geom_text(aes(label=oevk_id),
              size = 2) + geom_smooth(method = "loess") + ggtitle("MSZP actual vote share and prediction error by OEVK") +
    xlab("MSZP vote share") + ylab("Prediction error") +
    xlim(0.001, 0.60) +
  theme_minimal()
  ggsave(("mszp_deviation.png"), width = 25, height = 25, units = c("cm"))
  

  
  
  
  
  
  #TSTAR telepules adatok - (Budapest is merged)
  
  
  data_telepulesadatok_t_star <-read.csv("Telepulesadatok_t_star.csv")
  colnames(data_telepulesadatok_t_star)[1] <- "telep_nev"
  
  egyeni_lista_2018_telep_clean <- read.csv("2018_by_telep_clean.csv", fileEncoding = "UTF-8")
  
  oevk_2014_telep_clean <- read.csv("oevk2014_telepules_clean.csv", fileEncoding = "UTF-8") 
  
  
  t_star_telepules_2018 <- merge(data_telepulesadatok_t_star, egyeni_lista_2018_telep_clean, by.x = "telep_nev")
  
  t_star_telepules_2014_2018 <- merge(oevk_2014_telep_clean, t_star_telepules_2018, by = "telep_nev")
  
  t_star_telepules_2014_2018$megye_ido_km <- as.numeric(t_star_telepules_2014_2018$megye_ido_km)
  t_star_telepules_2014_2018$megyeszekhely_km <- as.numeric(t_star_telepules_2014_2018$megyeszekhely_km)
  t_star_telepules_2014_2018$megyeszekhely_min <- as.numeric(t_star_telepules_2014_2018$megyeszekhely_min)
  t_star_telepules_2014_2018$altisk <- as.numeric(t_star_telepules_2014_2018$altisk)
  
  t_star_telepules_2014_2018 <- as.data.table(t_star_telepules_2014_2018)
  
  
  write.csv(t_star_telepules_2014_2018, '2014_2018_tstar_by_telep.csv')
  
  
  ###t_star_telepules_2018 is all basic t_star data with all towns (Budapest merged)
  
  
  #### regression example - GLM
  
  ###applying quintiles to continuous variables to create categories for easier analysis
  
  t_star_telepules_2014_2018[is.na(t_star_telepules_2014_2018)] <- 0
  
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, reszvetel_q <- as.integer(cut(reszvetel, quantile(reszvetel, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, kabeltv_arany_q <- as.integer(cut(kabeltv_arany, quantile(kabeltv_arany, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, budapest_ido_km_q <- as.integer(cut(budapest_ido_km, quantile(budapest_ido_km, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, megye_ido_km_q <- as.integer(cut(megye_ido_km, quantile(megye_ido_km, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, megyeszekhely_km_q <- as.integer(cut(megyeszekhely_km, quantile(megyeszekhely_km, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, megyeszekhely_min_q <- as.integer(cut(megyeszekhely_min, quantile(megyeszekhely_min, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, budapest_min_q <- as.integer(cut(budapest_min, quantile(budapest_min, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, altisk_q <- as.integer(cut(altisk, quantile(altisk, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, jovedelem_fo_q <- as.integer(cut(jovedelem_fo, quantile(jovedelem_fo, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, ujszulott_rate_q <- as.integer(cut(ujszulott_rate, quantile(ujszulott_rate, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, idosek_rate_q <- as.integer(cut(idosek_rate, quantile(idosek_rate, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, nepesseg_q <- as.integer(cut(nepesseg, quantile(nepesseg, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, elvandorlas_q <- as.integer(cut(elvandorlas, quantile(elvandorlas, probs=0:10/10), include.lowest=TRUE)))
  t_star_telepules_2014_2018 <- within(t_star_telepules_2014_2018, odavanadorlas_q <- as.integer(cut(odavanadorlas, quantile(odavanadorlas, probs=0:10/10), include.lowest=TRUE)))
  

  
  regression_test_2014_2018_tstar_mini <- glm(egyeni_fidesz_pc.y ~ egyeni_fidesz_pc.x + reszvetel_q + 
                                               budapest_ido_km_q + megye_ido_km_q + megyeszekhely_km_q + megyeszekhely_min_q +
                                                budapest_min_q + altisk_q + jovedelem_fo_q + ujszulott_rate_q + 
                                                idosek_rate_q + nepesseg_q + elvandorlas_q + odavanadorlas_q, data=t_star_telepules_2014_2018, na.action = na.pass)
  summary(regression_test_2014_2018_tstar_mini)
  coeftest(regression_test_2014_2018_tstar_mini)
  
  ###Fidesz
  
  
  logitcoeffs_regression_fidesz_2014_2018_tstar_mini <- glm(egyeni_fidesz_pc.y ~ egyeni_fidesz_pc.x + reszvetel_q + 
                                                             budapest_ido_km_q + megye_ido_km_q + megyeszekhely_km_q + megyeszekhely_min_q +
                                                            budapest_min_q + altisk_q + jovedelem_fo_q + ujszulott_rate_q + 
                                                            idosek_rate_q + nepesseg_q + elvandorlas_q + odavanadorlas_q,  data=t_star_telepules_2014_2018, family='binomial'(link="logit"))
  
  summary(logitcoeffs_regression_fidesz_2014_2018_tstar_mini)
  
  
  t_star_telepules_2014_2018$pred_logit_fidesz <- predict.glm(logitcoeffs_regression_fidesz_2014_2018_tstar_mini, type="response")
  
  ggplot(data = t_star_telepules_2014_2018, aes(x=egyeni_fidesz_pc.y, y= pred_logit_fidesz)) + xlim(0.15, 1.00) + ylim(0.15, 1.00) +
    geom_text(aes(label=megyeid), size = 1, check_overlap = FALSE) +
    geom_smooth(method = "loess") +  ggtitle("Fidesz - Actual and predicted vote share in each town") + xlab("2018 Fidesz vote %") + ylab("GLM prediction") +
    geom_line(aes(x=pred_logit_fidesz, y=pred_logit_fidesz), color="green", size = 1) +
    theme_minimal() 
    ggsave(("fidesz_GLM_by_megye_id.png"), width = 25, height = 25, units = c("cm"))
    
    ggplot(data = t_star_telepules_2014_2018, aes(x=egyeni_fidesz_pc.y, y= pred_logit_fidesz)) + xlim(0.15, 1.00) + ylim(0.15, 1.00) +
      geom_text(aes(label=telep_nev), size = 1, check_overlap = FALSE) +
      geom_smooth(method = "loess") +  ggtitle("Fidesz - Actual and predicted vote share in each town") + xlab("2018 Fidesz vote %") + ylab("GLM prediction") +
      geom_line(aes(x=pred_logit_fidesz, y=pred_logit_fidesz), color="green", size = 1) +
    theme_minimal() 
    ggsave(("fidesz_GLM_by_town.png"), width = 25, height = 25, units = c("cm"))
    
### Jobbik
    
    logitcoeffs_regression_jobbik_2014_2018_tstar_mini <- glm(egyeni_jobbik_pc.y ~ egyeni_jobbik_pc.x + reszvetel_q + 
                                                                kabeltv_arany_q + budapest_ido_km_q + megye_ido_km_q + megyeszekhely_km_q + megyeszekhely_min_q +
                                                                budapest_min_q + altisk_q + jovedelem_fo_q + ujszulott_rate_q + 
                                                                idosek_rate_q + nepesseg_q + elvandorlas_q + odavanadorlas_q,  data=t_star_telepules_2014_2018, family='binomial'(link="logit"))
    
    summary(logitcoeffs_regression_jobbik_2014_2018_tstar_mini)
    
    
    t_star_telepules_2014_2018$pred_logit_jobbik <- predict.glm(logitcoeffs_regression_jobbik_2014_2018_tstar_mini, type="response")
    
    ggplot(data = t_star_telepules_2014_2018, aes(x=egyeni_jobbik_pc.y, y= pred_logit_jobbik)) + xlim(0.00, 0.65) + ylim(0.00, 0.65) +
      geom_text(aes(label=megyeid), size = 1, check_overlap = FALSE) +
      geom_smooth(method = "loess") +  ggtitle("Jobbik - Actual and predicted vote share in each town") + xlab("2018 Jobbik vote %") + ylab("GLM prediction") +
      geom_line(aes(x=pred_logit_jobbik, y=pred_logit_jobbik), color="green", size = 1)
    theme_minimal() 
    ggsave(("Jobbik_GLM_by_megye_id.png"), width = 25, height = 25, units = c("cm"))
    
    ggplot(data = t_star_telepules_2014_2018, aes(x=egyeni_jobbik_pc.y, y= pred_logit_jobbik)) + xlim(0.00, 0.65) + ylim(0.00, 0.65) +
      geom_text(aes(label=telep_nev), size = 1, check_overlap = FALSE) +
      geom_smooth(method = "loess") +  ggtitle("Jobbik - Actual and predicted vote share in each town") + xlab("2018 Jobbik vote %") + ylab("GLM prediction") +
      geom_line(aes(x=pred_logit_jobbik, y=pred_logit_jobbik), color="green", size = 1)
    theme_minimal() 
    ggsave(("Jobbik_GLM_by_town.png"), width = 25, height = 25, units = c("cm"))
 

  
    
  ###additional data tables t-star (unemployment rate, pensioners, people on benefits, etc)
    
    rm(list=ls())
    
    data_telepulesadatok_t_star <-read.csv("Telepulesadatok_t_star.csv")
    colnames(data_telepulesadatok_t_star)[1] <- "telep_nev"
    
    egyeni_lista_2018_telep_clean <- read.csv("2018_by_telep_clean.csv", fileEncoding = "UTF-8")
    
    oevk_2014_telep_clean <- read.csv("oevk2014_telepules_clean.csv", fileEncoding = "UTF-8") 
    
    
    t_star_telepules_2018 <- merge(data_telepulesadatok_t_star, egyeni_lista_2018_telep_clean, by.x = "telep_nev")
    
    t_star_telepules_2014_2018 <- merge(oevk_2014_telep_clean, t_star_telepules_2018, by = "telep_nev")
    
  
  data_allaskeresok <- read.csv("tstar/allaskeresok.csv", fileEncoding = "UTF-8")
  data_allaskeresok <- as.data.table(data_allaskeresok)
  data_allaskeresok <- data_allaskeresok[ev.3 == 2018]
    
  data_koraranyok <- read.csv("tstar/koraranyok.csv", fileEncoding = "UTF-8")
  data_koraranyok <- as.data.table(data_koraranyok)
  data_koraranyok <- data_koraranyok[ev.3 == 2018]
    
  data_munkaadok <- read.csv("tstar/munkaadok.csv", fileEncoding = "UTF-8")
  data_munkaadok <- as.data.table(data_munkaadok)
  data_munkaadok <- data_munkaadok[ev.3 == 2018]
    
  data_nyugdijasok <- read.csv("tstar/nyugdijasok.csv", fileEncoding = "UTF-8")
  data_nyugdijasok <- as.data.table(data_nyugdijasok)
  data_nyugdijasok <- data_nyugdijasok[ev.3 == 2018]

  data_tamogatottak <- read.csv("tstar/tamogatottak.csv", fileEncoding = "UTF-8")
  data_tamogatottak <- as.data.table(data_tamogatottak)
  data_tamogatottak <- data_tamogatottak[ev.3 == 2018]  
  
  
  colnames(t_star_telepules_2014_2018)[1] <- "telnev"
  
  merge_all_t_star1 <- merge(data_allaskeresok, t_star_telepules_2014_2018, by = "telnev")  
  merge_all_t_star2 <- merge(data_koraranyok, merge_all_t_star1, by = c("telnev", "ev.3"))
  merge_all_t_star3 <- merge(data_munkaadok, merge_all_t_star2, by = c("telnev", "ev.3"))
  merge_all_t_star4 <- merge(data_nyugdijasok, merge_all_t_star3, by = c("telnev", "ev.3"))
  
  merge_all_t_star4 <- subset(merge_all_t_star4, select = c(1:2, 4:5, 7:145))
  
  merge_all_t_star_all <-  merge(data_tamogatottak, merge_all_t_star4, by = c("telnev", "ev.3"))
  
  merge_all_t_star_all$megye_ido_km <- as.numeric(t_star_telepules_2014_2018$megye_ido_km)
  merge_all_t_star_all$megyeszekhely_km <- as.numeric(t_star_telepules_2014_2018$megyeszekhely_km)
  merge_all_t_star_all$megyeszekhely_min <- as.numeric(t_star_telepules_2014_2018$megyeszekhely_min)
  merge_all_t_star_all$altisk <- as.numeric(t_star_telepules_2014_2018$altisk)
  
  write.csv(merge_all_t_star_all, '2014_2018_tstar_all_by_telep.csv')
  
  
  merge_all_t_star_all[is.na(merge_all_t_star_all)] <- 0
  
  merge_all_t_star_all <- within(merge_all_t_star_all, reszvetel_q <- as.integer(cut(reszvetel, quantile(reszvetel, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, kabeltv_arany_q <- as.integer(cut(kabeltv_arany, quantile(kabeltv_arany, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, budapest_ido_km_q <- as.integer(cut(budapest_ido_km, quantile(budapest_ido_km, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, megye_ido_km_q <- as.integer(cut(megye_ido_km, quantile(megye_ido_km, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, megyeszekhely_km_q <- as.integer(cut(megyeszekhely_km, quantile(megyeszekhely_km, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, megyeszekhely_min_q <- as.integer(cut(megyeszekhely_min, quantile(megyeszekhely_min, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, budapest_min_q <- as.integer(cut(budapest_min, quantile(budapest_min, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, altisk_q <- as.integer(cut(altisk, quantile(altisk, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, jovedelem_fo_q <- as.integer(cut(jovedelem_fo, quantile(jovedelem_fo, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, ujszulott_rate_q <- as.integer(cut(ujszulott_rate, quantile(ujszulott_rate, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, idosek_rate_q <- as.integer(cut(idosek_rate, quantile(idosek_rate, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, nepesseg_q <- as.integer(cut(nepesseg, quantile(nepesseg, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, elvandorlas_q <- as.integer(cut(elvandorlas, quantile(elvandorlas, probs=0:10/10), include.lowest=TRUE)))
  merge_all_t_star_all <- within(merge_all_t_star_all, odavanadorlas_q <- as.integer(cut(odavanadorlas, quantile(odavanadorlas, probs=0:10/10), include.lowest=TRUE)))
  
  ###GLM with all T-star data
  
  ## Fidesz
  
  logitcoeffs_regression_fidesz_2014_2018_tstar_all <- glm(egyeni_fidesz_pc.y ~ egyeni_fidesz_pc.x + kozmunka_aranya + nyudijas_ffi_arany +
                                                             nyudijas_noi_arany + nok_30_39_aranya + nok_40_49_aranya + nok_65._aranya +
                                                             ffi_30_39_aranya + ffi_40_49_aranya + ffi_65._aranya + allaskeresok_aranya_8osztaly +
                                                             ervenytelen_szazalek + reszvetel_q + 
                                                             budapest_ido_km_q + megye_ido_km_q + megyeszekhely_km_q + megyeszekhely_min_q +
                                                             budapest_min_q + altisk_q + jovedelem_fo_q + ujszulott_rate_q + 
                                                             idosek_rate_q + nepesseg_q + elvandorlas_q + odavanadorlas_q
                                                          ,  data=merge_all_t_star_all, family='binomial'(link="logit"))
  
  
  summary(logitcoeffs_regression_fidesz_2014_2018_tstar_all)
  
  
  merge_all_t_star_all$pred_logit_fidesz <- predict.glm(logitcoeffs_regression_fidesz_2014_2018_tstar_all, type="response")
  
  ggplot(data = merge_all_t_star_all, aes(x=egyeni_fidesz_pc.y, y= pred_logit_fidesz)) + xlim(0.15, 1.00) + ylim(0.15, 1.00) +
    geom_text(aes(label=megyeid), size = 1, check_overlap = FALSE) +
    geom_smooth(method = "loess") +  ggtitle("Fidesz - Actual and predicted vote share in each town") + xlab("2018 Fidesz vote %") + ylab("GLM prediction") +
    geom_line(aes(x=pred_logit_fidesz, y=pred_logit_fidesz), color="green", size = 1) +
  theme_minimal() 
  ggsave(("fidesz_GLM_by_megye_id_2.png"), width = 25, height = 25, units = c("cm"))
  
  ggplot(data = merge_all_t_star_all, aes(x=egyeni_fidesz_pc.y, y= pred_logit_fidesz)) + xlim(0.15, 1.00) + ylim(0.15, 1.00) +
    geom_text(aes(label=telnev), size = 1, check_overlap = FALSE) +
    geom_smooth(method = "loess") +  ggtitle("Fidesz - Actual and predicted vote share in each town") + xlab("2018 Fidesz vote %") + ylab("GLM prediction") +
    geom_line(aes(x=pred_logit_fidesz, y=pred_logit_fidesz), color="green", size = 1) +
  theme_minimal() 
  ggsave(("fidesz_GLM_by_town_2.png"), width = 25, height = 25, units = c("cm"))
  
  
  
  ## Jobbik
  
  logitcoeffs_regression_jobbik_2014_2018_tstar_all <- glm(egyeni_jobbik_pc.y ~ egyeni_jobbik_pc.x + kozmunka_aranya + nyudijas_ffi_arany +
                                                             nyudijas_noi_arany + nok_30_39_aranya + nok_40_49_aranya + nok_65._aranya +
                                                             ffi_30_39_aranya + ffi_40_49_aranya + ffi_65._aranya + allaskeresok_aranya_8osztaly +
                                                             ervenytelen_szazalek + reszvetel_q + 
                                                             budapest_ido_km_q + megye_ido_km_q + megyeszekhely_km_q + megyeszekhely_min_q +
                                                             budapest_min_q + altisk_q + jovedelem_fo_q + ujszulott_rate_q + 
                                                             idosek_rate_q + nepesseg_q + elvandorlas_q + odavanadorlas_q
                                                           ,  data=merge_all_t_star_all, family='binomial'(link="logit"))
  
  
  summary(logitcoeffs_regression_jobbik_2014_2018_tstar_all)
  
  
  merge_all_t_star_all$pred_logit_jobbik <- predict.glm(logitcoeffs_regression_jobbik_2014_2018_tstar_all, type="response")
  
  ggplot(data = merge_all_t_star_all, aes(x=egyeni_jobbik_pc.y, y= pred_logit_jobbik)) + xlim(0.00, 0.65) + ylim(0.00, 0.65) +
    geom_text(aes(label=megyeid), size = 1, check_overlap = FALSE) +
    geom_smooth(method = "loess") +  ggtitle("Jobbik - Actual and predicted vote share in each town") + xlab("2018 Jobbik vote %") + ylab("GLM prediction") +
    geom_line(aes(x=pred_logit_jobbik, y=pred_logit_jobbik), color="green", size = 1) +
    theme_minimal() 
  ggsave(("Jobbik_GLM_by_megye_id_2.png"), width = 25, height = 25, units = c("cm"))
  
  ggplot(data = merge_all_t_star_all, aes(x=egyeni_jobbik_pc.y, y= pred_logit_jobbik)) + xlim(0.00, 0.65) + ylim(0.00, 0.65) +
    geom_text(aes(label=telnev), size = 1, check_overlap = FALSE) +
    geom_smooth(method = "loess") +  ggtitle("Jobbik - Actual and predicted vote share in each town") + xlab("2018 Jobbik vote %") + ylab("GLM prediction") +
    geom_line(aes(x=pred_logit_fidesz, y=pred_logit_fidesz), color="green", size = 1) +
    theme_minimal() 
  ggsave(("Jobbik_GLM_by_town_2.png"), width = 25, height = 25, units = c("cm"))
  
#### UNS predictions and deviation   
    
  rm(list=ls())
  
    ### TO BE CLEANED UP
 
  
  ###DATA PROCESSING from other file
  
  
    # LOAD  DATA

  
    data <- read.csv("vote_counts_precincts_2b.csv")
  
  data <- as.data.table(data)
  
  #kamupartok egyesitese es szurese
  kamu <- subset(data, select = c(id, mcp, haza_nem_elado, sms, fkgp, udp, sem, jesz, ump, munkaspart, szocdemek, kti, egyutt2014, zoldek, osszefogas))
  
  kamu$egyeb <- rowSums( kamu[,2:15] )
  
  kamu2 <- subset(kamu, select = c(id, egyeb))
  
  data2 <- merge(data, kamu2, by="id")
  
  #reszveteli adatok, 4 part listaja, egyeni, egyeb partok lista + megye, telepules, szavazokor kulon
  
  data_clean <- subset(data2, select = c(id, telepules_id, telep_nev, atjelentkezettek, szavazokor, oevk_id, szavazok, reszvetel, fidesz, lmp, kormanyvaltok, jobbik, egyeni_fidesz, egyeni_lmp, egyeni_kormanyvaltok, egyeni_jobbik, egyeb))
  
  
  
  #szavazasra jogosultak szama / szavazokor
  
  data_clean <- data_clean[, ossz_szavazo := round(szavazok / (reszvetel /100), digits = 0)] 
  
  #szavazokorok szama 
  
  data_szavazokorok_szama <- data_clean[, list(szavazokorok_szama = .N), by = oevk_id]
  
  data_clean <- merge(data_clean, data_szavazokorok_szama, by = "oevk_id")
  
  
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
  
  #oevk szintu, telepules szintu eredmenyek (4 fobb part)
  
  data_clean <- as.data.table(data_clean)
  
  by_szavazokor_2014 <- data_clean[, list(telep_nev, ossz_szavazo, reszvetel, szavazok, fidesz, egyeni_fidesz, lmp, egyeni_lmp, kormanyvaltok, egyeni_kormanyvaltok, jobbik, egyeni_jobbik, egyeb, szavazokorok_szama), 
                                   by = list(oevk_id, id)]
  
  by_telepules_2014 <- data_clean[, list(szavazok = sum(szavazok),
                                         reszvetel = sum(szavazok) / sum(ossz_szavazo),
                                         ossz_szavazo = sum(szavazok),
                                         egyeni_fidesz = sum(egyeni_fidesz),
                                         egyeni_lmp = sum(egyeni_lmp),
                                         egyeni_kormanyvaltok = sum(egyeni_kormanyvaltok),
                                         egyeni_jobbik = sum(egyeni_jobbik),
                                         egyeb = sum(egyeb),
                                         orszagos_fidesz = sum(fidesz),
                                         orszagos_fidesz_lmp = sum(lmp),
                                         orszagos_fidesz_kormanyvaltok = sum(kormanyvaltok),
                                         orszagos_fidesz_jobbik = sum(jobbik),
                                         egyeni_fidesz_pc = sum(egyeni_fidesz) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                         egyeni_lmp_pc = sum(egyeni_lmp) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                         egyeni_kormanyvaltok_pc = sum(egyeni_kormanyvaltok) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                         egyeni_jobbik_pc = sum(egyeni_jobbik) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                         orszagos_fidesz_pc = sum(fidesz) / sum(fidesz + lmp + kormanyvaltok + jobbik + egyeb),
                                         orszagos_lmp_pc = sum(lmp) / sum(fidesz + lmp + kormanyvaltok + jobbik + egyeb),
                                         orszagos_kormanyvaltok_pc = sum(kormanyvaltok) / sum(fidesz + lmp + kormanyvaltok + jobbik + egyeb),
                                         orszagos_jobbik_pc = sum(jobbik) / sum(fidesz + lmp + kormanyvaltok + jobbik + egyeb)),
                                  by = telep_nev]
  
  write.csv(by_telepules_2014, 'oevk2014_telepules.csv')
  
  by_oevk_2014 <- by_szavazokor_2014[, list(szavazok = sum(szavazok),
                                            reszvetel = sum(szavazok) / sum(ossz_szavazo),
                                            ossz_szavazo = sum(szavazok),
                                            egyeni_fidesz = sum(egyeni_fidesz),
                                            egyeni_lmp = sum(egyeni_lmp),
                                            egyeni_kormanyvaltok = sum(egyeni_kormanyvaltok),
                                            egyeni_jobbik = sum(egyeni_jobbik),
                                            orszagos_fidesz = sum(fidesz),
                                            orszagos_fidesz_lmp = sum(lmp),
                                            orszagos_fidesz_kormanyvaltok = sum(kormanyvaltok),
                                            orszagos_fidesz_jobbik = sum(jobbik),
                                            egyeni_fidesz_pc = sum(egyeni_fidesz) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                            egyeni_lmp_pc = sum(egyeni_lmp) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                            egyeni_kormanyvaltok_pc = sum(egyeni_kormanyvaltok) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                            egyeni_jobbik_pc = sum(egyeni_jobbik) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                            orszagos_fidesz_pc = sum(fidesz) / sum(fidesz + lmp + kormanyvaltok + jobbik + egyeb),
                                            orszagos_lmp_pc = sum(lmp) / sum(fidesz + lmp + kormanyvaltok + jobbik + egyeb),
                                            orszagos_kormanyvaltok_pc = sum(kormanyvaltok) / sum(fidesz + lmp + kormanyvaltok + jobbik + egyeb),
                                            orszagos_jobbik_pc = sum(jobbik) / sum(fidesz + lmp + kormanyvaltok + jobbik + egyeb),
                                            szavazokorok_szama = sqrt(sum(szavazokorok_szama))),
                                     by = oevk_id][order(oevk_id)] 
  
  write.csv(by_oevk_2014, 'oevk2014_clean.csv')
  
  
###MERGING 2014 with 2018
  
  
  ###### data of 2014 - to be merged with 2018 on OEVK level - some editing in Excel has been done
  
  new_by_oevk_2014 <- read.csv("oevk2014_clean_new.csv")
  egyeni_lista_by_oevk_2018 <- read.csv("2018_by_oevk_clean.csv")
  
  new_by_oevk_2014 <- as.data.table(new_by_oevk_2014)
  egyeni_lista_by_oevk_2018 <- as.data.table(egyeni_lista_by_oevk_2018)
  
  
  oevk_2014_vs_2018 <- merge(new_by_oevk_2014, egyeni_lista_by_oevk_2018, by = "oevk_id")
  
  write.csv(oevk_2014_vs_2018, "egyeni_oevk_2014_vs_2018.csv")
  
#### regression example from 106_OEVK_2014_glm - will be appiled to 2014-2018 prediction
  
  oevk_2014_vs_2018_glm_fidesz <- glm(egyeni_fidesz_pc.y ~ egyeni_fidesz_pc.x + szavazokorok_szama + telepulesek_szama + profil + varos_aranya, data=oevk_2014_vs_2018)
  summary(oevk_2014_vs_2018_glm_fidesz)
  coeftest(oevk_2014_vs_2018_glm_fidesz)
  
  logitcoeffs_oevk_2014_vs_2018_glm_fidesz <- glm(egyeni_fidesz_pc.y ~ egyeni_fidesz_pc.x + szavazokorok_szama + telepulesek_szama + profil + varos_aranya, data=oevk_2014_vs_2018, family='binomial')
  
  summary(logitcoeffs_oevk_2014_vs_2018_glm_fidesz)
  
  oevk_2014_vs_2018$pred_logit_fidesz <- predict.glm(logitcoeffs_oevk_2014_vs_2018_glm_fidesz, type="response")
  
  ggplot(data = oevk_2014_vs_2018, aes(x=egyeni_fidesz_pc.y, y= pred_logit_fidesz)) + xlim(0.25, 0.65) + ylim(0.25, 0.65) +
    geom_line(aes(x=pred_logit, y=pred_logit), colour="orange") +
    geom_point()
  
  
  ggplot(data = oevk_2014_vs_2018, aes(x=egyeni_fidesz_pc.y, y= pred_logit_fidesz)) + xlim(0.25, 0.65) + ylim(0.25, 0.65) +
    geom_text(aes(label=oevk_id), size = 1.5, check_overlap = FALSE) +
    geom_point(color ="red") +
    geom_smooth(method = "loess") +  ggtitle("Fidesz - Actual and predicted vote share in each OEVK") + xlab("2018 Fidesz vote %") + ylab("GLM prediction") +
    geom_line(aes(x=pred_logit_fidesz, y=pred_logit_fidesz), color="green", size = 1) +
    theme_minimal() 
  ggsave(("fidesz_GLM_by_OEVK.png"), width = 25, height = 25, units = c("cm"))
  
  ### UNIFORM SWING vote share
  
  #ALL tables - TO BE CURATED (ADD LISTAS VOTES)
  
  oevk_2014_vs_2018 <- oevk_2014_vs_2018[, Fidesz_2014_orszagos_UNS := 44.13] 
  data_UNS <- data_UNS[, Kormanyvaltok_2014_orszagos := 26.86]
  data_UNS <- data_UNS[, Jobbik_2014_orszagos := 20.34]
  data_UNS <- data_UNS[, LMP_2014_orszagos := 4.98]
  data_UNS <- data_UNS[, Egyeb_2014_orszagos := 3.69]
  
  data_UNS <- data_UNS[, reszvetel_2014 := 60.99] 
  
  #friss?theto ?j adatok alapj?n
  
  oevk_2014_vs_2018 <- oevk_2014_vs_2018[, Fidesz_2018_orszagos_UNS := 47.73] 
  data_UNS <- data_UNS[, MSZP_P_2018_orszagos := 12.46]
  data_UNS <- data_UNS[, Jobbik_2018_orszagos := 19.96]
  data_UNS <- data_UNS[, LMP_2018_orszagos := 7.43]
  data_UNS <- data_UNS[, DK_2018_orszagos := 5.6]
  data_UNS <- data_UNS[, Momentum_2018_orszagos := 3.18]
  
  data_UNS <- data_UNS[, reszvetel_2018 := 70.2]  
  
  
  
  oevk_2014_vs_2018$Fidesz_2018_UNS_egyeni_pc <- oevk_2014_vs_2018[, egyeni_fidesz_pc.x * (Fidesz_2018_orszagos_UNS / Fidesz_2014_orszagos_UNS)] 
  oevk_2014_vs_2018$Fidesz_2018_UNS_listas_pc <- oevk_2014_vs_2018[, orszagos_fidesz_pc * (Fidesz_2018_orszagos_UNS / Fidesz_2014_orszagos_UNS)]
  
  ggplot(data = oevk_2014_vs_2018, aes(x=egyeni_fidesz_pc.y, y= Fidesz_2018_UNS_egyeni_pc)) + xlim(0.25, 0.65) + ylim(0.25, 0.65) +
    geom_text(aes(label=oevk_id), size = 1.5, check_overlap = FALSE) +
    geom_point(color ="red") +
    geom_smooth(method = "loess") +  ggtitle("Fidesz - Actual and predicted vote share in each OEVK") + xlab("2018 Fidesz vote %") + ylab("UNS prediction") +
    geom_line(aes(x=Fidesz_2018_UNS_egyeni_pc, y=Fidesz_2018_UNS_pc), color="green", size = 1) +
    theme_minimal() 
  ggsave(("fidesz_UNS_by_OEVK.png"), width = 25, height = 25, units = c("cm"))
  
  ggplot(data = oevk_2014_vs_2018, aes(x=egyeni_fidesz_pc.y, y= Fidesz_2018_UNS_egyeni_pc)) + xlim(0.25, 0.65) + ylim(0.25, 0.65) +
    geom_text(aes(label=profil), size = 3, check_overlap = FALSE) +
    geom_smooth(method = "loess") +  ggtitle("Fidesz - Actual and predicted vote share in each OEVK type") + xlab("2018 Fidesz vote %") + ylab("UNS prediction") +
    geom_line(aes(x=Fidesz_2018_UNS_egyeni_pc, y=Fidesz_2018_UNS_pc), color="green", size = 1) +
    theme_minimal() 
  ggsave(("fidesz_UNS_by_OEVK_type.png"), width = 25, height = 25, units = c("cm"))
  
  
  

### UNS deviation   
  
  #FIDESZ
  
  oevk_2014_vs_2018$Fidesz_2018_UNS_egyeni_pc_elteres <- oevk_2014_vs_2018[, egyeni_fidesz_pc.y - Fidesz_2018_UNS_egyeni_pc] 
  
  ggplot(oevk_2014_vs_2018, aes(egyeni_fidesz_pc.y, Fidesz_2018_UNS_egyeni_pc_elteres)) + geom_point(color ="red") +
    geom_text(aes(label=oevk_id), size = 2) + geom_smooth(method = "lm") + ggtitle("Fidesz actual vote share and prediction error by OEVK") +
    xlab("Fidesz vote share") + ylab("Prediction error") +
    xlim(0.25, 0.65) +
    theme_minimal()
  ggsave(("fidesz_UNS_deviation_by_OEVK.png"), width = 25, height = 25, units = c("cm"))
  

  #TBD - all UNS deviations
  
  
#### 2014-2018 UNS from 106_OEVK_2014_glm file
  
  setwd('C:/Users/adasan01/OneDrive - ARM/Documents/GitHub/106/capstone_adat')
  data <- read.csv("vote_counts_precincts_2b.csv")
  
  data <- as.data.table(data)
  
  #kamupartok egyesitese es szurese
  kamu <- subset(data, select = c(id, mcp, haza_nem_elado, sms, fkgp, udp, sem, jesz, ump, munkaspart, szocdemek, kti, egyutt2014, zoldek, osszefogas))
  
  kamu$egyeb <- rowSums( kamu[,2:15] )
  
  kamu2 <- subset(kamu, select = c(id, egyeb))
  
  data2 <- merge(data, kamu2, by="id")
  
  #reszveteli adatok, 4 part listaja, egyeni, egyeb partok lista + megye, telepules, szavazokor kulon
  
  data_clean <- subset(data2, select = c(id, atjelentkezettek, szavazokor, oevk_id, szavazok, reszvetel, fidesz, lmp, kormanyvaltok, jobbik, egyeni_fidesz, egyeni_lmp, egyeni_kormanyvaltok, egyeni_jobbik, egyeb))
  
  
  
  #szavazasra jogosultak szama / szavazokor
  
  data_clean <- data_clean[, ossz_szavazo := round(szavazok / (reszvetel /100), digits = 0)] 
  
  #szavazokorok szama 
  
  data_szavazokorok_szama <- data_clean[, list(szavazokorok_szama = .N), by = oevk_id]
  
  data_clean <- merge(data_clean, data_szavazokorok_szama, by = "oevk_id")
  
  
  #UJ VALTOZOK - partok szazalekos egyeni eredmenye egymashoz viszonyitva (kamupartok nelkul)
  
  data_clean <- data_clean[, egyeni_fidesz_pc := egyeni_fidesz / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)] 
  
  data_clean <- data_clean[, egyeni_kormanyvaltok_pc := egyeni_kormanyvaltok / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)]
  
  data_clean <- data_clean[, egyeni_jobbik_pc := egyeni_jobbik / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)]
  
  data_clean <- data_clean[, egyeni_lmp_pc := egyeni_lmp / (egyeni_fidesz + egyeni_jobbik + egyeni_kormanyvaltok + egyeni_lmp + egyeb)]
  
  
  #uniform swing kalkulacio data_UNS
  
  #new inputs - 
  
  data_UNS <- data_clean[, Fidesz_2014_orszagos := 44.13] 
  data_UNS <- data_UNS[, Kormanyvaltok_2014_orszagos := 26.86]
  data_UNS <- data_UNS[, Jobbik_2014_orszagos := 20.34]
  data_UNS <- data_UNS[, LMP_2014_orszagos := 4.98]
  data_UNS <- data_UNS[, Egyeb_2014_orszagos := 3.69]
  
  data_UNS <- data_UNS[, reszvetel_2014 := 60.99] 
  
  #friss?theto ?j adatok alapj?n
  
  data_UNS <- data_UNS[, Fidesz_2018_orszagos := 47.73] 
  data_UNS <- data_UNS[, MSZP_P_2018_orszagos := 12.5]
  data_UNS <- data_UNS[, Jobbik_2018_orszagos := 23.5]
  data_UNS <- data_UNS[, LMP_2018_orszagos := 6]
  data_UNS <- data_UNS[, DK_2018_orszagos := 7.5]
  data_UNS <- data_UNS[, Momentum_2018_orszagos := 3]
  data_UNS <- data_UNS[, Egyutt_2018 := 2]
  data_uns <- data_UNS[, MKKP_2018 := 1]
  #egyeb 1.5%
  
  
  data_UNS <- data_UNS[, reszvetel_2018 := 70.2]  
  
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
                                      Momentum_2018 = sum((egyeni_lmp * 0.6) * reszvetel_2018_szorzo),
                                      Egyutt_2018 = sum((egyeni_lmp * 0.4) * reszvetel_2018_szorzo),
                                      MKKP_2018 = sum((egyeni_lmp * 0.2) * reszvetel_2018_szorzo)),
                               by = oevk_id][order(oevk_id)]
  
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
  
  
  write.csv(by_oevk_UNS_2018,'2018_UNS_baseline.csv')
  