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

### TELEPULES

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
  

  
  
  
  
  
  #TSTAR telepules adatok - (Budapest will be merged)
  
  
  data_telepulesadatok_t_star <-read.csv("Telepulesadatok_t_star.csv")
  colnames(data_telepulesadatok_t_star)[1] <- "telep_nev"
  
  egyeni_lista_2018_telep_clean <- read.csv("2018_by_telep_clean.csv", fileEncoding = "UTF-8")
  
  
  t_star_telepules_2018 <- merge(data_telepulesadatok_t_star, egyeni_lista_2018_telep_clean, by.x = "telep_nev")
  
  ###t_star_telepules_2018 is all basic t_star data with all towns (Budapest merged)
  
  ###additional data tables t-star (unemployment rate, pensioners, people on benefits, etc)

  
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
  
  
  colnames(t_star_telepules_2018)[1] <- "telnev"
  
  merge_all_t_star1 <- merge(data_allaskeresok, t_star_telepules_2018, by = "telnev")  
  merge_all_t_star2 <- merge(data_koraranyok, merge_all_t_star1, by = c("telnev", "ev.3"))
  merge_all_t_star3 <- merge(data_munkaadok, merge_all_t_star2, by = c("telnev", "ev.3"))
  merge_all_t_star4 <- merge(data_nyugdijasok, merge_all_t_star3, by = c("telnev", "ev.3"))
  
  merge_all_t_star4 <- subset(merge_all_t_star4, select = c(1:2, 4:5, 7:125))
  
  merge_all_t_star_all <-  merge(data_tamogatottak, merge_all_t_star4, by = c("telnev", "ev.3"))
  
  ##merge_all_t_star_all is all data with 300 towns missing
  
#### UNS predictions and deviation   
    
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
  
  data_clean <- subset(data2, select = c(id, telepules_id, atjelentkezettek, szavazokor, oevk_id, szavazok, reszvetel, fidesz, lmp, kormanyvaltok, jobbik, egyeni_fidesz, egyeni_lmp, egyeni_kormanyvaltok, egyeni_jobbik, egyeb))
  
  
  
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
  
  #oevk szintu eredmenyek (4 fobb part)
  
  by_szavazokor_2014 <- data_clean[, list(szavazok, reszvetel, ossz_szavazo, egyeni_fidesz, egyeni_lmp, egyeni_kormanyvaltok, egyeni_jobbik, egyeb, szavazokorok_szama), 
                                   by = list(oevk_id, id)]
  
  by_oevk_2014 <- by_szavazokor_2014[, list(szavazok = sum(szavazok),
                                            reszvetel = sum(szavazok) / sum(ossz_szavazo),
                                            ossz_szavazo = sum(ossz_szavazo),
                                            egyeni_fidesz = sum(egyeni_fidesz),
                                            egyeni_lmp = sum(egyeni_lmp),
                                            egyeni_kormanyvaltok = sum(egyeni_kormanyvaltok),
                                            egyeni_jobbik = sum(egyeni_jobbik),
                                            egyeni_fidesz_pc = sum(egyeni_fidesz) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                            egyeni_lmp_pc = sum(egyeni_lmp) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                            egyeni_kormanyvaltok_pc = sum(egyeni_kormanyvaltok) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                            egyeni_jobbik_pc = sum(egyeni_jobbik) / sum(egyeni_fidesz + egyeni_lmp + egyeni_kormanyvaltok + egyeni_jobbik + egyeb),
                                            szavazokorok_szama = sqrt(sum(szavazokorok_szama))),
                                     by = oevk_id][order(oevk_id)] 
  
  write.csv(by_oevk_2014, 'oevk2014_clean.csv')
  
  
###MERGING 2014 with 2018
  
  
  ###### data of 2014 - to be merged with 2018 on OEVK level
  
  ##OUTPUT from 106_OEVK_2014_glm
  
  new_by_oevk_2014 <- read.csv("oevk2014_clean_new.csv")
  new_by_oevk_2014 <- as.data.table(new_by_oevk_2014)
  
  
  oevk_2014_vs_2018 <- merge(new_by_oevk_2014, egyeni_lista_by_oevk_2018, by = "oevk_id")
  
  write.csv(oevk_2014_vs_2018, "egyeni_oevk_2014_vs_2018.csv")
  
#### regression example from 106_OEVK_2014_glm - will be appiled to 2014-2018 prediction
  
  regression_2010_2014_fidesz <- glm(FIDESZ_2014_pc ~ FIDESZ_2010_pc + szavazokorok_szama.x + telepulesek_szama + profil + varos_aranya, data=data_2010_2014_szavazokor)
  summary(regression_2010_2014_fidesz)
  coeftest(regression_2010_2014_fidesz)
  
  logitcoeffs_fidesz_2010_2014 <- glm(FIDESZ_2014_pc ~ FIDESZ_2010_pc + szavazokorok_szama.x + telepulesek_szama + profil + varos_aranya, data=data_2010_2014_szavazokor, family='binomial')
  
  summary(logitcoeffs_fidesz_2010_2014)
  
  data_2010_2014_szavazokor$pred_logit <- predict.glm(logitcoeffs_fidesz_2010_2014, type="response")
  
  ggplot(data = data_2010_2014_szavazokor, aes(x=FIDESZ_2014_pc, y= pred_logit)) + xlim(0.08, 0.55) + ylim(0.08, 0.55) +
    geom_line(aes(x=pred_logit, y=pred_logit), colour="orange") +
    geom_point()
  
  
#### 2014-2018 UNS from 106_OEVK_2014_glm file
  
  #uniform swing kalkulacio data_UNS
  
  #new inputs - orszagos pollok atlaga -4% Fidesz, frissitve m?rcius 2-an
  
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
  
  data_UNS <- data_UNS[, Fidesz_2018_orszagos := 47.73] 
  data_UNS <- data_UNS[, MSZP_P_2018_orszagos := 12.46]
  data_UNS <- data_UNS[, Jobbik_2018_orszagos := 19.96]
  data_UNS <- data_UNS[, LMP_2018_orszagos := 7.43]
  data_UNS <- data_UNS[, DK_2018_orszagos := 5.6]
  data_UNS <- data_UNS[, Momentum_2018_orszagos := 3.18]
  
  data_UNS <- data_UNS[, reszvetel_2018 := 70.4]  
  
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
  
  
  write.csv(by_oevk_UNS_2018,'2018_UNS.csv')
  
  
  ### TO BE ADDED UNS deviation visuals
  
  