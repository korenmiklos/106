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

#data <- as.data.table(data)

#data_telepules <- read.csv("106/adat/telepules/telepules_kodok.csv")

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




#TSTAR telepules adatok - TB ADDED (Budapest will be merged)

data_telep_kodok <- read.csv("telepules_kodok_KSH_kod_osztva_10.csv")

telepulesadatok_t_star <-read.csv("Telepulesadatok_t_star.csv")

egyeni_lista_by_telep_t_star <- merge(telepulesadatok_t_star, data_telep_kodok, by=  "ksh_kod")




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
  
