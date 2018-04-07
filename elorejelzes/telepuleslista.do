capture log close
log using telepuleslista.txt, text replace
tempfile telepules1 oevk
import delimited ../adat/telepules/telepules_kodok.csv, clear varnames(1) encoding("utf-8")
save `telepules1'

import delimited listas_106_ujpartok.csv, clear varnames(1) encoding("utf-8")

replace partnev = "baloldal" if inlist(partnev,"dk","mszp","egyutt")
collapse (sum) becsult_szavazat, by(oevk partnev)

egen rank = rank(-becsult_szavazat), by(oevk)
egen total = sum(becsult_szavazat), by(oevk)
gen becsult_arany = becsult_szavazat/total*100
keep oevk rank partnev becsult_arany
reshape wide becsult_arany partnev, i(oevk) j(rank)
save `oevk'


import delimited ../adat/telepules/szavazati_aranyok_2014.csv, clear varnames(1) encoding("utf-8")
merge m:m id2010 using `telepules1', nogen
merge m:1 oevk using `oevk', nogen

gen valasztopolgarok = int(osszes2014/reszveteli_arany2014*100)

* a 3. es tovabbi helyezett hany szazaleka kell a 2.-ra szavazzon, hogy megelozzek az 1-t?
gen szukseges_atszavazas = (becsult_arany1-becsult_arany2)/(100-becsult_arany1-becsult_arany2)*100 if partnev1=="fidesz"
* 2/3-nal nagyobb atszavazassal szamolni remenytelen
gen byte nyerheto = (partnev1=="fidesz") & (becsult_arany1<50) & (szukseges_atszavazas<=66)
gen byte kiegyensulyozott = nyerheto & abs(becsult_arany2-becsult_arany3)<15

* jobbikrol balra
scalar max_atszavazas_JB = 33
* balrol jobbikra
scalar max_atszavazas_BJ = 66
* balrol balra (sorry LMP)
scalar max_atszavazas_BB = 75
scalar min_atszavazas = 10
gen atszavaz = 0
forval i=3/6 {
	gen gap`i' = (becsult_arany2-becsult_arany`i')/becsult_arany2
	* minel egyertelmubb a rangsor, annal szivesebben szavaznak at
	if (partnev2=="jobbik") {
		scalar max_atszavazas = max_atszavazas_BJ
	}
	if (partnev2=="baloldal") {
		if (partnev`i'=="jobbik") {
			scalar max_atszavazas = max_atszavazas_JB
		}
		else {
			scalar max_atszavazas = max_atszavazas_BB
		}
	}
	replace atszavaz = atszavaz + (min_atszavazas+gap`i'*(max_atszavazas-min_atszavazas)) * becsult_arany`i' / 100
}
gen byte atbillen = (becsult_arany2+atszavaz)>becsult_arany1 & partnev1=="fidesz"

egen oevk_tag = tag(oevk)

* ad-hoc atszavazsi modellel az OEVK-k elorejlezese
tab partnev1  if oevk_tag & !atbillen
tab partnev2 if oevk_tag & atbillen

tab nyerheto kiegyensulyozott if oevk_tag 
tab nyerheto kiegyensulyozott 
tab nyerheto kiegyensulyozott [fw=osszes2014]
tab nyerheto kiegyensulyozott [fw=valasztopolgarok]

preserve
keep if nyerheto & kiegyensulyozott
sort telepules_nev
gen str location = telepules_nev + ", Hungary"

export delimited location if partnev2=="baloldal" & valasztopolgarok>=500 using baloldal.csv, replace
export delimited location if partnev2=="jobbik" & valasztopolgarok>=500 using jobbik.csv, replace
restore
sort oevk id2010
export delimited telepules_szintu_listas_elorejelzes.csv, replace

replace partnev1=partnev2 if atbillen
tab partnev1 if oevk_tag 
export delimited oevk partnev1 if oevk_tag using oevk_elorejelzes.csv, replace
capture log close
