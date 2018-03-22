clear all
capture log close
log using elorejelzes.txt, text replace

use szavazokor_szintu

replace part="baloldal" if inlist(part,"mszp","dk","egyutt")
replace part="kispart" if inlist(part,"lmp","momentum")
replace part="egyeb" if partnev=="mdf"

collapse (sum) szavazat2010 szavazat2014 arany2010 arany2014 (mean) osszes2010 osszes2014 reszveteli_arany2010 reszveteli_arany2014, by(szavazokor id2010 oevk partnev)

gen megyekod = real(substr(id2010,2,2))
merge m:1 megyekod partnev using ../adat/part/google_trends2014, keepusing(google2010 google2014) keep(master match) nogen
merge m:1 partnev using ../adat/part/kozvelemeny2014, keepusing(kozvelemeny2014) keep(master match) nogen
keep if !missing(oevk)

* korrekciok 2014 orszagos listas eredmenyek alapjan
local partok fidesz baloldal jobbik lmp
local Kfidesz 0.915
local Kbaloldal 0.759
local Kjobbik 1.253
local Klmp 1.343
local Gfidesz 1.809
local Gbaloldal 0.790
local Gjobbik 0.567
local Glmp 0.865

/*foreach p in `partok' {
	replace kozvelemeny2014 = kozvelemeny2014*`K`p'' if partnev=="`p'"
	replace google2014 = google2014*`G`p'' if partnev=="`p'"
	replace google2010 = google2010*`G`p'' if partnev=="`p'"
}*/
egen P = group(partnev)

gen kerulet = real(substr(oevk,6,2))
* paros szamu oevk-k kihagyva minden megyeben
gen byte holdout_sample = int(kerulet/3)*3==kerulet

foreach X of var google* kozvelemeny* *arany* {
	gen ln_`X' = ln(0.5+`X')
}
egen telepules_meret = sum(osszes2014), by(id2010)

gen str budapest_kerulet = substr(id2010,7,2) if megye==1
gen byte buda = inlist(budapest_kerulet,"01","02","11","12","22")
gen byte pest = megye==1 & !buda

* kulonbozo reszveteli aranyu telepulesek
gen polkat = reszveteli_arany2010
recode polkat 0/50=0 50/60=1 60/80=2 80/max=3

gen telkat = telepules_meret
recode telkat min/3000=1 3000/10000=2 10000/50000=3 50000/100000=4 100000/max=5
gen byte nagyvaros = (telepules_meret>100000)|(megyekod==1)
replace telkat = 5 if buda
replace telkat = 5 if pest

* create prediction for 2018
expand 2, gen(future)

foreach X in ln_arany ln_google ln_kozvelemeny {
	capture replace `X'2010=`X'2014 if future
	capture replace `X'2014=`X'2018 if future
}
gen aranysq = ln_arany2010^2
gen aranycb = ln_arany2010^3

local valtozok ln_arany2010 ln_google2014 ln_google2010 ln_kozvelemeny2014
forval t=1/5 {
	di in gre "Teltip: " in ye "`t'"
	* steady-state osszefugges szetosztashoz
	reg ln_arany2014 ln_google2014 ln_kozvelemeny2014 if telkat==`t' [fw=osszes2014 ]
	* egyutthatok elmentese
	scalar G`t' = _b[ln_google2014]
	scalar K`t' = _b[ln_kozvelemeny2014]
	
	foreach X in `valtozok' {
		gen t`t'_`X' = (telkat==`t')*`X'
	}
}
areg ln_arany2014 t?_* i.P if !holdout & !future [fw=osszes2010], a(szavazokor) vce(cluster id2010)
predict becsult_szavazat
* egyeb partokat nehez becsulni
replace becsult_szavazat = ln_arany2010 if partnev=="egyeb"

replace becsult_szavazat = exp(becsult_szavazat)
egen total = sum(becsult_szavazat), by(szavazokor future)
replace becsult_szavazat = int(becsult_szavazat/total*osszes2014)

preserve
	collapse (sum) szavazat2010 szavazat2014 becsult_szavazat, by(oevk partnev holdout future)
	foreach X of var szavazat???? becsult {
		egen total_`X' = sum(`X'), by(oevk future)
		gen arany_`X' = `X'/total_`X'*100
	}

	reg arany_szavazat2014 arany_becsult if holdout & !future [fw=total_szavazat2014], 
	reg arany_szavazat2014 arany_szavazat2010 if holdout & !future [fw=total_szavazat2014], 
	scatter arany_szavazat2014 arany_becsult if holdout & !future, msize(tiny) scheme(s2mono)
	graph export oevk_out_of_sample.png, width(800) replace
	scatter arany_szavazat2014 arany_szavazat2010 if holdout & !future, msize(tiny) scheme(s2mono)
	graph export oevk_2010.png, width(800) replace
restore
keep if future
ren becsult_szavazat csoport_becsult_szavazat
keep oevk szavazokor megyekod telkat partnev szavazat2014 csoport_becsult_szavazat
* uj partok letrehozasa: dk, egyutt, momentum, minden szavazokorben 3 uj sor
expand 1+(partnev=="kispart")+2*(partnev=="baloldal"), gen(ujpart)
replace szavazat2014=. if ujpart
egen partid = seq(), by(szavazokor partnev ujpart)

replace partnev="dk" if ujpart & partid==1 & partnev=="baloldal"
replace partnev="egyutt" if ujpart & partid==2 & partnev=="baloldal"
replace partnev="momentum" if ujpart & partnev=="kispart"
replace partnev="mszp" if partnev=="baloldal"
replace partnev="lmp" if partnev=="kispart"

merge m:1 megyekod partnev using ../adat/part/google_trends, keepusing(google2018 google2014) keep(master match) nogen
merge m:1 partnev using ../adat/part/kozvelemeny, keepusing(kozvelemeny2018) keep(master match) nogen

/*foreach p in `partok' {
	replace kozvelemeny2018 = kozvelemeny2018*`K`p'' if partnev=="`p'"
	replace google2014 = google2014*`G`p'' if partnev=="`p'"
	replace google2018 = google2018*`G`p'' if partnev=="`p'"
}*/


foreach X of var google* kozvelemeny* {
	gen ln_`X' = ln(0.5+`X')
}

* az uj partok tamogatottsagat csoporton belul osztjuk el a google trends es kozvelemeny aranyaban
gen csoport = partnev
replace csoport="kispart" if inlist(csoport,"lmp","momentum")
replace csoport="baloldal" if inlist(csoport,"mszp","dk","egyutt")

gen part_arany2018 = .
forval t=1/5 {
	* becsult sulyok
	replace part_arany2018 = exp(G`t'*ln_google2018 + K`t'*ln_kozvelemeny2018) if telkat==`t'
}
local t 2018
capture drop total`t'
egen total`t' = sum(part_arany`t'), by(szavazokor csoport)
replace part_arany`t' = part_arany`t'/total`t'*100
replace part_arany`t' = 100 if missing(part_arany`t')

gen becsult_szavazat = int(csoport_becsult_szavazat*part_arany2018/100)
collapse (sum) szavazat2014 becsult_szavazat, by(oevk partnev csoport ujpart)
replace szavazat2014=. if ujpart
egen total=sum(szavazat2014), by(oevk)
foreach X of var szavazat2014 becsult_szavazat {
	gen arany_`X' = `X'/total*100
}
drop total
export delimited listas_106_ujpartok.csv, replace

collapse (sum) szavazat2014 becsult_szavazat, by(partnev)
egen total=sum(szavazat2014)
foreach X of var szavazat2014 becsult_szavazat {
	gen arany_`X' = `X'/total*100
}

capture log close
