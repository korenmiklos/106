clear all
capture log close
log using elorejelzes.txt, text replace

use szavazokor_szintu

replace part="baloldal" if inlist(part,"mszp","dk","egyutt")
replace part="kispart" if inlist(part,"lmp","momentum")

collapse (sum) szavazat2010 szavazat2014 arany2010 arany2014 (mean) osszes2010 osszes2014, by(szavazokor id2010 oevk partnev)

gen megyekod = real(substr(id2010,2,2))
merge m:1 megyekod partnev using ../adat/part/google_trends, keep(master match) nogen
merge m:1 partnev using ../adat/part/kozvelemeny, keep(master match) nogen

keep if !missing(oevk)
*mvencode google* kozvelemeny*, mv(0) override

gen kerulet = real(substr(oevk,6,2))
* paros szamu oevk-k kihagyva minden megyeben
gen byte holdout_sample = int(kerulet/3)*3==kerulet

foreach X of var google* kozvelemeny* arany* {
	gen ln_`X' = ln(0.5+`X')
}
egen telepules_meret = sum(osszes2014), by(id2010)

gen str budapest_kerulet = substr(id2010,7,2) if megye==1
gen byte buda = inlist(budapest_kerulet,"01","02","11","12","22")
gen byte pest = megye==1 & !buda

gen telkat = telepules_meret
recode telkat min/3000=1 3000/10000=2 10000/50000=3 50000/100000=4 100000/max=5
gen byte nagyvaros = (telepules_meret>100000)|(megyekod==1)
replace telkat = 5 if buda
replace telkat = 5 if pest
gen becsult_szavazat = .
tempvar becsult

* create prediction for 2018
expand 2, gen(future)

foreach X in ln_arany ln_google ln_kozvelemeny {
	capture replace `X'2010=`X'2014 if future
	capture replace `X'2014=`X'2018 if future
}
gen aranysq = ln_arany2010^2
gen aranycb = ln_arany2010^3

forval t=1/5 {
	di in gre "Teltip: " in ye "`t'"
	areg ln_arany2014 ln_arany2010 ln_google2014 ln_google2010 ln_kozvelemeny2014 if telkat==`t' & !holdout & !future [fw=osszes2010], a(szavazokor) vce(cluster id2010)
	predict `becsult'
	replace becsult_szavazat = `becsult' if telkat==`t'
	* egyeb partokat nehez becsulni
	replace becsult_szavazat = ln_arany2010 if telkat==`t' & partnev=="egyeb"
	drop `becsult'
}

replace becsult_szavazat = exp(becsult_szavazat)
egen total = sum(becsult_szavazat), by(szavazokor future)
replace becsult_szavazat = int(becsult_szavazat/total*osszes2014)

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

BRK
keep if future
keep oevk partnev szavazat2014 becsult_szavazat arany_szavazat2010 arany_szavazat2014 arany_becsult_szavazat

export delimited listas_106.csv, replace

collapse (sum) szavazat2014 becsult_szavazat, by(partnev)
egen total=sum(szavazat2014)
foreach X of var szavazat2014 becsult_szavazat {
	gen arany_`X' = `X'/total*100
}

capture log close
