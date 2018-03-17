clear all
capture log close
log using elorejelzes.txt, text replace

use szavazokor_szintu
gen megyekod = real(substr(id2010,2,2))
merge m:1 megyekod partnev using ../adat/part/google_trends, keep(master match) nogen
merge m:1 partnev using ../adat/part/kozvelemeny, keep(master match) nogen

keep if !missing(oevk)

gen kerulet = real(substr(oevk,6,2))
* paros szamu oevk-k kihagyva minden megyeben
gen byte holdout_sample = int(kerulet/2)*2==kerulet

foreach X of var ma* arany* {
	gen ln_`X' = ln(0.1+`X')
}
egen telepules_meret = sum(osszes2014), by(id2010)
gen telkat = telepules_meret
recode telkat min/10000=1 10000/100000=2 100000/max=3
gen byte nagyvaros = (telepules_meret>100000)|(megyekod==1)
replace telkat = 0 if megyekod==1
gen ln_becsult = .
tempvar becsult

* create prediction for 2018
expand 2, gen(future)

foreach X in ln_arany ln_ma6google ln_ma3google {
	capture replace `X'2010=`X'2014 if future
	capture replace `X'2014=`X'2018 if future
}
gen aranysq = ln_arany2010^2
gen aranycb = ln_arany2010^3

forval t=0/3 {
	areg ln_arany2014 ln_arany2010 arany?? ln_ma3google2014 ln_ma3google2010 if telkat==`t' & !holdout & !future [fw=osszes2014], a(szavazokor) vce(cluster oevk)
	predict `becsult'
	replace ln_becsult = `becsult' if telkat==`t'
	drop `becsult'
}

gen becsult_szavazat = exp(ln_becsult)
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

keep if future
keep oevk partnev szavazat2014 becsult_szavazat arany_szavazat2010 arany_szavazat2014 arany_becsult_szavazat

export delimited listas_106.csv, replace


collapse (sum) szavazat2014 becsult_szavazat, by(partnev)
egen total=sum(szavazat2014)
foreach X of var szavazat2014 becsult_szavazat {
	gen arany_`X' = `X'/total*100
}

capture log close
