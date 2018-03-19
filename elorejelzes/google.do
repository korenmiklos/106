clear all

scalar decay = 0.5

import delimited ../adat/part/google_trends/GTrend_megye_aranyok.csv, clear varnames(1) encoding("utf-8")
ren * _*
ren _honap honap
ren _megye megye
reshape long _, i(megye honap) j(part) string
ren _ google
replace google=google*100

replace part="baloldal" if inlist(part,"mszp","dk","egyutt")
replace part="kispart" if inlist(part,"lmp","momentum")
collapse (sum) google, by(part megye honap)


gen date = monthly(honap,"YM")
egen group = group(megye part)
tsset group date, monthly

local X google
gen ma = (`X'+decay*L.`X'+decay^2*L2.`X'+decay^3*L3.`X')/(1+decay+decay^2+decay^3)

* FIXME: egyelore csak februari adatok vannak
replace honap = "2018-03" if honap=="2018-02"
keep if substr(honap,6,2)=="03"
gen year = substr(honap,1,4)

keep year megye part ma
ren ma google
reshape wide google, i(part megye) j(year) string
ren megye megyekod
ren part partnev

save ../adat/part/google_trends, replace

import delimited "../adat/part/kozvelemenykutatok/biztos_valasztok.csv", clear varnames(1) encoding("utf-8")
replace part="baloldal" if inlist(part,"mszp","dk","egyutt")
replace part="kispart" if inlist(part,"lmp","momentum")
collapse (sum) szazalek, by(part datum kutato)

gen honap = monthly(substr(datum,1,7),"YM")
format honap %tm
egen i = group(part kutato)
tsset i honap, monthly

replace szazalek = L.szazalek if missing(szazalek) & !missing(L.szazalek)

collapse (mean) szazalek , by(part datum honap)

egen i = group(part)
tsset i honap, monthly
local X szazalek
gen kozvelemeny = (`X'+decay*L.`X'+decay^2*L2.`X'+decay^3*L3.`X')/(1+decay+decay^2+decay^3)

* FIXME: egyelore csak januari adatok vannak
replace datum = "2018-03" if datum=="2018-01"
keep if substr(datum,6,2)=="03"
gen year = substr(datum,1,4)
drop datum honap szazalek
reshape wide kozvelemeny, i(part) j(year) string
ren part partnev

save ../adat/part/kozvelemeny, replace
