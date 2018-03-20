clear all

scalar decay = 0.33

tempfile nagypartok kispart baloldal
import delimited ../adat/part/google_trends/nagypartok.csv, clear varnames(1) encoding("utf-8")
ren search_volume search_volume1
save `nagypartok', replace
import delimited ../adat/part/google_trends/kispartok.csv, clear varnames(1) encoding("utf-8")
ren search_volume search_volume2

merge 1:1 geo month search_term using `nagypartok', 
gen megye=real(substr(geo,2,2))

* use common scaling. "mszp" are in both samples
gen search_volume = search_volume2
tempvar sv
forval m=1/20 {
	reg search_volume2 search_volume1 if megye==`m', nocons
	predict `sv',
	replace search_volume = `sv' if megye==`m' & missing(search_volume2)
	drop `sv'
}
egen `sv' = sum(search_volume), by(geo month)
replace search_volume = search_volume/`sv'*100
drop `sv'

keep megye month search_term search_volume
ren search_term part
ren search_volume google

preserve
	collapse (sum) google, by(part megye month)

	gen date = monthly(month,"YM")
	egen group = group(megye part)
	tsset group date, monthly

	local X google
	gen ma = (`X'+decay*L.`X'+decay^2*L2.`X'+decay^3*L3.`X')/(1+decay+decay^2+decay^3)

	keep if substr(month,6,2)=="03"
	gen year = substr(month,1,4)

	keep year megye part ma
	ren ma google
	reshape wide google, i(part megye) j(year) string
	ren megye megyekod
	ren part partnev
	save ../adat/part/google_trends, replace
restore
replace part="baloldal" if inlist(part,"mszp","dk","egyutt")
replace part="kispart" if inlist(part,"lmp","momentum")
	collapse (sum) google, by(part megye month)

	gen date = monthly(month,"YM")
	egen group = group(megye part)
	tsset group date, monthly

	local X google
	gen ma = (`X'+decay*L.`X'+decay^2*L2.`X'+decay^3*L3.`X')/(1+decay+decay^2+decay^3)

	keep if substr(month,6,2)=="03"
	gen year = substr(month,1,4)

	keep year megye part ma
	ren ma google
	reshape wide google, i(part megye) j(year) string
	ren megye megyekod
	ren part partnev
	save ../adat/part/google_trends2014, replace


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
