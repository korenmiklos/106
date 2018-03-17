clear all

import delimited ../adat/part/google_trends/GTrend_megye_aranyok.csv, clear varnames(1) encoding("utf-8")
ren * _*
ren _honap honap
ren _megye megye
reshape long _, i(megye honap) j(part) string
ren _ google
replace google=google*100

replace part="baloldal" if inlist(part,"mszp","dk","egyutt")
collapse (sum) google, by(part megye honap)


gen date = monthly(honap,"YM")
egen group = group(megye part)
tsset group date, monthly

* 4-year growth rate
gen ma3 = (google+L.google+L2.google)/3
gen ma6 = (google+L.google+L2.google+L3.google+L4.google+L5.google)/6
gen change = ma3 - L48.ma3
table part megye if honap>="2018-01", c(mean change)

keep if substr(honap,6,2)=="02"
gen year = substr(honap,1,4)

keep year megye part ma*
ren ma* ma*google
reshape wide ma3google ma6google, i(part megye) j(year) string
ren megye megyekod
ren part partnev

save ../adat/part/google_trends, replace

import delimited "../adat/part/kozvelemenykutatok/biztos_valasztok.csv", clear varnames(1) encoding("utf-8")
replace part="baloldal" if inlist(part,"mszp","dk","egyutt")
collapse (sum) szazalek, by(part datum kutato)

gen honap = monthly(substr(datum,1,7),"YM")
format honap %tm
egen i = group(part kutato)
tsset i honap, monthly

replace szazalek = L.szazalek if missing(szazalek) & !missing(L.szazalek)

collapse (mean) szazalek , by(part datum honap)
ren szazalek kozvelemeny

egen i = group(part)
tsset i honap, monthly
gen ma6kozvelemeny = (kozv+L.kozv+L2.kozv+L3.kozv+L4.kozv+L5.kozv)/6

keep if substr(datum,6,2)=="01"
gen year = substr(datum,1,4)
drop datum honap
reshape wide kozvelemeny ma6kozvelemeny, i(part) j(year) string
ren part partnev

save ../adat/part/kozvelemeny, replace
