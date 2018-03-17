clear all
local files jelolt szervezet listas

tempfile `files' google
foreach fn in `files' {
	import delimited ../adat/jelolt/`fn'.csv, clear varnames(1) encoding("utf-8")
	save ``fn''
}

import delimited ../adat/part/google/megye_2014.csv, clear varnames(1) encoding("utf-8")
save `google'

use `listas', clear
gen str megye = substr(szavazokor,1,3)

collapse (sum) szavazat, by(part megye)
ren part id
merge m:1 id using `szervezet'
drop _m

gen str part = substr(szervezet,1,4)
local fidesz FIDE
local jobbik JOBB
local baloldal MSZP
local lmp LMP
local osszesen Pár
local partok fidesz jobbik baloldal lmp osszesen
foreach X in `partok' {
	replace part="`X'" if part=="``X''"
}
keep if inlist(part,"fidesz", "jobbik", "baloldal", "lmp", "osszesen")
keep megye part szavazat

reshape wide szavazat, i(megye) j(part) string
foreach X in `partok' {
	gen arany_`X' = szavazat`X'/szavazatosszesen*100
}

ren megye megyekod
merge 1:1 megyekod using `google'
drop _m

gen baloldal = mszp+dk
