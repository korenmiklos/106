clear all
local files szervezet listas reszvetel
tempfile v2010 v2014 oevk

local 2010 ../adat/2010/
local 2014 ../adat/jelolt/

tempfile telepules listas2010 szervezet2010 listas2014 szervezet2014 reszvetel2010 reszvetel2014
import delimited ../adat/telepules/telepules_kodok.csv, clear varnames(1) encoding("utf-8")
save `telepules'

import delimited ../adat/telepules/szavazokor_oevk_2014.csv, clear varnames(1) encoding("utf-8")
save `oevk'

foreach fn in `files' {
	foreach y in 2010 2014 {
		import delimited ``y''`fn'.csv, clear varnames(1) encoding("utf-8")
		save ``fn'`y''
	}
}

use `listas2014'
gen str telepules_id = substr(szavazokor,1,8)
merge m:1 telepules_id using `telepules', nogen
*collapse (sum) szavazat, by(id2010 part)

gen id = part
merge m:1 id using `szervezet2014', nogen
merge m:1 szavazokor using `reszvetel2014', nogen
merge m:1 szavazokor using `oevk', nogen
gen ev = 2014
drop if id=="id"

* megye listak, mindig mashogy irjak a partok nevet
gen     partnev = "fidesz" if substr(strlower(szervezet),1,6)=="fidesz"
replace partnev = "jobbik" if substr(strlower(szervezet),1,6)=="jobbik"
replace partnev = "lmp" if inlist(substr(strlower(szervezet),1,5),"lehet","lmp")
replace partnev = "mszp" if inlist(substr(strlower(szervezet),1,18),"magyar szocialista","mszp")
replace partnev = "mdf" if inlist(substr(strlower(szervezet),1,18),"magyar demokrata f","mdf")
replace partnev = "baloldal" if substr(strlower(szervezet),1,8)=="mszp-egy"
replace partnev = "egyeb" if missing(partnev)

replace partnev = "baloldal" if partnev=="mszp" | substr(strlower(szervezet),1,4)=="munk"
save `v2014', replace

use `listas2010'
merge m:1 szavazokor using `reszvetel2010', nogen

egen tag = tag(szavazokor part)
local valtozok valasztopolgarok ervenyes ervenytelen
foreach X of var `valtozok' {
	replace `X' = `X'*tag
}

gen str id2010 = substr(szavazokor,1,8)
collapse (sum) szavazat `valtozok', by(id2010 part)

gen id = part
merge m:1 id using `szervezet2010', nogen
gen ev = 2010
tab szervezet
do partnevek
tab szervezet partnev [fw=szavazat], miss

drop szervezet part id

collapse (sum) szavazat (mean) `valtozok', by(partnev id2010 ev)
egen osszes = sum(szavazat), by(id2010 ev)
gen arany = szavazat/osszes*100
gen reszveteli_arany = ervenyes/valasztopolgarok*100
drop ervenyes ervenytelen valaszto

reshape wide szavazat osszes arany reszveteli_arany, i(partnev id2010) j(ev)
save `v2010', replace
export delimited using ../adat/telepules/telepules_listas_arany.csv, delimiter(",") replace 
use `v2014'
drop szervezet part id
collapse (sum) szavazat2014=szavazat ervenyes valasztopolgarok, by(szavazokor partnev oevk id2010 ev)
gen reszveteli_arany2014 = ervenyes/valasztopolgarok*100
drop ervenyes valaszto

egen osszes2014 = sum(szavazat), by(szavazokor)
gen arany2014 = szavazat2014/osszes2014*100

merge m:1 id2010 partnev using `v2010', nogen

save szavazokor_szintu, replace

