clear all
local files szervezet listas
tempfile v2010 v2014

local 2010 ../adat/2010/
local 2014 ../adat/jelolt/

tempfile telepules listas2010 szervezet2010 listas2014 szervezet2014
import delimited ../adat/telepules/telepules_kodok.csv, clear varnames(1) encoding("utf-8")
save `telepules'

foreach fn in `files' {
	foreach y in 2010 2014 {
		import delimited ``y''`fn'.csv, clear varnames(1) encoding("utf-8")
		save ``fn'`y''
	}
}

use `listas2014'
gen str telepules_id = substr(szavazokor,1,8)
merge m:1 telepules_id using `telepules'
collapse (sum) szavazat, by(id2010 part)

gen id = part
merge m:1 id using `szervezet2014'
drop _m

gen ev = 2014

save `v2014', replace

use `listas2010'
gen str id2010 = substr(szavazokor,1,8)
collapse (sum) szavazat, by(id2010 part)

gen id = part
merge m:1 id using `szervezet2010'
drop _m

gen ev = 2010
append using `v2014'

drop if id=="id"
tab szervezet

* megye listak, mindig mashogy irjak a partok nevet
gen     partnev = "fidesz" if substr(strlower(szervezet),1,6)=="fidesz"
replace partnev = "jobbik" if substr(strlower(szervezet),1,6)=="jobbik"
replace partnev = "lmp" if inlist(substr(strlower(szervezet),1,5),"lehet","lmp")
replace partnev = "mszp" if inlist(substr(strlower(szervezet),1,18),"magyar szocialista","mszp")
replace partnev = "mdf" if inlist(substr(strlower(szervezet),1,18),"magyar demokrata f","mdf")
replace partnev = "baloldal" if substr(strlower(szervezet),1,8)=="mszp-egy"
replace partnev = "egyeb" if missing(partnev)

replace partnev = "baloldal" if partnev=="mszp" | substr(strlower(szervezet),1,4)=="munk"

tab szervezet partnev [fw=szavazat], miss

drop szervezet part id

collapse (sum) szavazat, by(partnev id2010 ev)

egen osszes = sum(szavazat), by(id2010 ev)
gen arany = szavazat/osszes*100

reshape wide szavazat osszes arany, i(partnev id2010) j(ev)

export delimited using ../adat/telepules/telepules_listas_arany.csv, delimiter(",") replace 

/*import delimited ../adat/jelolt/listas.csv, clear varnames(1) encoding("utf-8")
gen str telepules_id = substr(szavazokor,1,8)
merge m:1 telepules_id using `telepules'
gen str oevk = substr(jelolt,1,7)

egen oevk_tag = tag(oevk)
egen veletlen = mean(cond(oevk_tag,uniform(),.)), by(oevk)

gen byte holdout_sample = veletlen<=0.5

merge m:1 id2010 using ../adat/telepules/telepules_listas_arany
drop _m

gen ln_szavazat = ln(szavazat)
reg */
