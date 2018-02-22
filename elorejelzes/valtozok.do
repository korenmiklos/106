clear all
local files jelolt szervezet egyeni listas

tempfile `files'
foreach fn in `files' {
	import delimited ../adat/jelolt/`fn'.csv, clear varnames(1) encoding("utf-8")
	save ``fn''
}

use `egyeni'

gen str telepules = substr(szavazokor,1,8)
gen str megye = substr(szavazokor,1,3)

gen id = jelolt
merge m:1 id using `jelolt'
drop _m

foreach X in jelolt telepules szavazokor szervezet {
	egen `X'_tag = tag(`X')
	egen `X'_kod = group(`X')
}
egen szavazokorok_szama = sum(szavazokor_tag), by(telepules)
egen partjeloltek_szama = sum(jelolt_tag), by(szervezet)
egen jeloltek_szama = sum(jelolt_tag), by(szavazokor)
egen osszes_szavazat = sum(szavazat), by(szavazokor)
gen arany = szavazat/osszes_szavazat
egen rang = rank(-szavazat), by(szavazokor) unique

foreach X of var szavazat arany jeloltek_szama osszes_szavazat {
	gen ln_`X' = ln(`X')
}

gen meret = szavazokorok_szama
recode meret 2/5=2 6/10=3 11/30=4 31/max=5

save becslopanel, replace
