tempfile korzetek
import delimited erzekeny_korzetek.txt, clear varnames(1) encoding("utf-8")
save `korzetek'

import delimited telepules_szintu_listas_elorejelzes.csv, clear varnames(1) encoding("utf-8")
merge m:1 oevk using `korzetek', nogen keep(match)

gen str location = telepules_nev + ", Hungary"

egen i = group(oevk)
su i
local N=r(max)

gen u =  uniform()
sort i u
forval i=1/`N' {
		* save a random half of cities so that treatment can be evaluated
		export delimited location if (i==`i')&(int(_n/2)*2==_n) using facebook`i'.csv, replace
}
