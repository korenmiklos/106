* megye listak, mindig mashogy irjak a partok nevet
gen     partnev = "fidesz" if substr(strlower(szervezet),1,6)=="fidesz"
replace partnev = "jobbik" if substr(strlower(szervezet),1,6)=="jobbik"
replace partnev = "lmp" if inlist(substr(strlower(szervezet),1,5),"lehet","lmp")
replace partnev = "mszp" if inlist(substr(strlower(szervezet),1,18),"magyar szocialista","mszp")
replace partnev = "mdf" if inlist(substr(strlower(szervezet),1,18),"magyar demokrata f","mdf")
replace partnev = "baloldal" if substr(strlower(szervezet),1,8)=="mszp-egy"
replace partnev = "egyeb" if missing(partnev)

replace partnev = "baloldal" if partnev=="mszp" | substr(strlower(szervezet),1,4)=="munk"

