clear all
use becslopanel

egen telepules_tipus = group(megye meret)
su telepules_tipus
local T = r(max)

forval t=1/`T' {
	reg ln_arany ln_jeloltek_szama i.szervezet_kod if telepules_tipus==`t'
}
