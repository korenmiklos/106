clear all
use becslopanel

* csak orszagos partok
* szavazatok 96% 
keep if partjeloltek_szama==106

egen nagyok_kozti_rang = rank(-szavazat), by(szavazokor) unique

* osszevonjuk a falvakat megyenkent, felteve, hogy hasonloan szavaznak
gen szintetikus_telepules = cond(szavazokorok_szama<=5,megye+"falu",telepules)
egen telepules_tipus = group(megye meret)
egen telepulesXszervezet = group(szintetikus_telepules szervezet)

su telepules_tipus
local T = r(max)

areg ln_arany ferfi dr listan_is_indul ismert i.telepulesXszervezet, a(szavazokor)
predict Yhat, xbd


reg ln_arany Yhat if rang<=3

/*
      Source |       SS           df       MS      Number of obs   =    31,048
-------------+----------------------------------   F(1, 31046)     =  81064.95
       Model |  5868.62969         1  5868.62969   Prob > F        =    0.0000
    Residual |  2247.54944    31,046  .072394171   R-squared       =    0.7231
-------------+----------------------------------   Adj R-squared   =    0.7231
       Total |  8116.17912    31,047   .26141589   Root MSE        =    .26906

------------------------------------------------------------------------------
    ln_arany |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        Yhat |   .8983735   .0031553   284.72   0.000      .892189     .904558
       _cons |  -.1234706   .0043972   -28.08   0.000    -.1320893   -.1148518
------------------------------------------------------------------------------
*/

egen rang_becsles = rank(-Yhat), by(szavazokor) unique
tab nagyok_kozti_rang rang_becsles

/*
    unique |
   rank of |
(-szavazat |
   )    by |   unique rank of (-Yhat)    by szavazokor
szavazokor |         1          2          3          4 |     Total
-----------+--------------------------------------------+----------
         1 |     8,988      1,223        175          0 |    10,386 
         2 |     1,266      7,241      1,872          5 |    10,384 
         3 |       132      1,910      8,014        315 |    10,371 
         4 |         0         10        310      9,679 |     9,999 
-----------+--------------------------------------------+----------
     Total |    10,386     10,384     10,371      9,999 |    41,140 

*/

gen becsult_szavazat = exp(Yhat)
tempvar sum
egen `sum' = sum(becsult_szavazat), by(szavazokor)
replace becsult_szavazat = int(becsult_szavazat/`sum'*osszes_szavazat)

* illeszkedes oevk szinten
gen str oevk = substr(jelolt,1,7)
collapse (sum) szavazat becsult_szavazat, by(oevk jelolt nev szervezet)
foreach X of var *szavazat {
	egen rang_`X' = rank(-`X'), by(oevk)
}
tab rang_szavazat rang_becsult
/*
   rank of |
(-szavazat |
   )    by |   rank of (-becsult_szavazat)    by oevk
      oevk |         1          2          3          4 |     Total
-----------+--------------------------------------------+----------
         1 |       104          2          0          0 |       106 
         2 |         2        101          3          0 |       106 
         3 |         0          3        102          1 |       106 
         4 |         0          0          1        105 |       106 
-----------+--------------------------------------------+----------
     Total |       106        106        106        106 |       424 
*/

reshape wide rang_szavazat jelolt nev szervezet szavazat becsult_szavazat , i(oevk ) j(rang_becsult_szavazat  )
gen byte sorrend_stimmel = rang_szavazat2 <rang_szavazat3 
tab sorrend_stimmel 

/*
sorrend_sti |
       mmel |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |          3        2.83        2.83
          1 |        103       97.17      100.00
------------+-----------------------------------
      Total |        106      100.00
*/
