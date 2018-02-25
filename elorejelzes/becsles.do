clear all
use becslopanel

* csak orszagos partok
* szavazatok 96% 
keep if partjeloltek_szama==106

* ezt a mintszukites utan ujra kell definialni
drop szavazokor_tag
egen szavazokor_tag = tag(szavazokor)

egen nagyok_kozti_rang = rank(-szavazat), by(szavazokor) unique

gen str part = substr(szervezet,1,3)
gen byte fidesz = part=="FID"
gen byte jobbik = part=="JOB"
gen byte lmp = part=="LMP"
gen byte baloldal = part=="MSZ"

local egyeni_valtozok listan_is_indul ferfi dr ismert egyeniben_nyert listan_nyert
foreach X of var `egyeni_valtozok' {
	foreach P in fidesz jobbik lmp baloldal {
		gen `P'_X_`X' = `P'*`X'
	}
}


* osszevonjuk a falvakat megyenkent, felteve, hogy hasonloan szavaznak
gen szintetikus_telepules = cond(szavazokorok_szama<=5,megye+"falu",telepules)
gen str budapest_kerulet = substr(telepules,7,2) if megye=="M01"
gen byte buda = inlist(budapest_kerulet,"01","02","11","12","22")

* budat es pestet kulon megyenek tekintjuk
gen str oevk = substr(jelolt,1,7)
egen telepules_tipus = group(megye buda meret)
egen telepulesXszervezet = group(oevk telepules_tipus szervezet)

* telepulesen beluli, de out-of-sample illeszkedes
tempvar veletlen vrang
set seed 123
* szavazokoronkent egy veletlenszam
egen `veletlen' = mean(cond(szavazokor_tag,uniform(),.)), by(szavazokor)
egen `vrang' = rank(`veletlen'), by(szintetikus_telepules szervezet )
egen szint_szavazokorok_szama = max(`vrang'), by(szintetikus_telepules)

gen holdout_sample = (`vrang'<= szint_szavazokorok_szama*0.5)
tab holdout_sample


su telepules_tipus
local T = r(max)

areg ln_arany `egyeni_valtozok' i.telepulesXszervezet if !holdout_sample, a(szavazokor)
/*
Linear regression, absorbing indicators         Number of obs     =     20,795
                                                F( 993,  14554)   =     170.83
                                                Prob > F          =     0.0000
                                                R-squared         =     0.9241
                                                Adj R-squared     =     0.8916
                                                Root MSE          =     0.3511

-----------------------------------------------------------------------------------
         ln_arany |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
------------------+----------------------------------------------------------------
  listan_is_indul |  -1.653961   .1655226    -9.99   0.000    -1.978407   -1.329516
            ferfi |  -1.684284   .1655226   -10.18   0.000     -2.00873   -1.359839
               dr |  -1.752771   .0973849   -18.00   0.000    -1.943658   -1.561884
           ismert |   6.065327   .2689002    22.56   0.000     5.538249    6.592406
  egyeniben_nyert |   .1400201   .1058686     1.32   0.186    -.0674958     .347536
     listan_nyert |   4.084798   .2069032    19.74   0.000     3.679242    4.490355
*/
predict Yhat, xb
gen becsult_szavazat = exp(Yhat)
tempvar sum
egen `sum' = sum(becsult_szavazat), by(szavazokor)
replace becsult_szavazat = int(becsult_szavazat/`sum'*osszes_szavazat)
replace Yhat = ln(becsult_szavazat/osszes_szavazat)

reg ln_arany Yhat if holdout_sample
** szavazokor-szinten az out-of-sample illeszkedes is jo
/*
      Source |       SS           df       MS      Number of obs   =    20,292
-------------+----------------------------------   F(1, 20290)     >  99999.00
       Model |  20156.4111         1  20156.4111   Prob > F        =    0.0000
    Residual |  2504.28485    20,290  .123424586   R-squared       =    0.8895
-------------+----------------------------------   Adj R-squared   =    0.8895
       Total |   22660.696    20,291  1.11678557   Root MSE        =    .35132

------------------------------------------------------------------------------
    ln_arany |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        Yhat |   .9631822   .0023834   404.12   0.000     .9585104    .9678539
       _cons |  -.1083725    .004858   -22.31   0.000    -.1178946   -.0988503
------------------------------------------------------------------------------
*/

egen rang_becsles = rank(-Yhat), by(szavazokor) unique
tab nagyok_kozti_rang rang_becsles if holdout_sample

/*
    unique |
   rank of |
(-szavazat |
   )    by |   unique rank of (-Yhat)    by szavazokor
szavazokor |         1          2          3          4 |     Total
-----------+--------------------------------------------+----------
         1 |     4,381        644        109          2 |     5,136 
         2 |       670      3,543        918          2 |     5,133 
         3 |        85        945      3,930        166 |     5,126 
         4 |         2          5        174      4,881 |     5,062 
-----------+--------------------------------------------+----------
     Total |     5,138      5,137      5,131      5,051 |    20,457 
*/


replace becsult_szavazat = . if !holdout_sample

* illeszkedes oevk szinten
collapse (sum) szavazat becsult_szavazat, by(oevk jelolt nev szervezet)
foreach X of var *szavazat {
	egen rang_`X' = rank(-`X'), by(oevk) unique
}
tab rang_szavazat rang_becsult
** oevk szinten is jo az illeszkedes
/*
    unique |
   rank of |
(-szavazat |  unique rank of (-becsult_szavazat)    by
   )    by |                    oevk
      oevk |         1          2          3          4 |     Total
-----------+--------------------------------------------+----------
         1 |       105          1          0          0 |       106 
         2 |         1        103          2          0 |       106 
         3 |         0          2        104          0 |       106 
         4 |         0          0          0        106 |       106 
-----------+--------------------------------------------+----------
     Total |       106        106        106        106 |       424 
*/

egen nyertes_szavazat = mean(cond(rang_szavazat==1,szavazat,.)), by(oevk)
egen osszes_szavazat = sum(szavazat), by(oevk)
gen nyeresi_arany = nyertes_szavazat/osszes_szavazat

reshape wide rang_szavazat jelolt nev szervezet szavazat becsult_szavazat , i(oevk nyeresi_arany) j(rang_becsult_szavazat  )
gen byte sorrend_stimmel = rang_szavazat2 <rang_szavazat3 
tab sorrend_stimmel

/*
sorrend_sti |
       mmel |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |          2        1.89        1.89
          1 |        104       98.11      100.00
------------+-----------------------------------
      Total |        106      100.00
*/
