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

* telepulesen beluli, de out-of-sample illeszkedes
tempvar veletlen vrang
set seed 123
* szavazokoronkent egy veletlenszam
egen `veletlen' = mean(cond(szavazokor_tag,uniform(),.)), by(szavazokor)
egen `vrang' = rank(uniform()), by(szintetikus_telepules szervezet)
egen szint_szavazokorok_szama = sum(szavazokor_tag), by(szintetikus_telepules)

gen holdout_sample = (`vrang'<= szint_szavazokorok_szama*0.70)
tab holdout_sample
/*
holdout_sam |
        ple |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |     37,012       89.09       89.09
          1 |      4,532       10.91      100.00
------------+-----------------------------------
      Total |     41,544      100.00

*/

su telepules_tipus
local T = r(max)

areg ln_arany ferfi dr listan_is_indul ismert egyeniben_nyert listan_nyert i.telepulesXszervezet if !holdout_sample, a(szavazokor)
/*
Linear regression, absorbing indicators         Number of obs     =     31,270
                                                F( 881,  20445)   =     248.85
                                                Prob > F          =     0.0000
                                                R-squared         =     0.9253
                                                Adj R-squared     =     0.8858
                                                Root MSE          =     0.3616

-----------------------------------------------------------------------------------
         ln_arany |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
------------------+----------------------------------------------------------------
            ferfi |   .0554165    .014169     3.91   0.000     .0276441    .0831889
               dr |   .0397268   .0116573     3.41   0.001     .0168776    .0625759
  listan_is_indul |   .0352629   .0144267     2.44   0.015     .0069855    .0635403
           ismert |   .1375403   .0232303     5.92   0.000     .0920071    .1830736
  egyeniben_nyert |    .085699   .0214345     4.00   0.000     .0436856    .1277125
     listan_nyert |   .0238007   .0131278     1.81   0.070    -.0019308    .0495321
*/
predict Yhat, xb
gen becsult_szavazat = exp(Yhat)
tempvar sum
egen `sum' = sum(becsult_szavazat), by(szavazokor)
replace becsult_szavazat = int(becsult_szavazat/`sum'*osszes_szavazat)
replace Yhat = ln(becsult_szavazat/osszes_szavazat)

reg ln_arany Yhat if holdout_sample
** szavazokor-szinten jo out-of-sample illeszkedes
/*
      Source |       SS           df       MS      Number of obs   =     9,865
-------------+----------------------------------   F(1, 9863)      =  74554.72
       Model |  9362.01246         1  9362.01246   Prob > F        =    0.0000
    Residual |  1238.52031     9,863  .125572373   R-squared       =    0.8832
-------------+----------------------------------   Adj R-squared   =    0.8832
       Total |  10600.5328     9,864  1.07466877   Root MSE        =    .35436

------------------------------------------------------------------------------
    ln_arany |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        Yhat |   .9596309   .0035145   273.05   0.000     .9527417    .9665201
       _cons |  -.1107614   .0071263   -15.54   0.000    -.1247304   -.0967925
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
         1 |     2,182        285         29          0 |     2,496 
         2 |       292      1,681        518          2 |     2,493 
         3 |        19        512      1,857         89 |     2,477 
         4 |         0          4         91      2,382 |     2,477 
-----------+--------------------------------------------+----------
     Total |     2,493      2,482      2,495      2,473 |     9,943 
*/


replace becsult_szavazat = . if !holdout_sample

* illeszkedes oevk szinten
gen str oevk = substr(jelolt,1,7)
collapse (sum) szavazat becsult_szavazat, by(oevk jelolt nev szervezet)
foreach X of var *szavazat {
	egen rang_`X' = rank(-`X'), by(oevk) unique
}
tab rang_szavazat rang_becsult
** oevk szinten rosszabb illeszkedes
/*
    unique |
   rank of |
(-szavazat |  unique rank of (-becsult_szavazat)    by
   )    by |                    oevk
      oevk |         1          2          3          4 |     Total
-----------+--------------------------------------------+----------
         1 |        88          5         10          3 |       106 
         2 |         6         74         20          6 |       106 
         3 |         7         22         70          7 |       106 
         4 |         5          5          6         90 |       106 
-----------+--------------------------------------------+----------
     Total |       106        106        106        106 |       424 
*/

egen nyertes_szavazat = mean(cond(rang_szavazat==1,szavazat,.)), by(oevk)
egen osszes_szavazat = sum(szavazat), by(oevk)
gen nyeresi_arany = nyertes_szavazat/osszes_szavazat

reshape wide rang_szavazat jelolt nev szervezet szavazat becsult_szavazat , i(oevk nyeresi_arany) j(rang_becsult_szavazat  )
gen byte sorrend_stimmel = rang_szavazat2 <rang_szavazat3 
tab sorrend_stimmel  if nyeresi_arany<=0.5

/*
sorrend_sti |
       mmel |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |         23       29.11       29.11
          1 |         56       70.89      100.00
------------+-----------------------------------
      Total |         79      100.00

*/
