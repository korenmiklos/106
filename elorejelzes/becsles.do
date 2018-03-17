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

gen reszveteli_arany = ervenyes/valasztopolgarok

local egyeni_valtozok listan_is_indul ferfi dr ismert egyeniben_nyert listan_nyert
foreach X of var `egyeni_valtozok' reszveteli_arany {
	foreach P in fidesz jobbik lmp baloldal {
		gen `P'_X_`X' = `P'*`X'
	}
}
* fix-hatas miatt egyet ki kell dobni
drop fidesz_X_reszveteli_arany


* osszevonjuk a falvakat megyenkent, felteve, hogy hasonloan szavaznak
gen szintetikus_telepules = cond(szavazokorok_szama<=5,megye+"falu",telepules)
gen str budapest_kerulet = substr(telepules,7,2) if megye=="M01"
gen byte buda = inlist(budapest_kerulet,"01","02","11","12","22")

* budat es pestet kulon megyenek tekintjuk
gen str oevk = substr(jelolt,1,7)
egen telepules_tipus = group(megye buda meret)
egen telepulesXszervezet = group(telepules_tipus szervezet)

* telepulesen beluli, de out-of-sample illeszkedes
tempvar veletlen vrang vmax
set seed 123
* szavazokoronkent egy veletlenszam
egen metszet = group(oevk telepules_tipus)
egen metszet_tag = tag(metszet)

egen `veletlen' = mean(cond(szavazokor_tag,uniform(),.)), by(szavazokor)
egen `vrang' = rank(`veletlen'), by(telepules_tipus szervezet)
egen `vmax' = max(`vrang'), by(telepules_tipus)

gen holdout_sample = (`vrang'<= `vmax'*0.33)
tab holdout_sample


su telepules_tipus
local T = r(max)

areg ln_arany `egyeni_valtozok' *_X_reszveteli_arany i.szervezet_kod if !holdout_sample, a(szavazokor)
/*
Linear regression, absorbing indicators         Number of obs     =     20,788
                                                F( 303,  15237)   =     452.86
                                                Prob > F          =     0.0000
                                                R-squared         =     0.9039
                                                Adj R-squared     =     0.8690
                                                Root MSE          =     0.3852

-----------------------------------------------------------------------------------
         ln_arany |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
------------------+----------------------------------------------------------------
  listan_is_indul |   .0633066   .0129597     4.88   0.000     .0379039    .0887093
            ferfi |   .0513491   .0116673     4.40   0.000     .0284798    .0742183
               dr |   .0162755   .0100684     1.62   0.106    -.0034597    .0360107
           ismert |   .1721298    .017472     9.85   0.000     .1378826     .206377
  egyeniben_nyert |   .0521725   .0191356     2.73   0.006     .0146643    .0896806
     listan_nyert |   .0535026   .0115845     4.62   0.000     .0307957    .0762095
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
      Source |       SS           df       MS      Number of obs   =    20,329
-------------+----------------------------------   F(1, 20327)     >  99999.00
       Model |  20077.2375         1  20077.2375   Prob > F        =    0.0000
    Residual |    2750.828    20,327  .135328774   R-squared       =    0.8795
-------------+----------------------------------   Adj R-squared   =    0.8795
       Total |  22828.0655    20,328   1.1229863   Root MSE        =    .36787

------------------------------------------------------------------------------
    ln_arany |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        Yhat |   .9720151   .0025236   385.17   0.000     .9670687    .9769615
       _cons |   -.098314   .0051207   -19.20   0.000    -.1083511    -.088277
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
         1 |     4,243        794        101          0 |     5,138 
         2 |       813      3,269      1,054          2 |     5,138 
         3 |        82      1,066      3,780        203 |     5,131 
         4 |         0          9        203      4,877 |     5,089 
-----------+--------------------------------------------+----------
     Total |     5,138      5,138      5,138      5,082 |    20,496 
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
         1 |        98          8          0          0 |       106 
         2 |         8         84         14          0 |       106 
         3 |         0         14         90          2 |       106 
         4 |         0          0          2        104 |       106 
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
          0 |         14       13.21       13.21
          1 |         92       86.79      100.00
------------+-----------------------------------
      Total |        106      100.00
*/
