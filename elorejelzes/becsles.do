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

gen holdout_sample = (`vrang'<= szint_szavazokorok_szama*0.33)
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

reg ln_arany ferfi dr listan_is_indul ismert i.telepulesXszervezet if !holdout_sample
/*
      Source |       SS           df       MS      Number of obs   =    36,654
-------------+----------------------------------   F(1175, 35478)  =    267.47
       Model |  37297.9207     1,175  31.7429112   Prob > F        =    0.0000
    Residual |  4210.45192    35,478  .118677826   R-squared       =    0.8986
-------------+----------------------------------   Adj R-squared   =    0.8952
       Total |  41508.3726    36,653  1.13246863   Root MSE        =     .3445

-----------------------------------------------------------------------------------
         ln_arany |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
------------------+----------------------------------------------------------------
            ferfi |   .0356002   .0100292     3.55   0.000     .0159427    .0552577
               dr |   .0462951   .0080439     5.76   0.000     .0305289    .0620614
  listan_is_indul |   .0201582   .0101792     1.98   0.048     .0002067    .0401096
           ismert |   .1504618   .0170022     8.85   0.000     .1171371    .1837866
*/
predict Yhat, xb

reg ln_arany Yhat if holdout_sample
** szavazokor-szinten jo out-of-sample illeszkedes
/*
      Source |       SS           df       MS      Number of obs   =     4,486
-------------+----------------------------------   F(1, 4484)      =  37847.77
       Model |  4382.45245         1  4382.45245   Prob > F        =    0.0000
    Residual |  519.209332     4,484  .115791555   R-squared       =    0.8941
-------------+----------------------------------   Adj R-squared   =    0.8941
       Total |  4901.66178     4,485  1.09290118   Root MSE        =    .34028

------------------------------------------------------------------------------
    ln_arany |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        Yhat |   1.008251   .0051826   194.55   0.000     .9980902    1.018411
       _cons |   .0093727   .0105921     0.88   0.376     -.011393    .0301384
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
         1 |     1,003        131          9          0 |     1,143 
         2 |       122        780        220          1 |     1,123 
         3 |         8        215        875         42 |     1,140 
         4 |         0          3         33      1,090 |     1,126 
-----------+--------------------------------------------+----------
     Total |     1,133      1,129      1,137      1,133 |     4,532 
*/

gen becsult_szavazat = exp(Yhat)
tempvar sum
egen `sum' = sum(becsult_szavazat), by(szavazokor)
replace becsult_szavazat = int(becsult_szavazat/`sum'*osszes_szavazat)

replace becsult_szavazat = . if !holdout_sample

* illeszkedes oevk szinten
gen str oevk = substr(jelolt,1,7)
collapse (sum) szavazat becsult_szavazat, by(oevk jelolt nev szervezet)
foreach X of var *szavazat {
	egen rang_`X' = rank(-`X'), by(oevk) unique
}
tab rang_szavazat rang_becsult
** oevk szinten sokkal rosszabb illeszkedes
/*
    unique |
   rank of |
(-szavazat |  unique rank of (-becsult_szavazat)    by
   )    by |                    oevk
      oevk |         1          2          3          4 |     Total
-----------+--------------------------------------------+----------
         1 |        83         10          6          7 |       106 
         2 |         9         58         30          9 |       106 
         3 |        10         30         59          7 |       106 
         4 |         4          8         11         83 |       106 
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
          0 |         35       33.02       33.02
          1 |         71       66.98      100.00
------------+-----------------------------------
      Total |        106      100.00
*/
