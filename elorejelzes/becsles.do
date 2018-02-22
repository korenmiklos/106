clear all
use becslopanel

egen telepules_tipus = group(megye meret)
su telepules_tipus
local T = r(max)

tempvar Yhat
gen Yhat=.
forval t=1/`T' {
	capture reg ln_arany ln_jeloltek_szama ln_osszes_szavazat i.szervezet_kod if telepules_tipus==`t'
	if _rc==0 {
		predict `Yhat'
		replace Yhat=`Yhat' if telepules_tipus==`t'
		drop `Yhat'
	}
}

reg ln_arany Yhat if rang<=3
/*

      Source |       SS           df       MS      Number of obs   =     4,326
-------------+----------------------------------   F(1, 4324)      =   7262.39
       Model |  692.191999         1  692.191999   Prob > F        =    0.0000
    Residual |  412.128422     4,324  .095311846   R-squared       =    0.6268
-------------+----------------------------------   Adj R-squared   =    0.6267
       Total |  1104.32042     4,325  .255334201   Root MSE        =    .30873

------------------------------------------------------------------------------
    ln_arany |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        Yhat |    .766615   .0089958    85.22   0.000     .7489787    .7842513
       _cons |  -.2803398   .0127964   -21.91   0.000    -.3054273   -.2552523
------------------------------------------------------------------------------

*/
