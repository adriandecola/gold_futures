****downloading data(as downloaded from fred)***
import delimited "C:\Users\adecola\Downloads\fredgraph.csv", clear

***cleaning the data***
*renaming variables
rename (date vixcls cpilfesl goldamgbd228nlbm) (old_date vol cpi gold)
*we don't yet have information on cpi for March 2021
drop if old_date== "2021-03-01"
*labeling variables
lab var old_date "old date variable(string)"
lab var vol "volatility index"
lab var cpi "consumer price index"
lab var gold "monthly average gold price index"


*creating new time variable and setting it
gen time= _n
tsset time
*creating inflation variable
gen lncpi = ln(cpi)
gen dlncpi = d.lncpi


****************************************ARIMA************************************
*getting an idea of the gold index
tsline gold
*doesn't look stationary, graphing autocorrelation funciton
ac gold
*doesn't look stationaty-autocorrelations presist too long, lets test
dfuller gold
*fail to reject
*lets try detrending a linear trend
reg gold time, robust
predict gold_lintrend, xb
predict gold_wo_lintrend, residuals
*looking at the linear trend
tsline gold gold_lintrend
*looking at the detrended series
tsline gold_wo_lintrend
*doesn't really look stationary-lets test stationarity of gold without linear trend
dfuller gold_wo_lintrend
*still fail to reject

*let's try an HP filter. This is monthly data, so our \lambda needs to be 14400
tsfilter hp gold_hp=gold, trend(gold_hptrend) smooth(14400)
*looking at the trend
tsline gold gold_hptrend
*looking at the detrended series
tsline gold_hp
*looks to revert back to zero more and therefore could be stationary, let's test
dfuller gold_hp
*can reject on the left side- we have a stationary series

*lets instead try differentiating-as this is financial data
gen dgold = d.gold
*lets look at the differentiated series
tsline dgold
*deffinetly looks stationary- lets test
dfuller dgold
*can reject at a greater significance level than with HP filter. Lets continue with the differentiated series

*looking at the autocorrelation function
ac dgold
*the first autocorrelation is definetley significant, but its hard to tell from there
*lets use the varsoc command to see what we might assume if we force only an ar process on the differentiated series
varsoc dgold
*points towards an AR(1),this is financial data so lets look at enough lags for 2 years
varsoc dgold, maxlag(24)
*we get either an AR(1) or AR(11). An ar(11) seems less intuitive, I would rather stick with an AR(1); however, it would be silly not to include MA processes. 

*This is a nested for loop that tests every cumulative ARIMA Process. I first did it going from to 12 for each AR and MA, however, we get the same results and that for loop takes forever to run. By using the estat ic command the forloop changes the AR and MA for each AIC and BIC if the ARIMA process creates generates a smaller score for each respective criteria
*initialization
gen bestiAIC = .
gen bestiBIC = .
gen bestjAIC = .
gen bestjBIC = .
gen smallestAIC = .
gen smallestBIC = .
*nested for loop
foreach i of numlist 0/6 { 
foreach j of numlist 0/6 { 
arima gold, arima(`i',1,`j') nocons
estat ic
replace bestiAIC = `i' if r(S)[1,5]<smallestAIC
replace bestjAIC = `j' if r(S)[1,5]<smallestAIC
replace bestiBIC = `i' if r(S)[1,6]<smallestBIC
replace bestjBIC = `j' if r(S)[1,6]<smallestBIC
replace smallestAIC = r(S)[1,5] if r(S)[1,5]<smallestAIC
replace smallestBIC = r(S)[1,6] if r(S)[1,6]<smallestBIC
}
}
*based on the results the AIC gives us that and arima(5,1,5) is the best model; however the BIC gives us that a simple arima(0,1,1) is the best process. We will contiune with Baysian Information Criterion. We can assume a sort of momentum then the changes in gold price index. 
*allowing for some correction through mean reversion
arima gold, arima(0,1,1) nocons
estat ic
arima dgold, ma(1,3) nocons
estat ic
arima dgold, ma(1,4) nocons
estat ic
arima dgold, ma(1,5) nocons
estat ic
*none provide a lower baysian information criterion score. Contiue with ARIMA(0,1,1)
arima gold, arima (0,1,1) nocons
predict arma0_1_errors, residuals
ac arma0_1_errors
*This looks like white noise. Let's try forecasting
predict farma0_1, dy(250)
*predicting standard errors
predict farma0_1_error, mse dynamic(250)
*creating a 95% confidence level
gen upper_farma0_1 = farma0_1 + 1.96*sqrt(farma0_1_error)
gen lower_farma0_1 = farma0_1 - 1.96*sqrt(farma0_1_error)
tsline dgold farma0_1 upper_farma0_1 lower_farma0_1 if time>=230 & time<=260

*************************VAR Process**********************
*viewing our data(on different graphs because scales are vastly different)
tsline dlncpi vol dgold
tsline dlncpi
tsline vol
tsline dgold

*Is vol stationary
dfuller vol
*yes it is stationanary
*is inflation stationary
dfuller dlncpi
*yes its stationary, from the graph it definetley looks like theres a constant.
*finding best cumulative ar process for dgold
varsoc dgold, maxlag(24)
*choose one lag, same as before
*checking that errors look like white noise
arima dgold, ar(1)
predict ar1_errors, resid
ac ar1_errors
var dgold dlncpi vol, lags(1)
***irf
irf create order1, set(irf_dgold) step(15) order(dlncpi vol dgold) replace
irf graph irf
*fixing scales
irf graph irf, impulse(vol) response(dgold)
irf graph irf, impulse(dlncpi) response(dgold)
irf graph irf, impulse(dgold) response(dgold)
*still hard to interpret the units, we can't easily relate this with the variance of the impulse variables and of dgold
*lets try orthogonalizing the vars
irf graph oirf, impulse(vol) response(dgold)
irf graph oirf, impulse(dlncpi) response(dgold)
irf graph oirf, impulse(dgold) response(dgold)
* can see relative effects; however you can't really see the effect through time-- lets make cummulative
irf graph coirf, impulse(vol) response(dgold)
irf graph coirf, impulse(dlncpi) response(dgold)
irf graph coirf, impulse(dgold) response(dgold)
irf table fevd
*lets try switching the order of the two exogenous variables to make sure that it makes sense
irf create order2, set(irf_dgold2) step(15) order(vol dlncpi dgold) replace
irf graph coirf, impulse(vol) response(dgold)
irf graph coirf, impulse(dlncpi) response(dgold)
*results look the same

*summarizing variables and transformed variables
sum gold dgold vol cpi dlncpi ar1_errors gold_hp gold_wo_lintrend arma0_1_errors