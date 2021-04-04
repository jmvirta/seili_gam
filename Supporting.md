This file contains a short description of the statistical tools used in the analyses.

#### Generalized Additive Mixed Models

The main model family used in the studies is the generalized additive mixed model (GAMM), where the conditional expectation of the response variable Y is modelled as

E(Y|X) = b_0 + f_1(X_1) + ... + f_p(X_p) + b_1 Z_1 + ... + b_q Z_q,

where
* b_0 is the intercept term as in standard linear regression.
* X_1, ..., X_p are predictor variables (such as temperature).
* f_1, ..., f_p are unknown smooth functions (that play the role of the usual regression coefficients in linear regression).
* Z_1, ..., Z_q are random effects (such as the effects of the different years, shared by all days of a given year).
* b_1, ..., b_q are the (known) coefficients of the random effects (such as dummy variables for the years).

After specifying a suitable basis for the smooth functions (the analyses here use a basis of 10 thin plate regression splines), the fixed model parameters b_0, f_1, ..., f_p can be estimated using restricted maximum likelihood.

Approximate p-values for the significance of each of the predictor variables are obtained afterwards, and the predictors' effects on the response can further be studied by plotting the corresponding estimated functions.

Using day of the year as one of the predictors X_j already allows taking the time dependency in the observations into account in the analysis, but additional time information can further be accommodated to the model by modelling the daily differences y_i - y_{i - 1} of the response values instead of the raw values y_i.

#### Model diagnostics

The model fit can be studied as in standard linear regression by plotting the residuals (observations minus predictions) versus observation time. The model can be seen to fit well if the residuals are approximtely randomly distributed (with constant variance) around zero. In particular, the residuals should not exhibit any autocorrelation (manifested in the plot as clear patterns in time).

The presence of autocorrelation can be studied by plotting the partial autocorrelation plot of the residuals. Using partial autocorrelation plot, which removes the effects of the earlier lags from the further autocorrelations, instead of the regular autocorrelation plot helps distinguish on which lags the autocorrelation truly resides. 

In case significant autocorrelation is present, alternative models should be tried. E.g, in the given code, the residuals in the model with the raw responses y_i exhibit strong autocorrelation on lag 1 and, as a remedy, we try two things:

* Incorporating lagged versions of predictors in the model (which does not help, and after which the residual autocorrelation still persists).
* Modelling the differences y_i - y_{i - 1} instead of the raw values y_i (which indeed rids the model of residual autocorrelation and gives a reasonably well fitting model) 
