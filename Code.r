# Getting Started in Fixed/Random Effects Models Using R.
# Oscar Torres-Reyna, Princeton University.

# Load Required Packages:
  require(foreign); library(foreign)
  require(car); library(car)
  require(gplots); library(gplots)

# Read Data Examples from Web or Local System:
  Panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
  Panel <- read.dta(file = "C:\\Users\\Pooya\\OneDrive\\MyThesis\\MyTutorials\\Panel_Data_using_R\\Data_Examples\\Data.dta")
  Panel <- read.csv(file = "C:\\Users\\Pooya\\OneDrive\\MyThesis\\MyTutorials\\Panel_Data_using_R\\Data_Examples\\Data.csv", header = TRUE, sep = ',')

# Exploring Panel Data:
  coplot(formula = y ~ year | country, data = Panel, type = "l")
  scatterplot(y ~ year | country, data = Panel, boxplot = FALSE, smooth = TRUE, reg.line = FALSE)
  
# Fixed Effects: Plot Heterogeneity across Countries:
  plotmeans(formula = y ~ country, data = Panel, main = "Heterogeineity across Countries")
  
# Fixed Effects: Plot Heterogeneity across Years
  plotmeans(formula = y ~ year, data = Panel, main = "Heterogeineity across Years")

# OLS Regression:
  View(Panel)
  # y ~ x1:
  plot(x = Panel$x1, y = Panel$y, type = "p", xlab = "x", ylab = "y", pch = 19, col = "green")
  OLS <- lm(formula = y ~ x1, data = Panel)
  abline(OLS, lwd = 3, col = "green", lty = 2)
  summary(OLS)
  # y ~ x2:
  par(new = TRUE)
  plot(x = Panel$x2, y = Panel$y, type = "p", xlab = "x", ylab = "y", pch = 19, col = "red")
  OLS <- lm(formula = y ~ x2, data = Panel)
  abline(OLS, lwd = 3, col = "red", lty = 2)
  summary(OLS)
  # y ~ x3:
  par(new = TRUE)
  plot(x = Panel$x3, y = Panel$y, type = "p", xlab = "x", ylab = "y", pch = 19, col = "blue")
  OLS <- lm(formula = y ~ x3, data = Panel)
  abline(OLS, lwd = 3, col = "blue", lty = 2)
  summary(OLS)
  # y ~ x1 + x2 + x3:
  par(new = TRUE)
  OLS <- lm(formula = y ~ x1 + x2 + x3, data = Panel)
  abline(OLS, lwd = 3, col = "black", lty = 2)
  summary(OLS)

# Fixed Effects Using Least Squares Dummy Variable Model:
  fixed.dum <- lm(y ~ x1 + x2 + x3 + factor(country) - 1, data = Panel)
  summary(fixed.dum)


  
  
  
  
  
  
  
  
# Why Intercept Ignore: fixed.dum <- lm(y ~ x1 + factor(country), data = Panel)

yhat <- fixed.dum$fitted.values

scatterplot(yhat ~ Panel$x1 | Panel$country, boxplot = FALSE, xlab = "x1", ylab = "yhat", smooth = FALSE)
abline(lm(Panel$y ~ Panel$x1), lwd = 3, col = "red")

# Comparing OLS vs LSDV model: ----------------------------------------------------------------

apsrtable(ols, fixed.dum, model.names = c("OLS", "OLS_DUM"))

# Fixed effects: n entity-specific intercepts (using plm): ------------------------------------

fixed.individual <- plm(formula = y ~ x1 + x2 + x3, data = Panel, index = c("country", "year"), model = "within", effect = "individual")
summary(fixed.individual)

fixed.time <- plm(formula = y ~ x1 + x2 + x3, data = Panel, index = c("country", "year"), model = "within", effect = "time")
summary(fixed.time)

fixed.twoways <- plm(formula = y ~ x1 + x2 + x3, data = Panel, index = c("country", "year"), model = "within", effect = "twoways")
summary(fixed.twoways)

# Extracts the fixed effects from a plm object: Fixed Effects (constants for each coun --------

fixef(fixed.individual)
fixef(fixed.time)
fixef(fixed.twoways, effect = "time")

# F Test for Individual and/or Time Effects: Testing for fixed effects, null - OLS better -----
# than fixed (If the p-value is < 0.05 then the fixed effects model is a better choice)

pFtest(fixed.individual,ols)
pFtest(fixed.time, ols)
pFtest(fixed.twoways, ols)

# RANDOM-EFFECTS MODEL: -----------------------------------------------------------------------

random <- plm(formula = y ~ x1 + x2 + x3, data = Panel, effect = "individual", model ="random", index = c("country", "year"))
summary(random)

# Fixed or Random: Hausman Test ---------------------------------------------------------------

phtest(fixed.individual, random)

# Testing for Time-Fixed Effects --------------------------------------------------------------
fixed.time.dummy <- plm(formula = y ~ x1 + x2 + x3 + factor(year), data = Panel, model ="within", index = c("country", "year"))
summary(fixed.time.dummy)

# Testing time-fixed effects:  The null is that no time-fixed effects needed ------------------

pFtest(fixed.time.dummy, fixed.individual)

# Lagrange Multiplier Test - time effects (Breusch-Pagan): ------------------------------------

plmtest(fixed.individual, c("time"), type = ("bp"))

# Testing for random effects: Breusch-Pagan Lagrange multiplier (LM) --------------------------

pool <- plm(formula = y ~ x1 + x2 + x3, data = Panel, index = c("country", "year"), effect = "individual", model = "pooling")
summary(pool)

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e.  --------

plmtest(pool, type = c("bp"))

# Testing for cross-sectional dependence/contemporaneous correlation: -------------------------
# using Breusch-Pagan LM test of independence and Pasaran CD test.
# The null hypothesis in the B-P/LM and Pasaran CD tests of independence is that residuals across entities are not correlated.

pcdtest(fixed.individual, test = c("lm"))

pcdtest(fixed.individual, test = c("cd"))

# Testing for serial correlation --------------------------------------------------------------
# The null is that there is not serial correlation:

pbgtest(fixed.individual)

# Testing for unit roots/stationarity ---------------------------------------------------------
# The Dickey-Fuller test to check for stochastic trends
# The null hypothesis is that the series has a unit root (i.e. non-stationary).

Panel.set <- plm.data(Panel, index = c("country", "year"))
adf.test(Panel.set$y, k = 2)

# Testing for heteroskedasticity: -------------------------------------------------------------




