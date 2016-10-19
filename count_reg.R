### Poisson Regression example in Section 3.1 in Faraway's book

library(faraway)
data(gala)
# The data is from Johnson and Raven (1973) and Weisberg (2005). 
# There are 30 Galapagos islands, each with different geological variables.
# Consider the relationship between the number of species (counts) and these geological variables.

# ?gala  																# Data description
gala
# Species    Area Elevation Nearest Scruz Adjacent
# Baltra            58   25.09       346     0.6   0.6     1.84
# Bartolome         31    1.24       109     0.6  26.3   572.33
# Caldwell           3    0.21       114     2.8  58.7     0.78
# Champion          25    0.10        46     1.9  47.4     0.18
# Coamano            2    0.05        77     1.9   1.9   903.82
# Daphne.Major      18    0.34       119     8.0   8.0     1.84
# Daphne.Minor      24    0.08        93     6.0  12.0     0.34
# Darwin            10    2.33       168    34.1 290.2     2.85
# Eden               8    0.03        71     0.4   0.4    17.95
# Enderby            2    0.18       112     2.6  50.2     0.10
# Espanola          97   58.27       198     1.1  88.3     0.57
# Fernandina        93  634.49      1494     4.3  95.3  4669.32
# Gardner1          58    0.57        49     1.1  93.1    58.27
# Gardner2           5    0.78       227     4.6  62.2     0.21
# Genovesa          40   17.35        76    47.4  92.2   129.49
# Isabela          347 4669.32      1707     0.7  28.1   634.49
# Marchena          51  129.49       343    29.1  85.9    59.56
# Onslow             2    0.01        25     3.3  45.9     0.10
# Pinta            104   59.56       777    29.1 119.6   129.49
# Pinzon           108   17.95       458    10.7  10.7     0.03
# Las.Plazas        12    0.23        94     0.5   0.6    25.09
# Rabida            70    4.89       367     4.4  24.4   572.33
# SanCristobal     280  551.62       716    45.2  66.6     0.57
# SanSalvador      237  572.33       906     0.2  19.8     4.89
# SantaCruz        444  903.82       864     0.6   0.0     0.52
# SantaFe           62   24.08       259    16.5  16.5     0.52
# SantaMaria       285  170.92       640     2.6  49.2     0.10
# Seymour           44    1.84       147     0.6   9.6    25.09
# Tortuga           16    1.24       186     6.8  50.9    17.95
# Wolf              21    2.85       253    34.1 254.7     2.33   
gala <- gala[,-2]

modl <- lm(Species ~ . , gala)										# try linear regression
summary(modl)

plot(predict(modl),residuals(modl),xlab="Fitted",ylab="Residuals")	# non-constant variance

modt <- lm(sqrt(Species) ~ . , gala)								# sqrt transform
summary(modt)



plot(predict(modt),residuals(modt),xlab="Fitted",ylab="Residuals")	# better fit

modp <- glm(Species ~ .,family=poisson, gala)
summary(modp)

# Call:
# glm(formula = Species ~ ., family = poisson, data = gala)

# Deviance Residuals: 
# Min       1Q   Median       3Q      Max  
# -8.2752  -4.4966  -0.9443   1.9168  10.1849  

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  3.155e+00  5.175e-02  60.963  < 2e-16 ***
# Area        -5.799e-04  2.627e-05 -22.074  < 2e-16 ***
# Elevation    3.541e-03  8.741e-05  40.507  < 2e-16 ***
# Nearest      8.826e-03  1.821e-03   4.846 1.26e-06 ***
# Scruz       -5.709e-03  6.256e-04  -9.126  < 2e-16 ***
# Adjacent    -6.630e-04  2.933e-05 -22.608  < 2e-16 ***
# ---
# Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

# (Dispersion parameter for poisson family taken to be 1)

# Null deviance: 3510.73  on 29  degrees of freedom
# Residual deviance:  716.85  on 24  degrees of freedom
# AIC: 889.68

# Number of Fisher Scoring iterations: 5


plot(log(fitted(modp)),log((gala$Species-fitted(modp))^2),
     xlab=expression(hat(mu)),ylab=expression((yhat(mu))^2),xlim=c(0,10),ylim=c(0,10))
abline(0,1)

dp <-sum(residuals(modp,type="pearson")^2)/modp$df.res;dp#?

summary(modp, dispersion=dp)						# No effect on slope coefficients due to 
# orthogonality but SE estimates are higher
# Call:
# glm(formula = Species ~ ., family = poisson, data = gala)

# Deviance Residuals: 
# Min       1Q   Median       3Q      Max  
# -8.2752  -4.4966  -0.9443   1.9168  10.1849  

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  3.1548079  0.2915897  10.819  < 2e-16 ***
# Area        -0.0005799  0.0001480  -3.918 8.95e-05 ***
# Elevation    0.0035406  0.0004925   7.189 6.53e-13 ***
# Nearest      0.0088256  0.0102621   0.860    0.390    
# Scruz       -0.0057094  0.0035251  -1.620    0.105    
# Adjacent    -0.0006630  0.0001653  -4.012 6.01e-05 ***
# ---
# Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

# (Dispersion parameter for poisson family taken to be 31.74914)

# Null deviance: 3510.73  on 29  degrees of freedom
# Residual deviance:  716.85  on 24  degrees of freedom
# AIC: 889.68

# Number of Fisher Scoring iterations: 5



### Rate Regression example in Section 3.2 in Faraway's book

library(faraway)
data(dicentric)
# Purott and Reeder (1976) on he effect of gamma radiation 
# on the numbers of chromosomal abnormalities
# ?dicentric
dicentric							# Note that variable "cells" represents the number of hundreds of cells
# cells  ca doseamt doserate
# 1    478  25     1.0     0.10
# 2   1907 102     1.0     0.25
# 3   2258 149     1.0     0.50
# 4   2329 160     1.0     1.00
# 5   1238  75     1.0     1.50
# 6   1491 100     1.0     2.00
# 7   1518  99     1.0     2.50
# 8    764  50     1.0     3.00
# 9   1367 100     1.0     4.00
# 10   328  52     2.5     0.10
# 11   185  51     2.5     0.25
# 12   342 100     2.5     0.50
# 13   310 100     2.5     1.00
# 14   278 107     2.5     1.50
# 15   259 107     2.5     2.00
# 16   249 102     2.5     2.50
# 17   298 110     2.5     3.00
# 18   243 107     2.5     4.00
# 19   210 100     5.0     0.10
# 20   138 113     5.0     0.25
# 21   160 144     5.0     0.50
# 22   120 106     5.0     1.00
# 23    90 111     5.0     1.50
# 24   100 132     5.0     2.00
# 25   313 419     5.0     2.50
# 26   182 225     5.0     3.00
# 27   144 206     5.0     4.00

round(xtabs(ca/cells ~ doseamt+doserate,dicentric),2)
# doserate
# doseamt  0.1 0.25  0.5    1  1.5    2  2.5    3    4
# 1   0.05 0.05 0.07 0.07 0.06 0.07 0.07 0.07 0.07
# 2.5 0.16 0.28 0.29 0.32 0.38 0.41 0.41 0.37 0.44
# 5   0.48 0.82 0.90 0.88 1.23 1.32 1.34 1.24 1.43

lmod <- lm(ca/cells ~ log(doserate)*factor(doseamt),dicentric)
summary(lmod)

# Call:
# lm(formula = ca/cells ~ log(doserate) * factor(doseamt), data = dicentric)

# Residuals:
# Min        1Q    Median        3Q       Max 
# -0.184275 -0.004212  0.001314  0.021208  0.089076 

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.063489   0.019528   3.251  0.00382 ** 
# log(doserate)                    0.004573   0.016692   0.274  0.78680    
# factor(doseamt)2.5               0.276315   0.027616  10.005 1.92e-09 ***
# factor(doseamt)5                 1.004119   0.027616  36.359  < 2e-16 ***
# log(doserate):factor(doseamt)2.5 0.063933   0.023606   2.708  0.01317 *  
# log(doserate):factor(doseamt)5   0.239129   0.023606  10.130 1.54e-09 ***
# ---
# Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

# Residual standard error: 0.05858 on 21 degrees of freedom
# Multiple R-squared:  0.9874,    Adjusted R-squared:  0.9844 
# F-statistic:   330 on 5 and 21 DF,  p-value: < 2.2e-16

plot(residuals(lmod) ~fitted(lmod),xlab="Fitted",ylab="Residuals")			# non-constant variance
abline(h=0)

dicentric$dosef <- factor(dicentric$doseamt)
pmod <- glm(ca ~ log(cells)+log(doserate)*dosef, family=poisson,dicentric)	# interested in multiplicative effect
summary(pmod)

# Call:
# glm(formula = ca ~ log(cells) + log(doserate) * dosef, family = poisson, 
# data = dicentric)

# Deviance Residuals: 
# Min        1Q    Median        3Q       Max  
# -1.49901  -0.62229  -0.05021   0.76919   1.59525  

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -2.76534    0.38116  -7.255 4.02e-13 ***
# log(cells)              1.00252    0.05137  19.517  < 2e-16 ***
# log(doserate)           0.07200    0.03547   2.030 0.042403 *  
# dosef2.5                1.62984    0.10273  15.866  < 2e-16 ***
# dosef5                  2.76673    0.12287  22.517  < 2e-16 ***
# log(doserate):dosef2.5  0.16111    0.04837   3.331 0.000866 ***
# log(doserate):dosef5    0.19316    0.04299   4.493 7.03e-06 ***
# ---
# Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

# (Dispersion parameter for poisson family taken to be 1)

# Null deviance: 916.127  on 26  degrees of freedom
# Residual deviance:  21.748  on 20  degrees of freedom
# AIC: 211.15

# Number of Fisher Scoring iterations: 4



rmod <- glm(ca ~ offset(log(cells))+log(doserate)*dosef,	# offset the coefficient of log(cells) to be 1
            family=poisson,dicentric)
summary(rmod)

# Call:
# glm(formula = ca ~ offset(log(cells)) + log(doserate) * dosef, 
# family = poisson, data = dicentric)

# Deviance Residuals: 
# Min        1Q    Median        3Q       Max  
# -1.49101  -0.62473  -0.05078   0.76786   1.59115  

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -2.74671    0.03426 -80.165  < 2e-16 ***
# log(doserate)           0.07178    0.03518   2.041 0.041299 *  
# dosef2.5                1.62542    0.04946  32.863  < 2e-16 ***
# dosef5                  2.76109    0.04349  63.491  < 2e-16 ***
# log(doserate):dosef2.5  0.16122    0.04830   3.338 0.000844 ***
# log(doserate):dosef5    0.19350    0.04243   4.561  5.1e-06 ***
# ---
# Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

# (Dispersion parameter for poisson family taken to be 1)

# Null deviance: 4753.00  on 26  degrees of freedom
# Residual deviance:   21.75  on 21  degrees of freedom
# AIC: 209.16

# Number of Fisher Scoring iterations: 4

### Negative Binomial Regression example in Section 3.3 in Faraway's book
data(solder)
# five factors relevant to a wave-soldering procedure for mounting components on printed circuit boards. 
# The response variable, skips, is a count of how many solder skips appeared to a visual inspection.
tail(solder)
# Opening Solder Mask PadType Panel skips
# 895       S   Thin   B6      W9     1    13
# 896       S   Thin   B6      W9     2    21
# 897       S   Thin   B6      W9     3    15
# 898       S   Thin   B6      L9     1    11
# 899       S   Thin   B6      L9     2    33
# 900       S   Thin   B6      L9     3    15


modp <- glm(skips ~ . , family=poisson, data=solder)
summary(modp)

# Call:
# glm(formula = skips ~ ., family = poisson, data = solder)

# Deviance Residuals: 
# Min       1Q   Median       3Q      Max  
# -3.6413  -1.2562  -0.5286   0.5670   4.7289  

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.10042    0.09267 -11.874  < 2e-16 ***
# OpeningM     0.57161    0.05707  10.017  < 2e-16 ***
# OpeningS     1.81475    0.05044  35.980  < 2e-16 ***
# SolderThin   0.84682    0.03327  25.453  < 2e-16 ***
# MaskA3       0.51315    0.07098   7.230 4.83e-13 ***
# MaskA6       1.81103    0.06609  27.404  < 2e-16 ***
# MaskB3       1.20225    0.06697  17.953  < 2e-16 ***
# MaskB6       1.86648    0.06310  29.580  < 2e-16 ***
# PadTypeD6   -0.40328    0.06028  -6.690 2.24e-11 ***
# PadTypeD7   -0.08979    0.05521  -1.626 0.103852    
# PadTypeL4    0.22460    0.05117   4.389 1.14e-05 ***
# PadTypeL6   -0.70633    0.06637 -10.642  < 2e-16 ***
# PadTypeL7   -0.48260    0.06176  -7.814 5.52e-15 ***
# PadTypeL8   -0.30778    0.05862  -5.251 1.51e-07 ***
# PadTypeL9   -0.63793    0.06489  -9.831  < 2e-16 ***
# PadTypeW4   -0.19021    0.05671  -3.354 0.000796 ***
# PadTypeW9   -1.56252    0.09165 -17.048  < 2e-16 ***
# Panel        0.14670    0.01745   8.405  < 2e-16 ***
# ---
# Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1

# (Dispersion parameter for poisson family taken to be 1)

# Null deviance: 8788.2  on 899  degrees of freedom
# Residual deviance: 1829.0  on 882  degrees of freedom
# AIC: 3967.6

# Number of Fisher Scoring iterations: 5

deviance(modp); df.residual(modp)		
pchisq(deviance(modp),df.residual(modp),lower=FALSE) # Overdispersion

modp2 <- glm(skips ~ (Opening +Solder + Mask + PadType + Panel)^2 , 
             family=poisson, data=solder)			

deviance(modp2); df.residual(modp2)
pchisq(deviance(modp2),df.residual(modp2),lower=FALSE) # Still overdispersion

library(MASS)
modn <- glm(skips ~ . , negative.binomial(1),solder) # NB with link k=1
summary(modn)							# Less overdispersion

modn <- glm.nb(skips ~ .,solder)
summary (modn)
