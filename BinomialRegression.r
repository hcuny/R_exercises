### Binomial Regression Fitting example in Section 2.2 in Faraway's book

library(faraway)
data(orings)
# The 1986 crash of the space shuttle Challenger was linked to failure of O-ring seals in the rocket engines.
# Data on the oring damage was collected on the 23 previous shuttle missions.
# Temperature is suspected to be the major effect.

orings
   # temp damage
# 1    53      5
# 2    57      1
# 3    58      1
# 4    63      1
# 5    66      0
# 6    67      0
# 7    67      0
# 8    67      0
# 9    68      0
# 10   69      0
# 11   70      1
# 12   70      0
# 13   70      1
# 14   70      0
# 15   72      0
# 16   73      0
# 17   75      0
# 18   75      1
# 19   76      0
# 20   76      0
# 21   78      0
# 22   79      0
# 23   81      0


plot (damage/6 ~ temp, orings, xlim=c(25,85), ylim =c(0,1),
xlab="Temperature", ylab="Prob of damage")


logitmod <- glm(cbind(damage,6-damage) ~ temp,		# Binomial regression with logit link
family=binomial, orings)
summary(logitmod)


x <- seq(25,85,1)
lines(x,ilogit(11.6630-0.2162*x))					# ilogit of eta is mu.hat
ilogit (11.6630-0.2162*31)							# Prediction of probability of damage with temp31
# [1] 0.9930414

probitmod <- glm(cbind(damage,6-damage) ~ temp,		# Binoial regression with probit link
family=binomial(link=probit), orings)
summary(probitmod)


lines(x, pnorm(5.5915-0.1058*x), lty=2)				# pnorm of eta is mu.hat
pnorm(5.5915-0.1058*31)								# Prediction of probability of damage
# [1] 0.9896029

### Binomial Regression Inference example in Section 2.3 in Faraway's book

pchisq(logitmod$null.deviance,logitmod$df.null,lower=FALSE)							# Chi-square test for the null
# [1] 0.01448877

pchisq(deviance(logitmod),							# Chi-square test for the logit model
df.residual(logitmod),lower=FALSE)
# [1] 0.7164099

c(-0.2162-1.96*0.0532,-0.2162+1.96*0.0532)			# Conventional CI for the slope
# [1] -0.320472 -0.111928

library(MASS)
confint(logitmod)									# Profile likelihood CI for the slope
# Waiting for profiling to be done...
                # 2.5 %    97.5 %
# (Intercept)  5.575195 18.737598
# temp        -0.332657 -0.120179

cooks.distance(logitmod)							# Influence of the observations
           # 1            2            3            4            5            6 
# 1.9805322113 0.1850353703 0.0763882815 0.0054176778 0.0179753627 0.0136862974 
           # 7            8            9           10           11           12 
# 0.0136862974 0.0136862974 0.0104386557 0.0079406759 0.1238952441 0.0060086873 
          # 13           14           15           16           17           18 
# 0.1238952441 0.0060086873 0.0033706435 0.0024973300 0.0013431691 0.3026332813 
          # 19           20           21           22           23 
# 0.0009759435 0.0009759435 0.0005070242 0.0003628629 0.0001835990

### Binomial Regression Interpretation example in Section 2.5 in Faraway's book

data(babyfood)
# Payne (1987) on infant respiratory disease, namely the proportions of children developing bronchitis 
# or pneumonia in their first year of life by type of feeding and sex.
# 6 treatments.

babyfood


xtabs(disease/(disease+nondisease)~sex+food,
babyfood)
      # food
# sex        Bottle     Breast      Suppl
  # Boy  0.16812227 0.09514170 0.12925170
  # Girl 0.12500000 0.06681034 0.12598425


mdl <- glm(cbind(disease, nondisease) ~ sex+food,	# Binomial regression with logit link
family=binomial,babyfood)
summary(mdl)

# Call:
# glm(formula = cbind(disease, nondisease) ~ sex + food, family = binomial, 
    # data = babyfood)

# Deviance Residuals: 
      # 1        2        3        4        5        6  
 # 0.1096  -0.5052   0.1922  -0.1342   0.5896  -0.2284  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.6127     0.1124 -14.347  < 2e-16 ***
# sexGirl      -0.3126     0.1410  -2.216   0.0267 *  
# foodBreast   -0.6693     0.1530  -4.374 1.22e-05 ***
# foodSuppl    -0.1725     0.2056  -0.839   0.4013    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

    # Null deviance: 26.37529  on 5  degrees of freedom
# Residual deviance:  0.72192  on 2  degrees of freedom
# AIC: 40.24

# Number of Fisher Scoring iterations: 4


drop1(mdl,test="Chi")								# Both factors are significant
# Single term deletions

# Model:
# cbind(disease, nondisease) ~ sex + food
       # Df Deviance    AIC     LRT  Pr(>Chi)    
# <none>      0.7219 40.240                      
# sex     1   5.6990 43.217  4.9771   0.02569 *  
# food    2  20.8992 56.417 20.1772 4.155e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

exp(-0.669)											# Interpretation: breast feeding reduces the odds of 
# [1] 0.5122205										# respiratory disease to 51% of that for bottle feeding.

exp(c (-0.669-1.96*0.153, -0.669+1.96*0.153))		# Conventional CI for the effect of breast feeding
# [1] 0.3795078 0.6913424

library(MASS)										# Profile likelihood CIs
exp(confint(mdl))
# Waiting for profiling to be done...
                # 2.5 %    97.5 %
# (Intercept) 0.1591988 0.2474333
# sexGirl     0.5536209 0.9629225
# foodBreast  0.3781905 0.6895181
# foodSuppl   0.5555372 1.2464312


### Binomial Regression: Link function example in Section 2.7 in Faraway's book
data (bliss)
# The data is from 5 experiments in Bliss(1935) on the effect of insecticide concentration.
# In each experiment, there are 30 insects. There are five doses of insecticide concentration. 
# The number of death and alive insects are recorded.

# ?bliss

bliss
#   dead alive conc
# 1    2    28    0
# 2    8    22    1
# 3   15    15    2
# 4   23     7    3
# 5   27     3    4

modl <- glm(cbind(dead, alive) ~ conc, family=binomial, data=bliss)					# default logit link
modp <- glm(cbind(dead, alive) ~ conc, family=binomial(link=probit),data=bliss)		# probit link
modc <- glm(cbind(dead, alive) ~ conc, family=binomial(link=cloglog), data=bliss)	# cloglog link

cbind(fitted(modl),fitted(modp),fitted(modc))										# predictions are similar
        # [,1]       [,2]      [,3]
# 1 0.08917177 0.08424186 0.1272700
# 2 0.23832314 0.24487335 0.2496909
# 3 0.50000000 0.49827210 0.4545910
# 4 0.76167686 0.75239612 0.7217655
# 5 0.91082823 0.91441122 0.9327715

windows(20,10)
par(mfrow=c(1,3))

x <- seq(-2,8,0.2)
pl <- ilogit(modl$coef[1]+modl$coef[2]*x)
pp <- pnorm(modp$coef[1]+modp$coef[2]*x)
pc <- 1-exp(-exp((modc$coef[1]+modc$coef[2]*x)))
plot(x,pl,type="l",ylab="Probability",xlab="Dose",main="Comparison of fitted values from different links")
#??
lines(x,pp,lty=2,col=2)
lines(x,pc,lty=5,col=4)
legend("bottomright", lty=c(1,2,5),col=c(1,2,4),legend=c("logit","probit","cloglog"),lwd=1.5,cex=1.5)

matplot(x,cbind(pp/pl,(1-pp)/(1-pl)),type="l",xlab="Dose",ylab="Ratio",main="Tail Prob Ratios: probit vs logit")
abline(h=1,col=4,lwd=2)
legend("bottomleft", lty=c(1,2,1),col=c(1,2,4),legend=c("lower tail ratio","upper tail ratio","1"),lwd=1.5,cex=1.5)

matplot(x,cbind(pc/pl,(1-pc)/(1-pl)),type="l",xlab="Dose",ylab="Ratio",main="Tail Prob Ratios: cloglog vs logit")
abline(h=1,col=4,lwd=2)
legend("bottomleft", lty=c(1,2,1),col=c(1,2,4),legend=c("lower tail ratio","upper tail ratio","1"),lwd=1.5,cex=1.5)



### Binomial Regression: Separation example in Section 2.8 in Faraway's book

library(faraway)
data(hormone)
# Study by Margolese, M. (1970) on urinary androsterone (androgen) and etiocholanolone (estrogen) 
# values from 26 healthy males.
# Consider the relationship between sexual orientation and hormone levels.
# ?hormone

hormone

   # androgen estrogen orientation
# 1       3.9      1.8           s
# 2       4.0      2.3           s
# 3       3.8      2.3           s
# 4       3.9      2.5           s
# 5       2.9      1.3           s
# 6       3.2      1.7           s
# 7       4.6      3.4           s
# 8       4.3      3.1           s
# 9       3.1      1.8           s
# 10      2.7      1.5           s
# 11      2.3      1.4           s
# 12      2.5      2.1           g
# 13      1.6      1.1           g
# 14      3.9      3.9           g
# 15      3.4      3.6           g
# 16      2.3      2.5           g
# 17      1.6      1.7           g
# 18      2.5      2.9           g
# 19      3.4      4.0           g
# 20      1.6      1.9           g
# 21      4.3      5.3           g
# 22      2.0      2.7           g
# 23      1.8      3.6           g
# 24      2.2      4.1           g
# 25      3.1      5.2           g
# 26      1.3      4.0           g

modl <- glm(orientation ~ estrogen + androgen,
hormone, family=binomial)												# MLE does not converge
summary(modl)															# large estimates

plot(estrogen ~androgen,data=hormone,pch=as.character(orientation)) 
abline(-84.5/90.2,100.9/90.2)											# perfect separation by the line

library(brglm)															# bias reduction GLM
modb <- brglm(orientation ~ estrogen + androgen,hormone, family=binomial)
summary(modb)
abline(-3.650/3.585,4.073/3.585,lty="dashed")	

### Binomial Regression: Over-dispersion example in Section 2.11 in Faraway's book

library(faraway)
data(troutegg)
# Data from Manly B. (1978) on the number of survival eggs at 5 locations and 4 time periods
# Study the effect of location and time on the survival proportion
# ?troutegg
troutegg
   # survive total location period
# 1       89    94        1      4
# 2      106   108        2      4
# 3      119   123        3      4
# 4      104   104        4      4
# 5       49    93        5      4
# 6       94    98        1      7
# 7       91   106        2      7
# 8      100   130        3      7
# 9       80    97        4      7
# 10      11   113        5      7
# 11      77    86        1      8
# 12      87    96        2      8
# 13      88   119        3      8
# 14      67    99        4      8
# 15      18    88        5      8
# 16     141   155        1     11
# 17     104   122        2     11
# 18      91   125        3     11
# 19     111   132        4     11
# 20       0   138        5     11

ftable(xtabs(cbind(survive,total)~location+period,troutegg))

ftable(xtabs(cbind(total,survive)~period+location,troutegg))
                 # survive total
# location period               
# 1        4            89    94
         # 7            94    98
         # 8            77    86
         # 11          141   155
# 2        4           106   108
         # 7            91   106
         # 8            87    96
         # 11          104   122
# 3        4           119   123
         # 7           100   130
         # 8            88   119
         # 11           91   125
# 4        4           104   104
         # 7            80    97
         # 8            67    99
         # 11          111   132
# 5        4            49    93
         # 7            11   113
         # 8            18    88
         # 11            0   138
		 
bmod <- glm(cbind(survive,total-survive) ~
location+period,family=binomial,troutegg)
summary(bmod)															# large residual deviance
# Call:
# glm(formula = cbind(survive, total - survive) ~ location + period, 
    # family = binomial, data = troutegg)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -4.8305  -0.3650  -0.0303   0.6191   3.2434  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   4.6358     0.2813  16.479  < 2e-16 ***
# location2    -0.4168     0.2461  -1.694   0.0903 .  
# location3    -1.2421     0.2194  -5.660 1.51e-08 ***
# location4    -0.9509     0.2288  -4.157 3.23e-05 ***
# location5    -4.6138     0.2502 -18.439  < 2e-16 ***
# period7      -2.1702     0.2384  -9.103  < 2e-16 ***
# period8      -2.3256     0.2429  -9.573  < 2e-16 ***
# period11     -2.4500     0.2341 -10.466  < 2e-16 ***
# ---
# Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# (Dispersion parameter for binomial family taken to be 1)

    # Null deviance: 1021.469  on 19  degrees of freedom
# Residual deviance:   64.495  on 12  degrees of freedom
# AIC: 157.03

# Number of Fisher Scoring iterations: 5


1-pchisq(deviance(bmod),df.residual(bmod))								# not a good fit
# [1] 3.379416e-09 

sigma2 <- sum(residuals(bmod,type="pearson")^2) /12;sigma2				# large estimate of the dispersion
# [1] 5.330322

bmod_quasi <- glm(cbind(survive,total-survive) ~
location+period,family=quasibinomial,troutegg)							# valid inference from quasi-binomial fit

summary(bmod_quasi)
# Call:
# glm(formula = cbind(survive, total - survive) ~ location + period, 
    # family = quasibinomial, data = troutegg)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -4.8305  -0.3650  -0.0303   0.6191   3.2434  

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.6358     0.6495   7.138 1.18e-05 ***
# location2    -0.4168     0.5682  -0.734 0.477315    
# location3    -1.2421     0.5066  -2.452 0.030501 *  
# location4    -0.9509     0.5281  -1.800 0.096970 .  
# location5    -4.6138     0.5777  -7.987 3.82e-06 ***
# period7      -2.1702     0.5504  -3.943 0.001953 ** 
# period8      -2.3256     0.5609  -4.146 0.001356 ** 
# period11     -2.4500     0.5405  -4.533 0.000686 ***
# ---
# Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# (Dispersion parameter for quasibinomial family taken to be 5.330358)

    # Null deviance: 1021.469  on 19  degrees of freedom
# Residual deviance:   64.495  on 12  degrees of freedom
# AIC: NA

# Number of Fisher Scoring iterations: 5



# Create some data
n <- 500
x1 <- runif(n,0,100)
x2 <- runif(n,0,100)
y <- (x2 - x1 + rnorm(n,sd=20)) < 0

# Fit a binomial regression model
model <- glm(y ~ x1 + x2, family="binomial")
summary(model)

#Create some new data
n2 <- 100
new.df <- data.frame(x1 = runif(n2, 0, 100), x2 = runif(n2,0,100))

# Predict the probabilities
# predict(model, new.df) will just give you the logit (z) values
# We want the probabilities so use the "response" type
probs <- predict(model, new.df, "response")

# Draw some random values between 0 and 1
draws <- runif(n2)

# Check if draw value is less than threshold probability
results <- (draws < probs)