### Contingency Table example in Section 4.1 in Faraway's book
library(faraway)

y <- c(320,14,80,36) # wafer data in Hall (1994)
particle <- gl (2,1,4,labels=c("no","yes"))
quality <- gl(2,2,labels=c("good","bad"))
wafer <- data.frame(y,particle,quality)
wafer
# y particle quality
# 1 320       no    good
# 2  14      yes    good
# 3  80       no     bad
# 4  36      yes     bad

(ov <- xtabs(y ~ quality+particle))
# particle
# quality  no yes
# good 320  14
# bad   80  36


(pp <- prop.table( xtabs(y ~ particle)))  # marginal proportion
# particle
# no yes
# 0.88889 0.11111

(qp <- prop.table( xtabs(y ~ quality)))		# marginal proportion
# quality
# good       bad 
# 0.7422222 0.2577778


(fv <- outer(qp,pp)*450)					# fitted value under H0
# particle
# quality       no      yes
# good 296.8889 37.11111
# bad  103.1111 12.88889

2*sum(ov*log(ov/fv))						# deviance in log-likelihood
# [1] 54.03045								# same as deviance in Poisson regression


modl <- glm(y ~ particle+quality, poisson)	# Poisson model
summary(modl)

# Call:
# glm(formula = y ~ particle + quality, family = poisson)

# Deviance Residuals: 
# 1       2       3       4  
# 1.324  -4.350  -2.370   5.266  

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   5.6934     0.0572  99.535   <2e-16 ***
# particleyes  -2.0794     0.1500 -13.863   <2e-16 ***
# qualitybad   -1.0575     0.1078  -9.813   <2e-16 ***
# ---
# Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# (Dispersion parameter for poisson family taken to be 1)

# Null deviance: 474.10  on 3  degrees of freedom
# Residual deviance:  54.03  on 1  degrees of freedom
# AIC: 83.774

# Number of Fisher Scoring iterations: 5
