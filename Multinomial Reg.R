### Multinomial Regression example in Section 5.1 in Faraway's book
library(faraway)
library(nnet)
data(nes96)
# From Rosenstone, Kinder, and Miller (1997) a 10 variable subset of the 1996 American National Election Study. 
# Study the relationship between the party identification and three variables: age, education, and income.
# ?nes96
head(nes96)
# popul TVnews selfLR ClinLR DoleLR     PID age  educ   income    vote
# 1     0      7 extCon extLib    Con  strRep  36    HS $3Kminus    Dole
# 2   190      1 sliLib sliLib sliCon weakDem  20  Coll $3Kminus Clinton
# 3    31      7    Lib    Lib    Con weakDem  24 BAdeg $3Kminus Clinton
# 4    83      4 sliLib    Mod sliCon weakDem  28 BAdeg $3Kminus Clinton
# 5   640      7 sliCon    Con    Mod  strDem  68 BAdeg $3Kminus Clinton
# 6   110      3 sliLib    Mod    Con weakDem  21  Coll $3Kminus Clinton

sPID <- nes96$PID  														# Party identification
levels(sPID)
# [1] "strDem"  "weakDem" "indDem"  "indind"  "indRep"  "weakRep" "strRep"

summary(sPID)																
# strDem weakDem  indDem  indind  indRep weakRep  strRep 
# 200     180     108      37      94     150     175

levels(sPID) <-c("Democrat","Democrat","Independent","Independent",			# Combing into 3 categories
                 "Independent","Republican","Republican")

summary(sPID)
# Democrat Independent  Republican 
# 380         239         325

inca <- c(1.5,4,6,8,9.5,10.5,11.5,12.5,13.5,14.5,16,18.5,21,23.5,			# Changing the variable to numerical value
          27.5,32.5,37.5,42.5,47.5,55,67.5,82.5,97.5,115)								# equalling the midpoint of each range
nincome <- inca[unclass(nes96$income)]
summary(nincome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.50   23.50   37.50   46.58   67.50  115.00

table(nes96$educ)
# MS HSdrop     HS   Coll  CCdeg  BAdeg  MAdeg 
# 13     52    248    187     90    227    127


windows(20,10)
par(mfrow=c(1,3))

matplot(prop.table(table(nes96$educ,sPID),1),type="l",
        xlab="Education",ylab="Proportion",ylim=c(0.1,0.7),lty=c(1,2,5),col=c(1,2,4)) # Proportion of D decreases as educ increases
legend("topright", lty=c(1,2,5),col=c(1,2,4),legend=c("Democrat","Independent","Republican"),lwd=1.5,cex=1.5)



cutinc <- cut(nincome,7)											#MAke -intevals- cut the variable into 7 levels
il <- c(8,26,42,58,74,90,107)												# approximate midpoints of the ranges
matplot(il,prop.table(table(cutinc,sPID),1),lty=c(1,2,5),ylim=c(0.1,0.7),col=c(1,2,4),
        type="l",ylab="Proportion",xlab="Income")									# Proportion of D decreases as income increases
legend("topright", lty=c(1,2,5),col=c(1,2,4),legend=c("Democrat","Independent","Republican"),lwd=1.5,cex=1.5)

cutage <- cut(nes96$age,7)													# ditto
al <- c(24,34,44,54,65,75,85)



matplot(al,prop.table(table(cutage,sPID),1),lty=c(1,2,5),ylim=c(0.1,0.7),col=c(1,2,4),
        type="l",ylab="Proportion",xlab="Age")
legend("topright", lty=c(1,2,5),col=c(1,2,4),legend=c("Democrat","Independent","Republican"),lwd=1.5,cex=1.5)



library(nnet)																# neural net work package
mmod <- multinom(sPID ~ age + educ + nincome, nes96)						# multinomial logit fit
summary(mmod)
# Call:
# multinom(formula = sPID ~ age + educ + nincome, data = nes96)

# Coefficients:
# (Intercept)          age     educ.L     educ.Q    educ.C
# Independent   -1.197260 0.0001534525 0.06351451 -0.1217038 0.1119542
# Republican    -1.642656 0.0081943691 1.19413345 -1.2292869 0.1544575
# educ^4     educ^5      educ^6    nincome
# Independent -0.07657336  0.1360851  0.15427826 0.01623911
# Republican  -0.02827297 -0.1221176 -0.03741389 0.01724679

# Std. Errors:
# (Intercept)         age    educ.L    educ.Q    educ.C    educ^4
# Independent   0.3265951 0.005374592 0.4571884 0.4142859 0.3498491 0.2883031
# Republican    0.3312877 0.004902668 0.6502670 0.6041924 0.4866432 0.3605620
# educ^5    educ^6     nincome
# Independent 0.2494706 0.2171578 0.003108585
# Republican  0.2696036 0.2031859 0.002881745

# Residual Deviance: 1968.333 
# AIC: 2004.333 


mmode <- multinom(sPID ~ age + nincome, nes96)				# multinomial logit fit without education
summary(mmode)

# Call:
# multinom(formula = sPID ~ age + nincome, data = nes96)

# Coefficients:
# (Intercept)          age   nincome
# Independent   -1.187181 0.0002260719 0.0161244
# Republican    -1.154787 0.0041328499 0.0178682

# Std. Errors:
# (Intercept)         age     nincome
# Independent   0.2941042 0.005143979 0.002861903
# Republican    0.2736512 0.004700426 0.002666931

# Residual Deviance: 1984.539 
# AIC: 1996.539

pchisq(deviance(mmode) - deviance(mmod),mmod$edf-mmode$edf,lower=F)	# education is not significant
# [1] 0.1819634

mmodi <- step(mmod)				# model selection based on AIC; resulting model with income only
# Start:  AIC=2004.33
# sPID ~ age + educ + nincome

# trying - age 
## weights:  27 (16 variable)
# initial  value 1037.090001 
# iter  10 value 988.896864
# iter  20 value 985.822223
# final  value 985.812737 
# converged
# trying - educ 
## weights:  12 (6 variable)
# initial  value 1037.090001 
# iter  10 value 992.269502
# final  value 992.269484 
# converged
# trying - nincome 
## weights:  27 (16 variable)
# initial  value 1037.090001 
# iter  10 value 1009.025560
# iter  20 value 1006.961593
# final  value 1006.955275 
# converged
# Df      AIC
# - educ     6 1996.539
# - age     16 2003.625
# <none>    18 2004.333
# - nincome 16 2045.911
## weights:  12 (6 variable)
# initial  value 1037.090001 
# iter  10 value 992.269502
# final  value 992.269484 
# converged

# Step:  AIC=1996.54
# sPID ~ age + nincome

# trying - age 
## weights:  9 (4 variable)
# initial  value 1037.090001 
# final  value 992.712152 
# converged
# trying - nincome 
## weights:  9 (4 variable)
# initial  value 1037.090001 
# final  value 1020.425203 
# converged
# Df      AIC
# - age      4 1993.424
# <none>     6 1996.539
# - nincome  4 2048.850
## weights:  9 (4 variable)
# initial  value 1037.090001 
# final  value 992.712152 
# converged

# Step:  AIC=1993.42
# sPID ~ nincome

# trying - nincome 
## weights:  6 (2 variable)
# initial  value 1037.090001 
# final  value 1020.636052 
# converged
# Df      AIC
# <none>     4 1993.424
# - nincome  2 2045.272
summary(mmodi)
# Call:
# multinom(formula = sPID ~ nincome, data = nes96)

# Coefficients:
# (Intercept)    nincome
# Independent  -1.1749331 0.01608683
# Republican   -0.9503591 0.01766457

# Std. Errors:
# (Intercept)     nincome
# Independent   0.1536103 0.002849738
# Republican    0.1416859 0.002652532

# Residual Deviance: 1985.424 
# AIC: 1993.424 

il;predict(mmodi,data.frame(nincome=il))
# [1] Democrat   Democrat   Democrat   Republican Republican Republican Republican
# Levels: Democrat Independent Republican

predict(mmodi,data.frame(nincome=il),type="probs")	# predicted proportions for midpoints
# Democrat Independent Republican
# 1 0.5566253   0.1955183  0.2478565
# 2 0.4804946   0.2254595  0.2940459
# 3 0.4134268   0.2509351  0.3356381
# 4 0.3493884   0.2743178  0.3762939
# 5 0.2903271   0.2948600  0.4148129
# 6 0.2375755   0.3121136  0.4503109
# 7 0.1891684   0.3266848  0.4841468


cc <- c(0,-1.17493,-0.95036)						# betas in mmodi
exp(cc)/sum(exp(cc))								# predicted proportions for income =0
# [1] 0.5898166 0.1821593 0.2280242
predict(mmodi,data.frame(nincome=0),type="probs")
# Democrat Independent  Republican 
# 0.5898168   0.1821588   0.2280244


(pp <-predict(mmodi,data.frame(nincome=c(0,1)),type="probs"))
# Democrat Independent Republican
# 1 0.5898168   0.1821588  0.2280244
# 2 0.5857064   0.1838228  0.2304708
log(pp[1,1]*pp[2,2]/(pp[1,2]*pp[2,1]))		# log odds change from dem to ind; beta for income in ind
# [1] 0.016087
log(pp[1,1]*pp[2,3]/(pp[1,3]*pp[2,1]))		# log odds change from dem to rep; beta for income in rep
# [1] 0.01766457


### Ordinal Response Regression example in Section 5.3 in Faraway's book

library(MASS)


?polr	
pomod <- polr(sPID ~ age + educ + nincome, nes96)
summary(pomod)

# Re-fitting to get Hessian

# Call:
# polr(formula = sPID ~ age + educ + nincome, data = nes96)

# Coefficients:
# Value Std. Error  t value
# age      0.005775   0.003887  1.48581
# educ.L   0.724087   0.384388  1.88374
# educ.Q  -0.781361   0.351173 -2.22500
# educ.C   0.040168   0.291762  0.13767
# educ^4  -0.019925   0.232429 -0.08573
# educ^5  -0.079413   0.191533 -0.41462
# educ^6  -0.061104   0.157747 -0.38735
# nincome  0.012739   0.002140  5.95187

# Intercepts:
# Value   Std. Error t value
# Democrat|Independent    0.6449  0.2435     2.6479
# Independent|Republican  1.7374  0.2493     6.9694

# Residual Deviance: 1984.211 
# AIC: 2004.211 

pomodi <- step(pomod)	# only income matters...
# Start:  AIC=2004.21
# sPID ~ age + educ + nincome

# Df    AIC
# - educ     6 2002.8
# <none>       2004.2
# - age      1 2004.4
# - nincome  1 2038.6

# Step:  AIC=2002.83
# sPID ~ age + nincome

# Df    AIC
# - age      1 2001.4
# <none>       2002.8
# - nincome  1 2047.2

# Step:  AIC=2001.36
# sPID ~ nincome

# Df    AIC
# <none>       2001.4
# - nincome  1 2045.3

summary(pomodi)

# Re-fitting to get Hessian

# Call:
# polr(formula = sPID ~ nincome, data = nes96)

# Coefficients:
# Value Std. Error t value
# nincome 0.01312   0.001971   6.657

# Intercepts:
# Value   Std. Error t value
# Democrat|Independent    0.2091  0.1123     1.8627
# Independent|Republican  1.2916  0.1201    10.7526

# Residual Deviance: 1995.363 
# AIC: 2001.363

ilogit(0.2091)					# probability being a democrat for 0 income
# [1] 0.5520854
ilogit(1.2916)-ilogit(0.2091)	# probability being independent for 0 income
# [1] 0.2323325
il <- c(8,26,42,58,74,90,107)	
predict(pomodi,data.frame(nincome=il,row.names=il),type="probs")
# Democrat Independent Republican
# 8   0.5260129   0.2401191  0.2338679
# 26  0.4670450   0.2541588  0.2787962
# 42  0.4153410   0.2617693  0.3228897
# 58  0.3654362   0.2641882  0.3703756
# 74  0.3182635   0.2612285  0.4205080
# 90  0.2745456   0.2531189  0.4723355
# 107 0.2324161   0.2395468  0.5280371








### Hierarchical Response example in Section 5.2 in Faraway's book
library(faraway)
library(nnet)
data (cns)
# From Lowe, Roberts, and Lloyd (1971) on frequencies of various malformations of the central nervous system.
# Study was designed to determine the effect of water hardness on the incidence of such malformations. 
# ?cns
cns
# Area             NoCNS An  Sp  Other  Water   Work
# 1        Cardiff  4091  5  9     5   110 NonManual
# 2        Newport  1515  1  7     0   100 NonManual
# 3        Swansea  2394  9  5     0    95 NonManual
# 4     GlamorganE  3163  9 14     3    42 NonManual
# 5     GlamorganW  1979  5 10     1    39 NonManual
# 6     GlamorganC  4838 11 12     2   161 NonManual
# 7      MonmouthV  2362  6  8     4    83 NonManual
# 8  MonmouthOther  1604  3  6     0   122 NonManual
# 9        Cardiff  9424 31 33    14   110    Manual
# 10       Newport  4610  3 15     6   100    Manual
# 11       Swansea  5526 19 30     4    95    Manual
# 12    GlamorganE 13217 55 71    19    42    Manual
# 13    GlamorganW  8195 30 44    10    39    Manual
# 14    GlamorganC  7803 25 28    12   161    Manual
# 15     MonmouthV  9962 36 37    13    83    Manual
# 16 MonmouthOther  3172  8 13     3   122    Manual

cns$CNS <- cns$An+cns$Sp+cns$Other

plot(log(CNS/NoCNS) ~ Water, cns,pch=as.character(Work))

binmodw <- glm(cbind(CNS,NoCNS) ~ Water + Work, cns,      #Work also catagorical , logic: doesn't matter at all
               family=binomial)

summary(binmodw)
# Call:
# glm(formula = cbind(CNS, NoCNS) ~ Water + Work, family = binomial, 
# data = cns)

# Deviance Residuals: 
# Min        1Q    Median        3Q       Max  
# -2.65570  -0.30179  -0.03131   0.57213   1.32998  

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -4.4325803  0.0897889 -49.367  < 2e-16 ***
# Water         -0.0032644  0.0009684  -3.371 0.000749 ***
# WorkNonManual -0.3390577  0.0970943  -3.492 0.000479 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 41.047  on 15  degrees of freedom
# Residual deviance: 12.363  on 13  degrees of freedom
# AIC: 102.49

# Number of Fisher Scoring iterations: 4



exp(-0.339058)	# births to nonmanual have a 29% chance of malformation than manual ones, holding water same
# [1] 0.7124411				


cmmod <- multinom(cbind(An,Sp,Other) ~ Water + Work,cns)	# effects on different types of malformation
summary(cmmod)												# effects not significant
# Call:
# multinom(formula = cbind(An, Sp, Other) ~ Water + Work, data = cns)

# Coefficients:
# (Intercept)        Water WorkNonManual
# Sp      0.3752018 -0.001297048     0.1157557
# Other  -1.1225496  0.002182689    -0.2702791

# Std. Errors:
# (Intercept)       Water WorkNonManual
# Sp      0.1900329 0.002032944     0.2086875
# Other   0.2795628 0.002896841     0.3247247

# Residual Deviance: 1371.524 
# AIC: 1383.524

nmod <- step(cmmod)									# stepwise regression returns null model
# Start:  AIC=1383.52
# cbind(An, Sp, Other) ~ Water + Work

# trying - Water 
## weights:  9 (4 variable)
# initial  value 762.436928 
# iter  10 value 686.562074
# final  value 686.562063 
# converged
# trying - Work 
## weights:  9 (4 variable)
# initial  value 762.436928 
# final  value 686.580556 
# converged
# Df      AIC
# - Water  4 1381.124
# - Work   4 1381.161
# <none>   6 1383.524
## weights:  9 (4 variable)
# initial  value 762.436928 
# iter  10 value 686.562074
# final  value 686.562063 
# converged

# Step:  AIC=1381.12
# cbind(An, Sp, Other) ~ Work

# trying - Work 
## weights:  6 (2 variable)
# initial  value 762.436928 
# final  value 687.227416 
# converged
# Df      AIC
# - Work  2 1378.455
# <none>  4 1381.124
## weights:  6 (2 variable)
# initial  value 762.436928 
# final  value 687.227416 
# converged

# Step:  AIC=1378.45
# cbind(An, Sp, Other) ~ 1

summary(nmod)
# Call:
# multinom(formula = cbind(An, Sp, Other) ~ 1, data = cns)

# Coefficients:
# (Intercept)
# Sp      0.2896333
# Other  -0.9808293

# Residual Deviance: 1374.455 
# AIC: 1378.455 

cc <- c(0,0.28963,-0.98083)				# predicted probability
names(cc) <- c("An","Sp","Other")
exp(cc)/sum(exp(cc))
# An        Sp     Other 
# 0.3688767 0.4927946 0.1383287 

mmodm <- multinom(cbind(NoCNS,An,Sp,Other) ~ Water + Work,cns)	# mutinomial model without hierarchy 

summary(mmodm)	# both water and work are significant; fails to recognize they do not have effect on 
# different types of malformation; large residual deviance...
# Call:
# multinom(formula = cbind(NoCNS, An, Sp, Other) ~ Water + Work, 
# data = cns)

# Coefficients:
# (Intercept)         Water WorkNonManual
# An      -5.455142 -0.0029088415    -0.3638609
# Sp      -5.071037 -0.0043231569    -0.2435803
# Other   -6.594755 -0.0005127307    -0.6420771

# Std. Errors:
# (Intercept)       Water WorkNonManual
# An      0.1478021 0.001586305     0.1604950
# Sp      0.1267049 0.001387540     0.1348058
# Other   0.2456265 0.002550003     0.2834179

# Residual Deviance: 9390.965 
# AIC: 9408.965 
