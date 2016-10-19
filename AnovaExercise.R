# This homework is an exercise on Two-way ANOVA.  We will build up R code 
# that interprets the coefficients in Two-way ANOVA.

# We will be negligent in the sense that we will ignore precautions 
# that a numerical analyst would build into his code, and we will not
# consider measures to improve numerical conditioning such as pivoting
# (switching the order of the variables) either.

#----------------------------------------------------------------

# PROBLEM 1: Description of the pvc data. Start by loading the data:
library(faraway)  				# install this package if needed
attach(pvc)						# load the pvc data.
pvc						# Type help(pvc) for more details
operator;resin
# Describe the data. Are the group balanced? Also make the interaction plot.
# Does there seem to be an interaction effect?

# SOLUTION:

#Description:
#From the pvc data, we can see that there are 2 factors "operator" and "resin",
#with 3 levels and 8 levels each, here I=3, J=8, K=2, i.e. for each pair of 
#factor(i,j), there are 2 observations.
table(operator,resin) 
#From the table, we can see the groups are balanced. 

interaction.plot(pvc$operator, pvc$resin, pvc$psize)
#By the interaction plot, there seems to be slight interaction between factors, 
#Since the lines are not that parallel


#----------------------------------------------------------------

# PROBLEM 2: Start with the default contrasts
options()$contrasts  
# Find the mean particle size for each resin in operator 3 
# (with two decimal places).
# Use only numbers from the following output
summary(lm(psize~operator*resin, data=pvc))

# SOLUTION:

# The general expression: Y_{ijk} = mu + alpha_i + beta_j + gamma_{ij} + epsilon_{ijk}
# We are using defualt contrainst indicating that
# alpha_1 = 0 = beta_1; gamma_{ij} = theta*alpha_i*beta_j
# using only numbers from "summary(lm(psize~operator*resin, data=pvc))"

result1 <- lm(psize~operator*resin, data=pvc)
Y_31_hat <- unname(result1$coeff["(Intercept)"] + result1$coeff["operator3"]) #no interaction term, since beta_1=0
Y_32_hat <- unname(result1$coeff["(Intercept)"] + result1$coeff["resin2"] + result1$coeff["operator3"] + result1$coeff["operator3:resin2"])#group 3&2
Y_33_hat <- unname(result1$coeff["(Intercept)"] + result1$coeff["resin3"] + result1$coeff["operator3"] + result1$coeff["operator3:resin3"])#group 3&3
Y_34_hat <- unname(result1$coeff["(Intercept)"] + result1$coeff["resin4"] + result1$coeff["operator3"] + result1$coeff["operator3:resin4"])#group 3&4                                                                           
Y_35_hat <- unname(result1$coeff["(Intercept)"] + result1$coeff["resin5"] + result1$coeff["operator3"] + result1$coeff["operator3:resin5"])#group 3&5
Y_36_hat <- unname(result1$coeff["(Intercept)"] + result1$coeff["resin6"] + result1$coeff["operator3"] + result1$coeff["operator3:resin6"])#group 3&6
Y_37_hat <- unname(result1$coeff["(Intercept)"] + result1$coeff["resin7"] + result1$coeff["operator3"] + result1$coeff["operator3:resin7"])#group 3&7
Y_38_hat <- unname(result1$coeff["(Intercept)"] + result1$coeff["resin8"] + result1$coeff["operator3"] + result1$coeff["operator3:resin8"])#group 3&8

Y_3_hat <-c(Y_31_hat,Y_32_hat,Y_33_hat,Y_34_hat,Y_35_hat,Y_36_hat,Y_37_hat,Y_38_hat )#Aggregate of each pair
round(Y_3_hat,digits = 2)
#Thus we find the mean particle size for each resin in operator 3 (with two decimal places).
#----------------------------------------------------------------

# PROBLEM 3: Now change the contrasts to 
options(contrasts=c("contr.sum", "contr.sum")) 
# Find the mean particle size for each resin in operator 2.
# (with two decimal places).
# Use only numbers from the following output.
summary(lm(psize~operator*resin, data=pvc))

# SOLUTION:
#Similar to problem 2, we use the coefficients of the new contrast which controls the
#sum of alpha_i and beta_j =0
options(contrasts=c("contr.sum", "contr.sum")) 
result2 <- lm(psize~operator*resin, data=pvc)
#For factor resin, group 1~7 just regular calculation as problem 2
Y_21_hat <- unname(result2$coeff["(Intercept)"] + result2$coeff["resin1"] + result2$coeff["operator2"] + result2$coeff["operator2:resin1"])
Y_22_hat <- unname(result2$coeff["(Intercept)"] + result2$coeff["resin2"] + result2$coeff["operator2"] + result2$coeff["operator2:resin2"])
Y_23_hat <- unname(result2$coeff["(Intercept)"] + result2$coeff["resin3"] + result2$coeff["operator2"] + result2$coeff["operator2:resin3"])
Y_24_hat  <- unname(result2$coeff["(Intercept)"] + result2$coeff["resin4"] + result2$coeff["operator2"] + result2$coeff["operator2:resin4"])                                                                            
Y_25_hat  <- unname(result2$coeff["(Intercept)"] + result2$coeff["resin5"] + result2$coeff["operator2"] + result2$coeff["operator2:resin5"])
Y_26_hat <- unname(result2$coeff["(Intercept)"] + result2$coeff["resin6"] + result2$coeff["operator2"] + result2$coeff["operator2:resin6"])
Y_27_hat  <- unname(result2$coeff["(Intercept)"] + result2$coeff["resin7"] + result2$coeff["operator2"] + result2$coeff["operator2:resin7"])

#Using the property of new contrast, we can get other coefficients by simple algebra
beta_8 <- -1*unname(result2$coeff["resin1"] + result2$coeff["resin2"] + result2$coeff["resin3"] + result2$coeff["resin4"] + result2$coeff["resin5"] + result2$coeff["resin6"] + result2$coeff["resin7"])
gamma_28 <- -1*unname(result2$coeff["operator2:resin1"] + result2$coeff["operator2:resin2"] + result2$coeff["operator2:resin3"] + result2$coeff["operator2:resin4"] + result2$coeff["operator2:resin5"] + result2$coeff["operator2:resin6"] + result2$coeff["operator2:resin7"])

Y_28_hat<- unname(result2$coeff["(Intercept)"] + beta_8 + result2$coeff["operator2"] + gamma_28)

Y_2_hat <-c(Y_21_hat,Y_22_hat,Y_23_hat,Y_24_hat,Y_25_hat,Y_26_hat,Y_27_hat,Y_28_hat )
round(Y_2_hat,digits = 2)
#Thus we find the mean particle size for each resin in operator 2 (with two decimal places).

#----------------------------------------------------------------

# PROBLEM 4: Produce the ANOVA table for the complete model. Is 
# the interaction significant?

# SOLUTION:

results = lm(psize ~ operator+resin+operator*resin, data=pvc)
anova(results)
#The p value of interaction term is 0.76, much larger than 0.05,
#Thus the interaction is not significant
#----------------------------------------------------------------