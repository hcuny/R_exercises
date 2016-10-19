# INSTRUCTION: Edit this R file by adding your solution to the end of each 
# question. Your submitted file should run smoothly in R and provide all 
# required results. You are allowed to work with other students but the 
# homework should be in your own words. Identical solutions will receive a 0 
# in grade and will be investigated.

# This homework is an exercise in GLM estimation and inference.  We will
# build up R code that 1) implement the IRLS algorithm for Poisson 
# GLM and 2) use the algorithm to analyze a real dataset. 

# We will be negligent in the sense that we will ignore precautions
# that a numerical analyst would build into his code, and we
# will not consider measures to improve numerical conditioning such as 
# pivoting (switching the order of the variables) either.



#--------------------------------------------------------------------

# PROBLEM 1: Write a function "my.poisson.glm(X,y)" to take X as a matrix
# and y as a vector of counts. The function then computes a 
# list with the following named elements:

# a) "Coeffs": a matrix with one row per regression coefficient and columns
#    named "Coefficient", "Std.Err.Est", "z-Statistic", "P-Value"
# b) "Residual Deviance": a number, D
# c) "Pearson Residuals": the vector of residuals
# d) "Deviance Residuals": the vector of residuals
# e) "Anscombe Residuals": the vector of residuals

# You must use the ITLS algorithm in the function. The link function should be 
# log(). The only high-level function allowed is lm().

# SOLUTION: 



#
my.poisson.glm<-function(X,y){
  
  mu <- y                   	#Initialization
  eta <- log(mu)						
  z <- eta + ((y-mu)/mu)			# Initial response
  w <- mu						# Initial weights
  lmod <- lm(z ~ X, weights=w)	# Initial LS estimate of beta
  
for(i in 1:15){
  beta_old<-coef(lmod)
  #eta <- lmod$fit
  eta <-  as.vector(cbind(1,X) %*% as.matrix(lmod$coefficients, nrow=ncol(X), ncol=1))
  mu <- exp(eta)  					# g-inverse of eta to update mu            
  z <- eta + ((y-mu)/mu)			# Updating the response             
  w <- mu					          	# Updating the weights                  
  lmod <- lm(z ~ X, weights=w)	# Updating the estimate of beta   
  beta<-coef(lmod)
  if(as.numeric(sqrt(crossprod(beta-beta_old))) < 0.0001) {
    break}      #if changes between 2 iterations is small enough, we break
  
}
#Store the coefficients into matrix Coeffs
Coeffs = matrix(nrow=length(coef(lmod)), ncol=4,byrow = TRUE)        # fill matrix by rows 
colnames(Coeffs) <- c("Coefficient", "Std.Err.Est", "z-Statistic", "P-Value")

Coeffs[,1] <- coef(lmod) #Coefficients
xm <- model.matrix(lmod)
wm <- diag(w)
Coeffs[,2] <- sqrt(diag(solve(t(xm) %*% wm %*% xm)))  #standard error
Coeffs[,3] <- Coeffs[,2]/Coeffs[,1]    # z-Statistic  
Coeffs[,4] <- 2*pt(-abs(Coeffs[,3]),df=length(y)-ncol(X))# P-Value


#Calculating Residuals
d=rep(1,nrow(X))
X<-cbind(d,X)                  #include intercept term

mu_hat <- X %*% Coeffs[,1]
theta_hat_MLE <- log(mu_hat)
theta_tilde <- log(y)
D <- 2*sum((theta_tilde-theta_hat_MLE)*y - y + mu_hat)
Pearson_Residuals <- (y - mu_hat)/sqrt(mu_hat)
Deviance_Residuals <- sign(y - mu_hat)*sqrt(D)
Anscombe_Residuals <- (1.5*y^(2/3) - 1.5*mu_hat^(2/3))/(mu_hat^(1/6))

return(list(Coeffs=Coeffs, Residual_Deviance=D, Pearson_Residuals=Pearson_Residuals, 
            Deviance_Residuals=Deviance_Residuals, Anscombe_Residuals=Anscombe_Residuals))
}

#my.poisson.glm(K,y)


#

#----------------------------------------------------------------

# PROBLEM 2: Description of the discoveries data. Start by loading the data:
library(faraway)  				# Install this package if needed
data(discoveries)					# Load the discoveries data.
# Type "help(discoveries)" for details.
class(discoveries)				# What type is the dataset?

mean(discoveries)
max(discoveries)
min(discoveries)

which(discoveries==max(discoveries))

which(discoveries==min(discoveries))

plot(discoveries)

qqnorm(discoveries)
qqline(discoveries)
# Plot the data and make a summary of data (average, max, min, etc). 
# Is there any special pattern (trend, seasonality)?

# SOLUTION:
#Comment
#The average number of discoveries year 1860~1959 is 3.1. 
#The minimum number and maximum number of discoveries is 0(multiple observations) and 12(1866).  
#The number of discoveries seems to fluctuate and a rise\fall is followed by
#a fall\rise, but the trend of variation seems to decrease.
#Because we do not have enough seasonal data, seasonal variation can't be analized.
#By the last two orders, we can see that the data can not be seemed as normally distributed over time.

#----------------------------------------------------------------

# PROBLEM 3: We study if the association between the number of discoveries
# and time. We first make the variable
years <- 1860:1959
# We now fit the Poisson GLM "my.poisson.glm()" with the canonical link.
# We shall use "discoveries" as the response and "years" as the predictor. 
# Make sure to include the intercept in the regression. Provide all the 
# output of the regression. Compare your output with that from R:
# 	modl <- glm(discoveries~years,family=poisson(link = "log"))
# 	summary(modl)
# In particular, provide the Pearson residuals and the Anscombe residuals
# from the fitted model "modl"

# SOLUTION:

install.packages("wle")
library(wle)   

my.poisson.glm(as.matrix(years),discoveries)

lmod <- glm(discoveries~years,family=poisson(link = "log"))
summary(lmod)
Pearson_Residuals <- residuals(lmod, "pearson")
Anscombe_Residuals<-residualsAnscombe(discoveries,mu=modl$fitted.values,family=poisson())


#----------------------------------------------------------------