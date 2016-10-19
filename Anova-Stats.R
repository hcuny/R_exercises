# This homework is an exercise on ANOVA.  We will
# build up R code that 1) produces important ANOVA statistics, and
# 2) perform ANOVA on a real dataset.

# We will be negligent in the sense that we will ignore precautions 
# that a numerical analyst would build into his code, and we will not
# consider measures to improve numerical conditioning such as pivoting
# (switching the order of the variables) either.

#----------------------------------------------------------------

# PROBLEM 1: Description of the coagulation data. Start by loading the data:
# There are four groups of observations in the data. Make four plots comparing
# the mean, median, variance, and distribution of the observations within the
# four groups. Make comments.

# SOLUTION:


library(faraway)  				# install this package if needed
attach(coagulation)				# load the coagulation data.
coagulation						# Type help(coagulation) for more details
coag;diet

coagdf<-data.frame(coagulation)

windows(20,12)
par(mfrow=c(2,2),mar=c(3,3,2,2),mgp=c(1.8,0.5,0))
plot.design(coagdf)  			### Comparison the means
title("Comparison of the means")
plot.design(coagdf, fun=median)	### Comparing the medians
title("Comparison of the medians")
plot.design(coagdf, fun=var)	### Comparing the variances
title("Comparison of the variance")
boxplot(coag~diet)			### Outlier seen in Group C!
title("Boxplots for each treatment")	

#Comments:
#1.Groups in the middle hava higher means(group C highest) than groups in the ends(Group A&B)
#2.The distribution of medians is similar to the distribution of means.
#3.Gropu B & D have a higher variance compared to group A & C,
#but it may have something to do with different number of observations in each group.
#4.An outlier is found in group C, while no outlier is found in other groups.
#----------------------------------------------------------------

# PROBLEM 2: Write a function my.anova(x,y) that takes x as a vector indicating
# the groups and y as a vector with observations. The function then computes a 
# list with the following named elements:

# a) "Group_Mean": a matrix with one row per group and columns
#    named "Mean" and "Std.Err.Est"
# b) "RMSE": a number, s
# c) "SSE": a number, SSE
# d) "df_SSE": a number, df_SSE
# e) "SST": a number, SST
# f) "df_SST": a number, df_SST
# g) "F": a number, F
# h) "p-value": a number, pv, for the F-test

# You may use matrix multiplication if convenient, but no high-level
# functions other than things like sqrt().

# Try your solution on the coagulation data and show the full result:
#   my.anova(diet,coag)
# Compare with results from the canned functions
#   anova(lm(coag~diet))
# and
#   oneway.test(coag~diet,var.equal=T)
# You need to get the exact numbers for the F stat and the p-value.
# Are the group means all equal?

# SOLUTION:

my.anova<-function(x,y){
  M<-data.frame(x,y)    
  n=length(M[,1])       #number of total observations
  I=nlevels(M[,1])       #number of groups
  level<-levels(M[,1])   #attributes of different groups
  K<-0
  
Group_mean<-matrix(nrow=I,ncol=2)  
colnames(Group_mean)<-c('Mean','Std.Err.Est') 
rownames(Group_mean)<-c(LETTERS[1:I])     

for (i in 1:I){ 
  v=as.numeric(M[M$x==level[i],]$y) #denote column y in each group as a vector for convenience of computation
  Group_mean[i,1]=mean(v) #get subset of each group and compute mean
  Group_mean[i,2]=sqrt(var(v))#get subset of each group and compute standard error      
  K=K+var(v)*(length(v)-1) 
}                    

SSE=K        
s=sqrt(SSE/(n-I))       #rmse
RSS0=var(M$y)*(n-1)     #RSS0, total sum of squares
SST=RSS0-SSE           #sum of square due to treatment
df_SSE=n-I                 #degree of freedom of SSE
df_SST=I-1                 #degree of freedom of SST

F_value=(SST/df_SST)/(SSE/df_SSE)   #compute F statistic
p=pf(F_value, df_SST,df_SSE, lower.tail=F) #compute corresponding p value

print(Group_mean)
cat("rmse is",s,collapse=", ")
cat("SSE is:",SSE,collapse=", ")
cat("SST is:",SST,collapse=", ")
cat("F_value is:",F_value,collapse=", ")
cat("p value is", p)                        
}                     

my.anova(diet,coag)     #try the coagulation data on my_anova

anova(lm(coag~diet))
oneway.test(coag~diet,var.equal=T)

#Comment: We get exactly the same statistic values in functions above,
#Since p value is very small, we will reject the null hypothesis, i.e.,
#the group means are not equal.
 

#----------------------------------------------------------------

# PROBLEM 3: Test the equality of variance through the Bartlett test.
#   bartlett.test(coag~diet)
# Can we assume that the group variances are all equal?

# If not, then the test of equality of group means can be done by
#  oneway.test(coag~diet)
# Compare this test with the same test in Problem 2 assuming an equal
# variance. Make comments.

# SOLUTION:
bartlett.test(coag~diet)
#Since the p value is big, we tend to accept the null hypothesis.

my.anova(diet,coag) 
oneway.test(coag~diet)
#Comment:
#oneway.test() has a larger F value, and also a larger p value, 
#the group variances may have effect on comparing group means. 


#----------------------------------------------------------------

# PROBLEM 4: Construct the Tukey CIs for the contrasts.
# Which group means are different if any?

# SOLUTION:
TukeyHSD(aov(coag~diet),conf.level=0.95)  
#From the output, we can see that the difference between B-A, C-A, D-B, D-c are significant under level
#alpha=0.05.

#----------------------------------------------------------------