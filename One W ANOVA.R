#### Sample Code for One-Way ANOVA

#### Labor Data. Collected from East Germany in 1994. See page 36 of Fan and Li notes. 
labor <- read.table("http://www.unc.edu/~zhangk/STOR665Spring15/labor.dat", header=T)

options(width=200)
dim(labor)  					### 607 observations and 9 variables
head(labor)
labor[1:15,]
earning <- labor[,3]			### Hourly Salary

edu <- labor[,5]				### Years of Education: A for Graduate education.
eduf <- rep("A", length(edu))
eduf[edu<16] <- "B"				### B for College education
eduf[edu<13] <- "C"				### C for High School education
eduf[edu<12] <- "D"				### D for not finishing High School
eduf <- factor(eduf)			### Convert eduf to be a factor, see the table below

laborf <- data.frame(earning, eduf)

windows(20,12)
par(mfrow=c(2,2),mar=c(3,3,2,2),mgp=c(1.8,0.5,0))
plot.design(laborf)				### Comparison the means
title("Comparison of the means")
plot.design(laborf, fun=median)	### Comparing the medians
title("Comparison of the medians")
plot.design(laborf, fun=var)	### Comparing the variances
title("Comparison of the variance")
boxplot(earning~eduf)			### Outlier seen in Group C!
title("Boxplots for each treatment")	

labor[which(earning==max(earning)),]### Does this person look normal?

summary(aov(earning~eduf))		###  F stat for ANOVA, I=4, n=607
summary(lm(earning~eduf))		###  ANOVA as a regression

I <- nlevels(eduf)				###  Number of levels in eduf
n <- dim(labor)[1]				###  Number of observations in labor
m <- I*(I-1)/2					###  Number of pairwise comparisons
a <- 0.05						###	 alpha

qt(1-a/2/m,n-I)   				###  Coefficient for Bonferroni intervals
sqrt((I-1)*qf(1-a,I-1,n-I))		###  Coefficient for Scheffe intervals
qtukey(1-a,I,n-I)/sqrt(2)		###  Coefficient for Tukey intervals