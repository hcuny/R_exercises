#### Sample Code for Two-Way ANOVA

#### Battery Data. Life time per dollar of batteries from Dean and Voss (1999). 
battery <- read.table("http://www.unc.edu/~zhangk/STOR665Spring15/battery.dat", header=T)
battery											### Balanced design. Four observations in each Duty/Brand cell.

interaction.plot(battery$Brand,battery$Duty,battery$Cost)
title("Interaction Plot", sub="Battery")

options()$contrasts    							### Current contrasts. The first for unordered and the second for ordered.
options(contrasts=c("contr.sum", "contr.sum")) ### Setting the sum of alphas, betas, and gammas to be 0.
summary(aov(Cost~Brand*Duty, data=battery)) 	### Complete model
summary(lm(Cost~Brand*Duty, data=battery)) 		### Estimates of mu, alphas, betas, and gammas.

table(battery$Brand,battery$Duty)

model.matrix(aov(Cost~Duty+Brand,data=battery)) ### In balanced designs, the matrix for the two variables is orthogonal.
summary(aov(Cost~Brand+Duty, data=battery)) 	### So the ANOVA tables are the same.
summary(aov(Cost~Duty+Brand, data=battery))   ###
summary(lm(Cost~Brand+Duty, data=battery)) 		### Estimates of mu, alphas, and betas
summary(lm(Cost~Brand*Duty, data=battery)) 

options(contrasts=c("contr.treatment", "contr.treatment")) ### Set the first level (alpha1, beta1) of each factor to be 0
model.matrix(aov(Cost~Duty+Brand,data=battery)) 
summary(aov(Cost~Duty+Brand, data=battery)) 	### The ANOVA table remains the same
summary(lm(Cost~Duty+Brand, data=battery))		### Coefficient estimates.
summary(lm(Cost~Duty*Brand, data=battery))
summary(lm(Cost~Brand*Duty, data=battery))



#### Female Labor Supply Data
labor <- read.table("http://www.unc.edu/~zhangk/STOR665Spring15/labor.dat", header=T)
head(labor)
earning <- labor[,3]
edu <- labor[,5]
eduf <- rep("A", length(edu))
eduf[edu<16] <- "B"
eduf[edu<13] <- "C"
eduf[edu<12] <- "D"
eduf <- factor(eduf)
child <- factor(labor[,8])
table(child,eduf) 								### Unbalanced design

interaction.plot(eduf,child,earning)
title("Interaction Plot", sub="Female Labor Supply")

options(contrasts=c("contr.sum", "contr.sum"))
summary(aov(earning~child+eduf))				### Different SS
summary(aov(earning~eduf+child))         ##unbalanced can't use

### Adding interaction
summary(aov(earning~child*eduf))				### Different SS for child and eduf. same interaction
summary(aov(earning~eduf*child))

### Coeffcient estimates are the same
summary(lm(earning~child+eduf))
summary(lm(earning~eduf+child))
summary(lm(earning~child*eduf))
summary(lm(earning~eduf*child))

#### alloy data
alloy <- matrix(scan(file="http://www.unc.edu/~zhangk/STOR665Spring15/alloy.txt"), ncol=7, byrow=T)
Strength <- as.vector(alloy[,2:7])
Lab <- factor(c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4), rep(6,4)))
Bar <- factor(rep(1:4,6))
alloy <- data.frame(Strength, Bar, Lab)
alloy										### One observation per Lab/Bar cell

interaction.plot(Bar,Lab,Strength)
title("Interaction Plot", sub="Alloy")

summary(aov(Strength~Bar+Lab, alloy))
summary(aov(Strength~Bar*Lab, alloy))

###
Lab.mean <- c(mean(alloy$Strength[alloy$Lab==1]), mean(alloy$Strength[alloy$Lab==2]),mean(alloy$Strength[alloy$Lab==3]),mean(alloy$Strength[alloy$Lab==4]),mean(alloy$Strength[alloy$Lab==5]),mean(alloy$Strength[alloy$Lab==6]))
Bar.mean <- c(mean(alloy$Strength[alloy$Bar==1]), mean(alloy$Strength[alloy$Bar==2]),mean(alloy$Strength[alloy$Bar==3]),mean(alloy$Strength[alloy$Bar==4]))

tukey.1df <- function(y, factorA, factorB) {
factorA <- as.factor(factorA)
factorB <- as.factor(factorB)
a <- length(unique(factorA))
b <- length(unique(factorB))
Y <- matrix(nrow=a, ncol=b)
for(i in 1:a) {for (j in 1:b) Y[i,j] <- y[factorA == unique(factorA)[i] & factorB == unique(factorB)[j]]}
Ameans <- apply(Y, 1, mean)
Bmeans <- apply(Y, 2, mean)
term1 <-  t(Ameans)%*%Y%*%Bmeans
y.. <- mean(Y)
SSA <- b*sum((Ameans-y..)^2)
SSB <- a*sum((Bmeans-y..)^2)
term2 <- SSA + SSB + a*b*y..^2
SSN <- a*b*((term1 - y..*term2)^2)/(SSA*SSB)
SST <- sum((Y-y..)^2)
SSE <- SST-SSN-SSA-SSB

SS <- c(SSA, SSB, SSN, SSE, SST)
df <- round(c(a-1, b-1, 1, (a-1)*(b-1)-1, a*b-1))
MS <- SS[1:4]/df[1:4]
F0 <- c(MS[1]/MS[4], MS[2]/MS[4], MS[3]/MS[4])
p <- vector(length=3)
for(i in 1:3) p[i] <-1-pf(F0[i], df[i], df[4])

SS <- round(SS,3)
MS <- round(MS,3)
F0 <- round(F0,3)
p <- round(p,4)

MS <- c(MS, " ")
F0 <- c(F0, " ", " ")
p <- c(p, " ", " ")

out <- cbind(SS, df, MS, F0, p)
dimnames(out) <- list(c("A","B","N","Err","Tot"),c("SS","df","MS", "F0", "p"))
#print.matrix(out, quote=F)
list(out = out)
}
tukey.1df(alloy$Strength, alloy$Bar, alloy$Lab)


