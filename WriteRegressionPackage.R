
# This homework is an exercise in Least Squares computation.  We will
# build up R code that 1) computes a Q-R decomposition of a matrix X,
# 2) solves equations when the coefficients are upper triangular,
# 3) inverts an upper triangular matrix R, and finally
# 4) computes and returns most things we find useful in regression.
# We will be negligent in the sense that we will ignore precautions 
# that a numerical analyst would build into his code, and we will not
# consider measures to improve numerical conditioning such as pivoting
# (switching the order of the variables) either.


# BACKGROUND: Most LS solvers are based on decompositions of the
# design matrix of the form X = Q R, where R is square and triangular
# and Q has orthonormal columns.  The idea is that the LS criterion
# can be written as follows:
#      | y - X b |^2  =  | Q^T y - Q^T X b |^2  +  | r |^2
#                     =  | Q^T y - R b |^2      +  | r |^2
# where the first term can be forced to zero by solving  
#      Q^T y  =  R b
# This equation is easily solved when  R  is triangular.
# Furthermore, for inference we want the matrix (X^T X)^{-1},
# but with a Q-R decomposition this is easily gotten by
# inverting R because
#   (X^T X)^{-1}  =  R^{-1} R^{-1}^T


#----------------------------------------------------------------

# PROBLEM 1: Write a function 'gs(X)' to implement the so-called
# modified Gram-Schmidt procedure for orthnormalizing an nxp matrix.
# This procedure consists of orthnormalizing the columns of X to form
# the matrix Q of the same size, and storing the coefficients to
# reconstitute X from Q in an upper triangular matrix R.

# Here is the algorithm:
#   Initialize Q with a copy of X
#   Loop over the columns of Q and do the following:
#     At stage j,
#       normalize Q[,j] in place
#       adjust the columns of Q[,(j+1):p] for Q[,j]
#       store the original length of Q[,j] in R[j,j]
#         and the coefficients from adjustment properly in row R[j,].
# If you do this right, it should hold that  X = Q R.
# Finish the function gs() with  return(list(Q=Q, R=R))

# In this algorithm, use loops and sum() to calculate inner products
# and sqrt() for lengths, but not lm() or any other high-level function.

# Test your function on these data:
#   X <- cbind(rep(1,10), 1:10, (1:10)^2)
#   sol <- gs(X)
# Show these and comment on them:
#   sol$Q %*% sol$R
#   t(sol$Q) %*% sol$Q

# SOLUTION: 


norm_vec <- function(x) {
  sqrt(sum(x^2))}    #First define norm function of a vector we will use later


gs<-function(A){
  n=nrow(A)
  p=ncol(A)
  R=matrix(nrow=p,ncol=p)
  U=matrix(ncol=p,nrow=n)
  E=matrix(ncol=p,nrow=n)
  U[,1]=A[,1]  #initialization U[,1]
  E[,1]=U[,1]/norm_vec(U[,1]) #initialization Q[,1]
  for(j in 1:p){
    R[1,j]=sum(A[,j]*E[,1])
  }    
  for (i in 2:p){           #recursively compute each column of Q using Gram-Schmidt method
    s=rep(0,n)
    for(k in 1:(i-1)){           
      s=s+sum(A[,i]*E[,k])*E[,k]
    }
    U[,i]<-A[,i]-s
    E[,i]<-U[,i]/norm_vec(U[,i])  
    for (j in i:p){
      R[i,j]=sum(A[,j]*E[,i]) #recursively compute each column of R using Gram-Schmidt method
    }
    for (j in 1:i-1){
      R[i,j]=0
    }
  }
  return(list(Q=E, R=R))
}

#Verify
X <- cbind(rep(1,10), 1:10, (1:10)^2)
sol<-gs(X)
sol$Q %*% sol$R
t(sol$Q) %*% sol$Q


#----------------------------------------------------------------

# PROBLEM 2: Write function 'tri.solve(R,z)' that accepts an upper
# triangular matrix R and a colum z and returns the solution b of z =
# R b.  Show the results for these inputs:

#   z <- 10:1
#   R <- (row(diag(10)) <= col(diag(10)))

# You may use loops and the sum() function inside tri.solve() to spare
# yourself an inner loop, but you must not use canned solvers such as
# solve() or ginv() (the latter from the MASS package).

# SOLUTION:

tri.solve<-function(R,b){
  n=length(b)  
  z=array()
  z[n]=b[n]/R[n,n]          #Start from the bottom of the upper triangular matrix,
  
  for(j in (n-1):1){         #to solve the equations recursively
    t=0
    for (i in (j+1):n){
      t=t+R[j,i]*z[i]
    }
    z[j]=(b[j]-t)/R[j,j]
  }
  
  print (z)
}




#----------------------------------------------------------------

# PROBLEM 3: No programming, just thinking.
# Assume R and R1 are upper triangular square matrices.
# Questions:
# a) If the vector b is has non-zero values only in the first k entries,
#    what can one say about z=Rb?
     
# b) When is R invertible?
     
# c) If the vector z has non-zero values only in the first k entries,
#    and if R is invertible, what can one say about the vector b?
    
# d) What can one say about R1%*%R?
    
# e) What can one say about R^{-1}?
     
# SOLUTION:

# a) 
# z has no zero values only in the first k entries.
# b) 
#The elements in diagonol are non-zero.
# c)
#b has no zero values only in the first k entries
# d)
#It's Upper triangular.
# e) 
#It's Upper triangular.


#----------------------------------------------------------------

# PROBLEM 4: For inference about the regression coefficients, we need
# the matrix (X^T X)^{-1}.  In preparation for it, we need the
# inversion of an upper triangular matrix R because from a Q-R
# decomposition X=QR we can easily (X^T X)^{-1} if we have R^{-1}.
# Package the inversion in a function inv(R).  Try it out on the R
# matrix of the above 'sol':
#   inv(sol$R)
#   inv(sol$R) %*% sol$R
# Show both and comment on both.

# SOLUTION:


#use the function defined in Q2

inv<-function(R){
  n=ncol(R)
  V=matrix(ncol=n,nrow=n)
  I=diag(n)
  for (i in 1:n){
    V[,i]<-tri.solve(R,I[,i]) #Solve each equation to get each column of V.
  }
  return(V)

}
#Verify
inv(sol$R)
inv(sol$R)%*%sol$R



#----------------------------------------------------------------

# PROBLEM 5: Using tri.solve(R,z), inv(R) and gs(X,y), write a
# function reg(X,y) that computes a list with the following named
# elements:

# a) "Coeffs": a matrix with one row per regression coefficient and columns
#    named "Coefficient", "Std.Err.Est", "t-Statistic", "P-Value"
# b) "Var": the coefficient variance matrix  s^2*(X^T X)^{-1}
# c) "RMSE": a number, s
# d) "R2": a number, R2
# e) "Residuals": the vector of residuals

# Assume that X does not have a column of 1's but an intercept is desired,
# hence first thing in the function do  X <- cbind(Icept=1,X).

# You may use matrix multiplication if convenient, but no high-level
# functions other than things like sqrt() and pt().

# Try your solution on the following data and show the full result:
#   X <- cbind(Pred1=1:10, Pred2=(1:10)^2)
#   y <- rep(1,10) + 2*X[,1] + 3*X[,2] + resid(lm(rep(0:1,5)~X))
#   reg(X,y)
# Compare with results from the canned regression function
#   summary(lm(y~X))
# No need to comment, but you need to get the exact numbers.

# Compare the function execution time. Your function should be much faster.
#   system.time(for (i in 1:100){reg(X,y)})
#   system.time(for (i in 1:100){lm(y~X)})

# SOLUTION:


reg<-function(X,y){
  X <- cbind(Icept=1,X)
  n=nrow(X)
  p=ncol(X)
  QR=gs(X)   #QR decomposition of X
  R=QR$R
  Q=QR$Q
  
  RM<-matrix(nrow=p,ncol=4)  #the required matrix "Coeffs"
  
  colnames(RM) <-c("Coefficient","Std.Err.Est","t-Statistic","P-Value")  #define column names
  prefix<-"Coef"  #define row names
  suffix<-seq(1:p)
  a<-paste(prefix, suffix) 
  rownames(RM) <-noquote(a) 
  
  RM[,1]=inv(R)%*%t(inv(R))%*%t(X)%*%y  
  s=sqrt(sum((y-X%*%RM[,1])^2)/(n-p))  #Compute standard error s
  RM[,2]=sqrt(s^2*diag(inv(R)%*%t(inv(R))))
  RM[,3]=RM[,1]/RM[,2]
  RM[,4]=(1-pt(RM[,3],df=n-p))*2
  
  SSTO=sum((y-mean(y))^2)
  SSR=sum((X%*%RM[,1]-mean(y))^2)
  print("Covariance Matrix is",quote=FALSE)
  print(s^2*inv(R)%*%t(inv(R)))
  print(c("RMSE is", s),quote=FALSE)
  print("R2 is", quote=FALSE)
  print(SSR/SSTO)
  print("Residual is",quote=FALSE)
  print(t(y-X%*%RM[,1]))
  return(RM)
}



X <- cbind(Pred1=1:10, Pred2=(1:10)^2)
y <- rep(1,10) + 2*X[,1] + 3*X[,2] + resid(lm(rep(0:1,5)~X))

reg(X,y)
summary(lm(y~X))

#system.time(for (i in 1:100){reg(X,y)})
#system.time(for (i in 1:100){lm(y~X)})
