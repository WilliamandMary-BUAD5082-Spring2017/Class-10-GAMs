rm(list=ls())
require(gam)
#########################
#########Section 1  #####
#########################
# We fit a GAM to predict wage using natural spline functions of year
# and age, treating education as a qualitative predictor. Since
# this is just a big linear regression model using an appropriate choice of
# basis functions, we can do this using the lm() function.
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
# Conveniently, even though gam1 is not of class gam but rather of class lm, 
# we can still use plot.gam() on it.
par(mfrow=c(1,3))
plot.gam(gam1, se=TRUE, col="red")
#########################
#########Section 2  #####
#########################
# We now fit the model using smoothing splines rather than natural
# splines. Use the gam() function to fit more general sorts of GAMs, using smoothing splines
# or other components that cannot be expressed in terms of basis functions
# and then fit using least squares regression.
#
# The s() function, which is part of the gam library, is used to indicate that
# we would like to use a smoothing spline. We specify that the function of
# year should have 4 degrees of freedom, and that the function of age will
# have 5 degrees of freedom. Since education is qualitative, we leave it as is,
# and it is converted into four dummy variables.
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
plot(gam.m3, se=TRUE,col="blue")
# In these plots, the function of year looks rather linear. We can perform a
# series of ANOVA tests in order to determine which of these three models is
# best: 
#   a GAM that excludes year (M1)
#   a GAM that uses a linear function of year (M2)
#   a GAM that uses a spline function of year (M3 above).
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
# We find that there is compelling evidence that a GAM with a linear function
# of year is better than a GAM that does not include year at all
# (p-value=0.00014). However, there is no evidence that a non-linear function
# of year is needed (p-value=0.349). In other words, based on the results
# of this ANOVA, M2 is preferred.
summary(gam.m3)
# The summary() function produces a summary of the gam fit.
# The p-values for year and age correspond to a null hypothesis of a linear
# relationship versus the alternative of a non-linear relationship. The large
# p-value for year reinforces our conclusion from the ANOVA test that a linear
# function is adequate for this term. However, there is very clear evidence
# that a non-linear term is required for age.
#
# We can make predictions from gam objects, just like from lm objects,
# using the predict() method for the class gam. Here we make predictions on
# the training set.
preds=predict(gam.m2,newdata=Wage)
#
# We can also use local regression fits as building blocks in a GAM, using
# the lo() function. Here we have used local regression for the age term, with a span of 0.7.
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
plot.gam(gam.lo, se=TRUE, col="green")
# We can also use the lo() function to create interactions before calling the
# gam() function.
# Next fit a two-term model, in which the first term is an interaction between
# year and age, fit by a local regression surface. We can plot the resulting
# two-dimensional surface if we first install the akima package.
gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)
require(akima)
plot(gam.lo.i)
# In order to fit a logistic regression GAM, we once again use the I() function
# in constructing the binary response variable, and set family=binomial.
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")
table(Wage$education,I(Wage$wage>250))
# There are no high earners in the <HS category:
gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")
#########################
#########Section 3  #####
#########################
# ISLR Chapter 7 Exercise 11 
set.seed(0)
# Part (a):
n = 100
X1 = rnorm( n )
X2 = rnorm( n )
# the true values of beta_i:
beta_0 = 3.0
beta_1 = 5.0
beta_2 = -0.2
Y = beta_0 + beta_1 * X1 + beta_2 * X2 + 0.1 * rnorm( n )
# Part (b):
#
beta_1_hat = -3.0
# Part (c)
a = Y - beta_1_hat * X1
beta_2_hat = lm( a ~ X2 )$coef[2]
# Part (d)
n_iters = 10
beta_0_estimates = c()
beta_1_estimates = c()
beta_2_estimates = c()
for( ii in 1:n_iters ){
  
  a = Y - beta_1_hat * X1
  beta_2_hat = lm( a ~ X2 )$coef[2] 
  
  a = Y - beta_2_hat * X2
  m = lm( a ~ X1 )
  beta_1_hat = m$coef[2]
  
  beta_0_hat = m$coef[1] 
  
  beta_0_estimates = c(beta_0_estimates, beta_0_hat)
  beta_1_estimates = c(beta_1_estimates, beta_1_hat)
  beta_2_estimates = c(beta_2_estimates, beta_2_hat)
}
# Get the coefficient estimates using lm:
#
m = lm( Y ~ X1 + X2 )

###Backfitting iterations are black dots
###True value of the parameters are the horizontal green lines
###lm()'s estimated coefficients are the horizontal gray lines
old_par = par(mfrow=c(1,3))
plot( 1:n_iters, beta_0_estimates, main='beta_0', pch=19, ylim=c(beta_0*0.999,max(beta_0_estimates)) )
abline(h=beta_0,col='green',lwd=4)
abline(h=m$coefficients[1],col='gray',lwd=4)
grid()

plot( 1:n_iters, beta_1_estimates, main='beta_1', pch=19 )
abline(h=beta_1,col='green',lwd=4)
abline(h=m$coefficients[2],col='gray',lwd=4)
grid()

plot( 1:n_iters, beta_2_estimates, main='beta_2', pch=19 )
abline(h=beta_2,col='green',lwd=4)
abline(h=m$coefficients[3],col='gray',lwd=4)
grid()

par(old_par)

