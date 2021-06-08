## -------------------------------------------- ##
## EDSD 2020-2021: Population projections
## Lecture 1 - extra script
## Introduction to population projections
##
## Comparison between true and approximated formulas
## of the constant exponential growth model
##
## Date: 08/06/2020
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##

##----- EXERCISE 1 --------
rm(list=ls(all=TRUE))

## let's define the input of our exercise
N0_a <- 50
N0_b <- 35
t <- seq(0,10,0.01)
r_a <- 0.05
r_b <- 0.15

## let's write two functions for our constant exponential growth model
## this is the approximated version
PopProj <- function(N0,r,t){
  NT <- N0*(1+r)^t
  return(NT)
}

## this is the actual formula
PopProj_v2 <- function(N0,r,t){
  NT <- N0*exp(r*t)
  return(NT)
}

## let's compute the projected population at each time t
## using the two formulas
Na <- PopProj(N0_a,r_a,t)
Nb <- PopProj(N0_b,r_b,t)
Na2 <- PopProj_v2(N0_a,r_a,t)
Nb2 <- PopProj_v2(N0_b,r_b,t)

## computing the time point when Nb is greater than Na
(t_hat <- t[which(Nb>Na)[1]])
(t_hat2 <- t[which(Nb2>Na2)[1]])

## plotting the results
plot(t,Na,t="n",lwd=2,ylim = range(Na,Nb),
     ylab="population size", xlab="time (years)",
     main = "first example of constant exponential growth model")
grid()
lines(t,Na,col=1,lwd=2)
lines(t,Na2,col=3,lwd=2,lty=2)
lines(t,Nb,col=2,lwd=2)
lines(t,Nb2,col=4,lwd=2,lty=2)
abline(v=t_hat,lty=2)
abline(v=t_hat2,lty=2)
legend("topleft",c("Pop A","Pop B"),col=1:2,lwd=2,cex=1.25,title = "Approximated:")
legend("bottomright",c("Pop A","Pop B"),col=3:4,lwd=2,lty=2,cex=1.25,title = "True:")
