## -------------------------------------------- ##
## EDSD 2020-2021: Population projections
## Lecture 1
## Introduction to population projections
## Date: 07/06/2020
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

## let's write a function for our constant exponential growth model
PopProj <- function(N0,r,t){
  NT <- N0*(1+r)^t
  return(NT)
}

## let's compute the projected population at each time t
Na <- PopProj(N0_a,r_a,t)
Nb <- PopProj(N0_b,r_b,t)

## computing the time point when Nb is greater than Na
t_hat <- t[which(Nb>Na)[1]]

## plotting the results
plot(t,Na,t="n",lwd=2,ylim = range(Na,Nb),
     ylab="population size", xlab="time (years)",
     main = "first example of constant exponential growth model")
grid()
lines(t,Na,col=1,lwd=2)
lines(t,Nb,col=2,lwd=2)
abline(v=t_hat,lty=2)
legend("topleft",c("Pop A","Pop B"),col=1:2,lwd=2,cex=1.5)


##----- EXERCISE 2 --------
rm(list=ls(all=TRUE))

## let's define the input of our exercise
N0_a <- 50
N0_b <- 35
t <- seq(0,10,0.01)
b_a <- m_b <- 0.03
d_a <- 0.05
m_a <- 0.01
b_b <- 0.07
d_b <- 0.04

## let's write a function for our constant exponential growth model
PopProj <- function(N0,b,d,m,t){
  r <- b - d + m
  NT <- N0*(1+r)^t
  return(NT)
}

## let's compute the projected population at each time t
Na <- PopProj(N0_a,b_a,d_a,m_a,t)
Nb <- PopProj(N0_b,b_b,d_b,m_b,t)

## computing the time point when Nb is greater than Na
(t_hat <- t[which(Nb>Na)[1]])

## plotting the results
plot(t,Na,t="n",lwd=2,ylim = range(Na,Nb),
     ylab="population size", xlab="time (years)",
     main = "first example of constant exponential growth model")
grid()
lines(t,Na,col=1,lwd=2)
lines(t,Nb,col=2,lwd=2)
abline(v=t_hat,lty=2)
legend("topleft",c("Pop A","Pop B"),col=1:2,lwd=2,cex=1.5)


##----- EXERCISE 3 --------
rm(list=ls(all=TRUE))

## let's define the input of our exercise
N0 <- 50
t <- 1:21   ## one more than projection year (because we start from 1 rather than 0)
b <- 0.05
d <- 0.04
e <- 0.01
I <- 1.5

## let's write a function for our constant exponential growth model
PopProj <- function(N0,b,d,e,I,t){
  r <- b - d - e
  NT <- rep(NA,t)
  NT[1] <- N0
  for (i in 2:t){
    NT[i] <- NT[i-1] * (1+r) + I
  }
  return(NT)
}

## let's compute the projected population at each time t
NT <- PopProj(N0,b,d,e,I,max(t))

## plotting the population
plot(t,NT,t="l",lwd=2,ylim = range(NT),
     ylab="population size", xlab="time (years)",
     main = "")

## INCORPORATING FUTURE ASSUMPTIONS
e <- c(rep(0.01,11),rep(0.02,10))
I <- c(rep(1.5,10),seq(1.5,0,length.out = 11))
y <- t-1  ## x-axis (start from 0)

## plotting our assumptions
par(mfrow=c(1,2))
plot(y,e,main="emigration rate",xlab="time (years)");abline(v=10,lty=2)
plot(y,I,main="immigration counts",xlab="time (years)");abline(v=10,lty=2)
par(mfrow=c(1,1))

## let's write a function for our constant exponential growth model
PopProj <- function(N0,b,d,e,I,t){
  NT <- rep(NA,t)
  NT[1] <- N0
  for (i in 2:t){
    r <- b - d - e[i]
    NT[i] <- NT[i-1] * (1+r) + I[i]
  }
  return(NT)
}

## let's compute the projected population at each time t
## in this scenario
NT_scenario <- PopProj(N0,b,d,e,I,max(t))

## plotting the population
plot(y,NT,t="n",xlab="time (years)",ylab="population size")
grid()
lines(y,NT,lwd=2)
lines(y,NT_scenario,col=2,lwd=2,lty=2)
abline(v=10,lty=2)
legend("topleft",c("no change","migration scenario"),col=1:2,lwd=2,lty=c(1,2))

## plotting only the last ten years
lines(y[11:20],NT_scenario[11:20],col=3,lwd=2,lty=2)

## END