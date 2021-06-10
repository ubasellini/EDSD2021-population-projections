## -------------------------------------------- ##
## EDSD 2020-2021: Population projections
## Lecture 4
## Extensions of Matrix Projections
## Date: 10/06/2021
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##


##----- EXERCISE 1: RC migration schedule --------
rm(list=ls(all=TRUE))

## load the migest package
library(migest)

## check the fundamental RC parameters
rc9.fund

## five-year age groups (works well also with one-year)
x <- seq(0,85,5)
mx <- rc9(x, param = rc9.fund)
plot(x, mx, type="o",pch=16)

## assume a total of 100,000 net migration counts
I <- 1e5
Ix <- I*mx
sum(Ix)
plot(x, Ix, type="o",pch=16,
     xlab = "Age group",ylab= "Net migrant counts",
     main="RC migration schedule for 100,000 net migrants")


##----- EXERCISE 2: Projections with open population --------
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)
library(viridis)
library(migest)

## set working directory in active folder (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## loading the data 
dir()
load("EDSD.lecture3.Rdata")  

## derive the net female migrants by age (RC schedule)
I <- 25000
x <- dta.swe$Age
mx <- rc9(x=dta.swe$Age, param = rc9.fund)
Ix <- I*mx

## let's do the same, but with a function instead
## function to project several periods
pop.proj.MIG <- function(x,AgeGroup,Nx,sx,bx,Ix,n){
  ## number of age groups
  m <- length(x)
  ## create Leslie matrix
  L <- matrix(0,m,m)
  L[1,] <- bx
  diag(L[-1,]) <- sx
  L[m,m] <- sx[m-1]
  ## create population matrix
  N <- matrix(0,m,n+1)
  N[,1] <- Nx
  for (i in 1:n){
    N[,i+1] <- L%*%(N[,i]+Ix/2) + Ix/2
  }
  out <- cbind(data.frame(x=x,AgeGroup=AgeGroup),N)
  return(out)
}

## define projection period
n <- 20
my.proj.base <- pop.proj(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,
                         Nx=NFx,sx=sFx,bx=bFx,n=n)
my.proj.migr <- pop.proj.MIG(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,
                         Nx=NFx,sx=sFx,bx=bFx,Ix,n=n)

## long data format
dta.swe.l <- my.proj.base %>%
  pivot_longer(-c(x,AgeGroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year = 1993 + (period-1)*5,
         YearF=as.factor(Year),
         type="no migration")
dta.swe.l.mig <- my.proj.migr %>%
  pivot_longer(-c(x,AgeGroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year = 1993 + (period-1)*5,
         YearF=as.factor(Year),
         type="with migration")

## combine the long data
dta.swe.all <- dta.swe.l %>% 
  bind_rows(dta.swe.l.mig)

## plotting with pyramid
ggplot(dta.swe.all,aes(x=AgeGroup,y=population,fill=type)) +
  geom_bar(data = subset(dta.swe.all, period == 21),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population 2093") +
  scale_fill_manual(name = 'Assumption', values=c("#E69F00", "#56B4E9"))

## alternative visualization with overlapping bars
ggplot(dta.swe.all,aes(x=AgeGroup,y=population,fill=type)) +
  geom_bar(data = subset(dta.swe.all, period == 21 & type == "no migration"),
           stat = "identity",position = "dodge",size=0.5) +
  geom_bar(data = subset(dta.swe.all, period == 21 & type == "with migration"),
           stat = "identity",position = "dodge",color = "black",size=2) +
  coord_flip() +
  theme_bw() +
  ggtitle(paste("Swedish female population, year",
                subset(dta.swe.l,period==21)$Year)) +
  scale_fill_manual(name = 'Year', 
                    values=c("#E69F00", NA))


##----- EXERCISE 3: TIME-VARYING assumptions on TFR --------

## adjusting the TFR
Fx <- dta.swe$Fx
tfr.curr <- 5*sum(Fx)  ## current TFR
TFR <- seq(tfr.curr,1.5,length.out = n)
plot(TFR)

## create a matrix of TFR over prejtion periods
FX <- matrix(c(Fx),nrow=m,ncol=n)

i <- 1
for (i in 1:n){
  FX[,i] <- TFR[i] * (FX[,i]/tfr.curr)
}

5*colSums(FX)

matplot(x,FX,col=viridis(n),t="l",lty = 1)
lines(x,FX[,1],lwd=3,col=1)
lines(x,FX[,n],lwd=3,col=4)


i <- 10
srb=1.05
L0 <- dta.swe$LFx[1]
l0=1e5
srb.fact <- 1/(1+srb)
suv.fact <- L0/(2*l0)
sx <- dta.swe$sFx
bFx <- srb.fact*suv.fact*(FX[-m,i] + sx[-m]*FX[-1,i])
length(bFx)


## function to project several periods with TFR assumption
pop.proj.TFR <- function(x,AgeGroup,Nx,sx,Fx,srb=1.05,L0,l0=1e5,n){
  ## number of age groups
  m <- length(x)
  ## other adjustments
  srb.fact <- 1/(1+srb)
  suv.fact <- L0/(2*l0)
  ## create Leslie matrix
  L <- matrix(0,m,m)
  diag(L[-1,]) <- sx
  L[m,m] <- sx[m-1]
  ## create population matrix
  N <- matrix(0,m,n+1)
  N[,1] <- Nx
  for (i in 1:n){
    ## at each step, update our bx and Leslie matrix
    bFx <- srb.fact*suv.fact*(FX[-m,i] + sx[-m]*FX[-1,i])
    L[1,] <- c(bFx,0)
    N[,i+1] <- L%*%N[,i]
  }
  out <- cbind(data.frame(x=x,AgeGroup=AgeGroup),N)
  return(out)
}

my.proj.TFR <- pop.proj.TFR(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,
                             Nx=NFx,sx=sFx,Fx=FX,L0=L0,n=n)

## long data format
dta.swe.l <- my.proj.base %>%
  pivot_longer(-c(x,AgeGroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year = 1993 + (period-1)*5,
         YearF=as.factor(Year),
         type="baseline")
dta.swe.l.tfr <- my.proj.TFR %>%
  pivot_longer(-c(x,AgeGroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year = 1993 + (period-1)*5,
         YearF=as.factor(Year),
         type="TFR assumption")

## combine the long data
dta.swe.all <- dta.swe.l %>% 
  bind_rows(dta.swe.l.tfr)

## plotting with pyramid
ggplot(dta.swe.all,aes(x=AgeGroup,y=population,fill=type)) +
  geom_bar(data = subset(dta.swe.all, period == 21),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population 2093") +
  scale_fill_manual(name = 'Assumption', values=c("#E69F00", "#56B4E9"))


## bind the two long dataset together
## with the bind_rows function from tidyverse
dta.swe.all <- dta.swe.l %>% 
  bind_rows(dta.swe.l.TFR)

## SHINY APP for dynamic visualization of your results
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "year", label = "Year", step = 5,
              value = min(dta.swe.l$Year), min = min(dta.swe.l$Year), max = max(dta.swe.l$Year)),
  column(12, plotOutput("plot_pyr1"))
)
server <- function(input, output){
  output$plot_pyr1 <- renderPlot({
    ## plotting pyramid
    ggplot(dta.swe.all,aes(x=AgeGroup,y=population,fill=type)) +
      geom_bar(data = subset(dta.swe.all, Year == input$year),
               stat = "identity",position = "dodge",color = "black") +
      coord_flip() +
      theme_bw() +
      ggtitle(paste("Swedish female population, year",subset(dta.swe.all, Year == input$year)$Year)) +
      scale_fill_manual(name = 'Projection', values=c("#E69F00", "#56B4E9","#1C7C54")) +
      scale_y_continuous(limits = c(0, 350000), breaks = seq(0, 350000, 100000))
  })
}
shinyApp(ui = ui, server = server)



##----- EXERCISE 4: stable population and Leslie matrix --------

rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)
library(viridis)
library(migest)

## set working directory in active folder (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## loading the data 
dir()
load("EDSD.lecture3.Rdata")

## longer time horizon for projections
n <- 50
my.proj <- pop.proj.v2(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,NFx=NFx,sFx=sFx,bFx=bFx,
                       NMx=NMx,sMx=sMx,bMx=bMx,n=n)
## long data
dta.swe.l <- my.proj %>%
  pivot_longer(-c(x,AgeGroup,sex),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year))

## compute total population by sex in each year
tot.pop <- dta.swe.l %>%
  group_by(YearF) %>%
  summarise(TotPop=sum(population))

## compute distribution of the population in each age group
dta.swe.l <- dta.swe.l %>%
  left_join(tot.pop) %>%
  mutate(Pch=population/TotPop)

## extract distribution in last year
x <- dta.swe$Age
xG <- dta.swe$AgeGroup
yF <- dta.swe.l$Pch[dta.swe.l$Year == max(dta.swe.l$Year) & dta.swe.l$sex == "Females"]
yM <- dta.swe.l$Pch[dta.swe.l$Year == max(dta.swe.l$Year) & dta.swe.l$sex == "Males"]

plot(x,yF)

## eigendecomposition of the Leslie matrix
ev.decomp <- eigen(L)

## extract largest eigenvalue
## this will give you the long-term CGR of the stable population
ev.val <- abs(ev.decomp$values)[1]
cgr_leslie <- log(ev.val)

## extract corresponding eigenvectors
ev.vec <- ev.decomp$vectors[,1]
ev.vecF <- ev.vec[1:m]
ev.vecM <- ev.vec[1:m + m]

## rescale them to sum to the long-term sex-specific total distribution
## this will give you the long-term distribution of the stable population
ev.vecF <- (ev.vecF/sum(ev.vecF)) * sum(yF)
ev.vecM <- (ev.vecM/sum(ev.vecM)) * sum(yM)

## compare Male distribution of projection vs Leslie
plot(x,yM,t="n",axes = F,xlab = "age group",ylab = "",
     ylim = range(yM,yF),main="Males",cex.main=1.5)
axis(1)
axis(2);grid();box()
mtext("Pop distribution",side=2,cex=1.5,line=2.4,las=3)
points(x,yM,lwd=2)
points(x,ev.vecM,col=4,pch=4,lwd=2)
legend("bottomleft",c("From projections","From Leslie"),pch=c(1,4),col=c(1,4),
       lwd=2,cex=1.25,inset = 0.01,lty=NA,bg="white")



## extract overall total population
tot.pop <- tot.pop %>%
  pull(TotPop)

## compute growth rate
CGR <- rep(NA,n)
for (i in 1:n){
  CGR[i] <- log(tot.pop[i+1]/tot.pop[i])
}

## plotting crude growth rate
plot(1:n,CGR,col=4,t="l",lwd=2)
abline(h=cgr_leslie,col=2,lty=2,lwd=2)
legend("topright",c("From projections","From Leslie"),pch=NA,col=c(4,2),
       lwd=2,cex=1.25,lty=c(1,2),bg="white")


## END