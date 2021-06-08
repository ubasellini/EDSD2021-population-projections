## -------------------------------------------- ##
## EDSD 2020-2021: Population projections
## Lecture 2
## The cohort component method
## Date: 08/06/2020
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##


##----- EXERCISE 1 --------
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)

## set working directory in active folder (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## loading the data (derived from Preston et al. 2001)
dir()
load("dta.swe.1993.Rdata")  

## explore the data
head(dta.swe)

## project the age groups forward
dta.swe <- as_tibble(dta.swe) %>% 
  mutate(sFx = lead(LFx) / LFx, 
         NFx5 = lag(NFx*sFx))

## checking the results
head(dta.swe[,c(1:4,8,9)])

## example of lead function
dta.swe$sFx[1]
dta.swe$LFx[2] / dta.swe$LFx[1] 

## example of lag function
dta.swe$NFx5[2]
dta.swe$NFx[1] * dta.swe$sFx[1]

## adjusting the last age group
dta.swe <- dta.swe %>% 
  mutate(sFx = ifelse(test = Age == 80,
                      yes  =  lead(LFx)/(LFx + lead(LFx)),
                      no   = sFx),
         NFx5 = ifelse(test = Age == 85,
                       yes = (NFx+lag(NFx))*lag(sFx),
                       no  = NFx5 ))

## checking the results
tail(dta.swe[,c(1:4,8,9)])


## some inputs fdr adjusting the first age group
srb <- 1.05
fact.srb <- 1/(1+srb)
l0 <- 1e5
LF0 <- dta.swe$LFx[1]

## adjust the first age group
dta.swe <- dta.swe %>% 
  mutate(bFx = fact.srb * LF0 / (2*l0) * (Fx + sFx * lead(Fx)),
         Bx = Fx * 5 * (NFx + NFx5)/2,
         NFx5 = ifelse(test = Age == 0,
                       yes = fact.srb*LF0/(5*l0)*sum(Bx,na.rm = T),
                       no  = NFx5)
  )

dta.swe$NFx5[1]
sum(dta.swe$NFx * dta.swe$bFx,na.rm = T)

## first example of pyramid plotting

## long data format
dta.swe.l <- dta.swe %>% 
  select(AgeGroup,NFx,NFx5) %>% 
  rename("1993"=NFx,"1998"=NFx5) %>% 
  # pivot_longer(-AgeGroup)
  pivot_longer(-AgeGroup,names_to="year",values_to="population")

## plot a pyramid
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=year)) +
  geom_bar(stat="identity",position="dodge",color="black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))


## CASE of MALE population

## some inputs fdr adjusting the first age group
fact.srb.M <- srb/(1+srb)
LM0 <- dta.swe$LMx[1]

## projecting the male population
dta.swe <- dta.swe %>%
  mutate(sMx=lead(LMx)/LMx,
         NMx5=lag(NMx*sMx),
         sMx=ifelse(test = Age==80,
                    yes = lead(LMx)/(LMx + lead(LMx)),
                    no = sMx),
         NMx5=ifelse(test = Age==85,
                     yes = (NMx+lag(NMx))*lag(sMx),
                     no = NMx5),
         bMx=fact.srb.M * LM0 / (2*l0) * (Fx + sFx*lead(Fx)),
         NMx5=ifelse(test = Age==0,
                     yes = sum(bMx*NFx,na.rm = T),
                     no = NMx5))

## checking the results
head(dta.swe[,c(1:3,6,9,13)],n=3)

## saving the data for tomorrow's lecture
save.image("EDSD.lecture2.Rdata")

## END