## -------------------------------------------- ##
## EDSD 2020-2021: Population projections
## Lecture 3
## Matrix Projections
## Date: 09/06/2020
## Instructor: Ugofilippo Basellini
## -------------------------------------------- ##


##----- MATRIX PROJECTION - FEMALES --------
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)
library(viridis)

## set working directory in active folder (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## loading the data (derived from Preston et al. 2001)
dir()
load("EDSD.lecture2.Rdata")  

## extract sFx, NFx, bFx
sFx <- dta.swe$sFx
bFx <- dta.swe$bFx
NFx <- dta.swe$NFx

## adjust bFx
bFx[is.na(bFx)] <- 0

## remove last NA from sFx
sFx <- sFx[!is.na(sFx)]

## create our Leslie matrix
m <- nrow(dta.swe)   ## number of age groups
L <- matrix(0,m,m)

## assign these two vectors to L
L[1,] <- bFx 
diag(L[-1,]) <- sFx
L[m,m] <- sFx[m-1]

## compute the projection
NFx5.matrix <- c(L%*%NFx)
NFx5.manual <- dta.swe$NFx5
all.equal(NFx5.matrix,NFx5.manual)

## create population matrix
n <- 20    ## projection period
N <- matrix(0,m,n+1)
N[,1] <- NFx

## for loop to update the population projection
i <- 1
for (i in 1:n){
  N[,i+1] <- L%*%N[,i]
}

## checking that things work as expected
N[,2]
NFx5.matrix

## let's do the same, but with a function instead
## function to project several periods
pop.proj <- function(x,AgeGroup,Nx,sx,bx,n){
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
    N[,i+1] <- L%*%N[,i]
  }
  out <- cbind(data.frame(x=x,AgeGroup=AgeGroup),N)
  return(out)
}
## actual projection
my.proj <- pop.proj(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,
                    Nx=NFx,sx=sFx,bx=bFx,n=n)
all.equal(my.proj$"1",NFx)
all.equal(my.proj$"2",NFx5.manual)

## long data
dta.swe.l <- my.proj %>%
  pivot_longer(-c(x,AgeGroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year = 1993 + (period-1)*5,
         YearF=as.factor(Year))

## plotting with pyramid
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
  geom_bar(data = subset(dta.swe.l, period %in% c(1,2,20)),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population") +
  scale_fill_manual(name = 'Year', values=c("#E69F00", "#56B4E9","#1C7C54"))


##----- ANIMATION INTERMEZZO -------

## start by creating a multiple layer pdf

plots <- list()
my.cols <- cividis(n+1)
my.years <- unique(dta.swe.l$Year)
i <- 1
for (i in 1:(n+1)){
  gg <- ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
    geom_bar(data = subset(dta.swe.l, period == i),
             stat = "identity",color = "black") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(paste("Swedish female population, year",my.years[i])) +
    scale_fill_manual(values=my.cols[i])
  plots[[i]] <- gg
}
## saving plots in a single file
pdf("myAnimFig.pdf")
invisible(lapply(plots, print))
dev.off()


## gganimate
library(gganimate)
library(gifski)
gg <- ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
  geom_bar(stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option="cividis")
gg + transition_states(YearF) +
  ggtitle('Swedish female population, year {closest_state}')
anim_save("F2.gif")


## do with magick package
library(magick)

## create a directory to which the images will be written
dir_out <- file.path(tempdir(), "temp_dir")
dir.create(dir_out, recursive = TRUE)

## loop through years and write plot to file
for (i in 1:(n+1)){
  gg <- ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
    geom_bar(data = subset(dta.swe.l, period == i),
             stat = "identity",color = "black") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(paste("Swedish female population, year",my.years[i])) +
    scale_fill_manual(values=my.cols[i])
  fp <- file.path(dir_out, paste0(i, ".png"))
  ggsave(plot = gg, 
         filename = fp, 
         device = "png")
}


## list file names and read in
imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)
## join the images together
img_joined <- image_join(img_list)
## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)
## view animated image
img_animated
## save to your pc
image_write(image = img_animated,
            path = "example.gif")

library("shiny")

## general parameters
n1 <- max(dta.swe.l$period)
my.cols <- cividis(n1)
my.years <- unique(dta.swe.l$Year)
## build your user interfact
ui <- fluidPage(
  ## title of your shiny
  titlePanel('My first shiny app'),
  ## display a slider that returns input$year to pass to the server function
  sliderInput(inputId = "year", label = "Year", step = 5,
              value = min(my.years), min = min(my.years), max = max(my.years)),
  ## display a plot returned from the server
  plotOutput("plot_pyr1")
  # plotlyOutput("plot_pyr1")
)

## build your server
server <- function(input, output){
  ## create an output that renders a plot
  output$plot_pyr1 <- renderPlot({
  # output$plot_pyr1 <- renderPlotly({  
    ## any ggplot or plot,
    ## here subsetting the year of the given input$year
    ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
      geom_bar(data = subset(dta.swe.l, Year == input$year),
               stat = "identity",color = "black") +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste("Swedish female population, year",input$year)) +
      scale_fill_manual(values=my.cols[which(input$year==my.years)])
    # ggplotly(gg,tooltip = c('AgeGroup','population'))
  })
}
## run the shiny app, which puts together the ui and server
shinyApp(ui = ui, server = server)


## ADDING INTERACTIVITY
library("plotly")

## build your user interfact
ui <- fluidPage(
  ## title of your shiny
  titlePanel('My first shiny app'),
  ## display a slider that returns input$year to pass to the server function
  sliderInput(inputId = "year", label = "Year", step = 5,
              value = min(my.years), min = min(my.years), max = max(my.years)),
  ## display a plot returned from the server
  plotlyOutput("plot_pyr1")
)

## build your server
server <- function(input, output){
  ## create an output that renders a plot
  # output$plot_pyr1 <- renderPlot({
  output$plot_pyr1 <- renderPlotly({
    ## any ggplot or plot,
    ## here subsetting the year of the given input$year
    ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
      geom_bar(data = subset(dta.swe.l, Year == input$year),
               stat = "identity",color = "black") +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste("Swedish female population, year",input$year)) +
      scale_fill_manual(values=my.cols[which(input$year==my.years)])
    # ggplotly(gg,tooltip = c('AgeGroup','population'))
  })
}
## run the shiny app, which puts together the ui and server
shinyApp(ui = ui, server = server)


##----- MATRIX PROJECTION - MALES --------


## extract sMx, NMx, bMx
sMx <- dta.swe$sMx
bMx <- dta.swe$bMx
NMx <- dta.swe$NMx

## adjust bMx
bMx[is.na(bMx)] <- 0

## remove last NA from sFx
sMx <- sMx[!is.na(sMx)]

## female Leslie matrix
LF <- L

## male Leslie matrix
BM <- LM <- matrix(0,m,m)
BM[1,] <- bMx

diag(LM[-1,]) <- sMx
LM[m,m] <- sMx[m-1]

## put together females and males
ZEROS <- diag(0,m)
Lup <- cbind(LF,ZEROS)
Ldown <- cbind(BM,LM)
L <- rbind(Lup,Ldown)

## making the projection
Nx <- c(NFx,NMx)
Nx5.matrix <- c(L%*%Nx)

NFx5.manual <- dta.swe$NFx5
NMx5.manual <- dta.swe$NMx5
all.equal(NFx5.manual,Nx5.matrix[1:m]) 
all.equal(NMx5.manual,Nx5.matrix[1:m+m]) 

## let's do this with a function
## function to project several periods
pop.proj.v2 <- function(x,AgeGroup,NFx,sFx,bFx,NMx,sMx,bMx,n){
  ## number of age groups
  m <- length(x); m2 <- m*2
  ## female Leslie matrix
  LF <- matrix(0,m,m)
  LF[1,] <- bFx
  diag(LF[-1,]) <- sFx
  LF[m,m] <- sFx[m-1]
  ## male Leslie matrix
  BM <- LM <- matrix(0,m,m)
  BM[1,] <- bMx
  diag(LM[-1,]) <- sMx
  LM[m,m] <- sMx[m-1]
  ## putting them together
  ZEROS <- diag(0,m)
  Lup <- cbind(LF,ZEROS)
  Ldown <- cbind(BM,LM)
  L <- rbind(Lup,Ldown)
  ## create population matrix
  N <- matrix(0,m2,n+1)
  N[,1] <- c(NFx,NMx)
  for (i in 1:n){
    N[,i+1] <- L%*%N[,i]
  }
  out <- cbind(data.frame(x=rep(x,2),AgeGroup=rep(AgeGroup,2),
                          sex=rep(c("Females","Males"),each=m)),N)
  return(out)
}


my.proj <- pop.proj.v2(x=dta.swe$Age,AgeGroup=dta.swe$AgeGroup,NFx=NFx,sFx=sFx,bFx=bFx,
                       NMx=NMx,sMx=sMx,bMx=bMx,n=20)
## long data
dta.swe.l <- my.proj %>%
  pivot_longer(-c(x,AgeGroup,sex),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year))
## plotting
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=YearF)) +
  geom_bar(data = subset(dta.swe.l, period %in% c(1,2,21) & sex == "Males"),
           stat = "identity",position = "dodge",color = "black",mapping = aes(y = -population)) +
  geom_bar(data = subset(dta.swe.l, period %in% c(1,2,21) & sex == "Females"),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish population") +
  scale_y_continuous(limits=c(-3.5e5,3.5e5),
                     breaks = seq(-4e5,4e5,1e5),
                     labels = abs(seq(-4e5,4e5,1e5))) +
  scale_fill_brewer(name="Year",palette = 'Blues', direction = -1) +
  geom_text(data = subset(dta.swe.l, period %in% c(1)),
            aes(y = max(population)/1.25, x = 17, label='Females'),size=7) +
  geom_text(data = subset(dta.swe.l, period %in% c(1)),
            aes(y = -max(population)/1.25, x = 17, label='Males'),size=7)


## saving the data for tomorrow's lecture
save.image("EDSD.lecture3.Rdata")

## END