#R for ID Modeling
#Yang Liu
#August 2019, Shanghai

####Section 1: Fundamentals of R####
# object levels
# slide 11
lvl3_1 <- 2 #double/ integer
lvl3_2 <- "Shanghai" #character/ text
lvl3_3 <- TRUE #logical
lvl3_4 <- factor(c("animal")) #factor
lvl3_5 <- as.Date("2019-07-30") #date

lvl2_1 <- c(1:10) #vector, 1D
lvl2_2 <- matrix(lvl2_1, ncol = 2) #matrix, 2D
lvl2_3 <- data.frame(lvl2_2) #dataframe, 2D

lvl1_1 <- list(lvl3_1,
               lvl3_2,
               lvl3_3,
               lvl3_4,
               lvl3_5)#list

print(lvl3_1)
class(lvl1_1)
lvl2_1[1]
lvl2_2[1,2]

# subsetting
# slide 13
lvl2_3
lvl2_3[1,2]
lvl2_3[which(lvl2_3$X1>3),]
lvl2_3[1,]
lvl2_3[,"X2"]

# defining your own function
# slide 14
circle_area <- function(r){
  area_tmp <- pi * r * r
  return(area_tmp)
}
circle_area(2)

# working with packages
# slide 15
# install.packages("EpiDynamics")
library("EpiDynamics")
require("EpiDynamics")
EpiDynamics::SIR
# advance options
# install.packages("pacman")
# pacman::p_load("sf","tidyverse","devtools","xlsx")
#Sys.setenv("TAR" = "internal") #only use this if you see the error message saying the files do not appear to be a pacakge
# devtools::install_github("njtierney/mmcc")#this is a package for re-organizing results from mcmc

# finding the right environment
# slide 16
getwd()
list.files()
#setwd("~/SOMWHERE ELSE")

# expressions in R
# slide 17
lvl3_1 == 2
lvl3_1 == 3
objects()

#####Section 2: R in your work flow####
#####Section 2.1: Import and Export####
# text files
# slide 25
lvl2_3
write.table(lvl2_3, file = "lvl2_3.txt")
read.table("lvl2_3.txt")

# xlsx files
# slide 26-27
#install.packages("xlsx")
#install.packages("readxl")
library(readxl)
library(xlsx)
library(writexl)

download.file(url = "https://github.com/yangclaraliu/ShanghaiIDWorkshop2019/raw/master/Lecture 2 - R4ID/X3.xlsx", destfile = "X3.xlsx", mode = "wb")
lvl2_3["X3"] <- read_excel("X3.xlsx", col_names = F)
#Sys.setlocale("LC_ALL","Chinese")
Encoding(lvl2_3$X3)
lvl2_3
xlsx::write.xlsx(lvl2_3, file = "lvl2_3.xlsx")
writexl::write_xlsx(lvl2_3, path = "lvl2_3.xlsx")

# shape files
# slide 28
#install.packages("rgdal")
library(rgdal)
chn <- readOGR("C:/Users/eideyliu/Downloads/2014Shapefile_ChinaCDC_20140325/shp","sheng")
plot(chn)

# advance options
# sf is a newer class of geospatial objects. Unlike 
# install.packages("sf")
# library(sf)
# t1 <- Sys.time(); chn;Sys.time()-t1 #on my computer, it takes 15 secs to just open this object in the console window
# chn <- st_as_sf(chn)
# t1 <- Sys.time(); chn;Sys.time()-t1 #voila!On my computer, this takes about 0.3 secs

# install.packages("mapview")
# library(mapview)
# mapview(chn, zcol = "AREA")

#####Section 2.2: Tidy (and Transform)####
# class conversion
# slide 34
as.character(12)
as.numeric("Apple")
as.logical(12)
as.logical(1)
as.logical(0)
as.logical(-1)
as.numeric(TRUE)
as.numeric(FALSE)
as.matrix(lvl2_1)
as.vector(lvl2_2)

# IF statement
# slide 35
fruits <- lvl2_3$X3[c(1,2,4)]
is_fruit <- function(type){
  if(type %in% fruits){
    return(T)
    } else {return(F)}
  }
#Try this!
#is_fruit()

# Loops
# slide 36-37
# for
lvl2_3
for(i in c(2,4)) lvl2_3[i,] <- lvl2_3[i,1:2] * 10
lvl2_3

# repeat
tmp <- 0
repeat{
  tmp <- tmp+1
  print(tmp)
  if(tmp > 8) {break}
}

# while
tmp <- 0
while(tmp < 7){
  tmp <- tmp+1
  print(tmp)
}

# Tidyverse
# install.packages("tidyverse")
# slide 39
library(tidyverse)

#We should already called the package called "EpiDynamics" from the code above. But just in case, if R tells you it can't find the package named "EpiDynamics", please run the following two lines
#install.packages("EpiDynamics")
library(EpiDynamics)

#let's make a toy model of an imaginary disease Birdy Pox.
#beta is the transmission rate; gamma is the recovery rate
# slide 41-43
parameters <- c(beta = 2, gamma = 0.15)
initials <- c(S = 1 - 1e-06, I = 1e-06, R = 0)
res <- SIR(pars = parameters, 
           init = initials, 
           time = 0:70)$results
head(res)

# slide 44
select(res, time, I)

# slide 45
mutate(res, time_wk = time/7)

# slide 46
dplyr::filter(res, I > 0.2)
 
# slide 47
dplyr::filter(res, time%%7 == 0)

# slide 50
tmp <- gather(res, key = key, value = value, -time)
tmp

# slide 51
spread(tmp, key = key, value = value)

# slide 52
class(tmp$key)
tmp$key <- factor(tmp$key, levels = c("S","I","R"))
tmp <- tmp[order(tmp$key),]
class(tmp$key)
spread(tmp, key = key, value = value)

# slide 54
holiday <- data.frame(time = c(4,23),
                      holiday = 1)
tmp <- full_join(res, holiday, by = "time")
tmp[is.na(tmp$holiday),"holiday"] <- 0
head(tmp)

# slide 56
# advance option: pipes
# tmp <- dplyr::filter(res, time%%7 == 0)
# tmp <- gather(tmp, key = state, value = proportion, -time)
# tmp
# 
# res %>% 
#   filter(., time%%7 == 0) %>% 
#   gather(., key = state, value = proportion, -time) -> tmp

#####Section 2.3: Visualization####
# base R plots
# slides 60-61
par(mfrow=c(2,2))
#line plot
plot(x = res$time, y = res$I, type = "l", xlab = "Time", ylab = "I", main = "Time-series of I")
#bar plot
barplot(res$R, xlab = "Time", ylab = "R", main = "Time-series of R")
#scatter plot
plot(x = res$S, y = res$I, xlab = "S", ylab = "I", main = "Relationship between S and I", pch = 19)
#histogram + density line
hist(res$I, breaks = 10, freq = F, ylim=c(0, 11), xlab = "I", main = "Histogram of I")
lines(density(res$I), col = "red")
dev.off()

# save
# slide 62
png("sample.png")
plot(x = res$time, 
     y = res$I, 
     type = "l", 
     xlab = "Time", 
     ylab = "I", 
     main = "Time-series of I")
dev.off()

# ggplot
# slides 64-69
tmp <- gather(res, 
              key = state, 
              value = proportion, -time)

tmp <- mutate(tmp, 
              state = factor(state, 
                             levels = c("S","I","R"),
                             labels = c("Susceptible","Infectious","Recovered")))
  
ggplot(tmp, aes(x = time, 
                y = proportion, 
                group = state, 
                color = state)) +
  geom_line()+
  labs(x = "Time", y = "Proportion")  +
  theme_bw() +
  facet_grid(rows = vars(state))+
  theme(legend.position = "bottom")#,
#        text = element_text(family = "Garamond", size = 14))

ggsave("sample.png")

#####Section 3: Finding Answers/ Slef-learning####
# slide 73
?SIR

# slide 74
vignette("broom")

# slide 75
EpiDynamics::SIR
