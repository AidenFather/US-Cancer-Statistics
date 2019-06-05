library(dplyr)
library(tidyverse)
library(DT)
library(shinydashboard)
library(shiny)
library(ggplot2)
library(gganimate)
library(plotly)
library(colourpicker)
library(viridis)
library(RColorBrewer)
library(shinyWidgets)
library(devtools)
library(animation)
library(cowplot)
library(magick)
library(praise)
library(testthat)

theme_set(theme_classic())

### read perState data ###
state.df1999 = read.csv(file = "./state1.df1999.csv")

state.df1999 = state.df1999 %>% 
  mutate(., rate1 = crude_rate, rate2 = count/population*100000)


### read perAge data ###
age.df = read.csv(file = "./age.df.csv") ## 1999 - 2015 ##

age.df$age = factor(age.df$age, levels =c("<1","1-4", "5-9","10-14","15-19","20-24","25-29","30-34",
                             "35-39","40-44","45-49","50-54", "55-59", "60-64", "65-69", 
                             "70-74", "75-79", "80-84", "85+"))



##create a variable that lists cancer type
cancer.type = sort(unique(state.df1999$site))

##create a sex varilable
cancer.sex = sort(unique(state.df1999$sex))

##create a variable that lists event_type 
cancer.event_type = sort(unique(state.df1999$event_type))

##create a variable that list states
cancer.area = sort(unique(state.df1999$area1))




