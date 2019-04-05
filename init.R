#workspace cleaning
remove(list = ls())
cat("\014")

oldw <- getOption("warn")
options(warn = -1)

#package loading
library(dplyr)
library(stringr)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(grid)
library(gridExtra)
library(plotwidgets)
library(circlize)
library(jpeg)
library(scales)

source(file.path(.session$homedir,"src/colors.R"), echo = FALSE)
source(file.path(.session$homedir,"src/parsing.R"), echo = FALSE)

options(warn = oldw)
rm(oldw)