##This should detect and install missing packages before loading them - hopefully!
# 
# list.of.packages <- c("shiny","data.table","DT","ggplot2")
# lapply(list.of.packages,function(x){library(x,character.only=TRUE)})

require("shiny")
require("data.table")
require("DT")
require("ggplot2")

#  
# 
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 


